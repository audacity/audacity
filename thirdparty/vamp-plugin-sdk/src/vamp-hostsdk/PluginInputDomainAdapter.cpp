/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2009 Chris Cannam and QMUL.
  
    This file is based in part on Don Cross's public domain FFT
    implementation.

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR
    ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    Except as contained in this notice, the names of the Centre for
    Digital Music; Queen Mary, University of London; and Chris Cannam
    shall not be used in advertising or otherwise to promote the sale,
    use or other dealings in this Software without prior written
    authorization.
*/

#include <vamp-hostsdk/PluginInputDomainAdapter.h>

#include <cmath>

#include "Window.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <limits.h>

_VAMP_SDK_HOSTSPACE_BEGIN(PluginInputDomainAdapter.cpp)

#include "../vamp-sdk/FFTimpl.cpp"

namespace Vamp {

namespace HostExt {

class PluginInputDomainAdapter::Impl
{
public:
    Impl(Plugin *plugin, float inputSampleRate);
    ~Impl();
    
    bool initialise(size_t channels, size_t stepSize, size_t blockSize);
    void reset();

    size_t getPreferredStepSize() const;
    size_t getPreferredBlockSize() const;

    FeatureSet process(const float *const *inputBuffers, RealTime timestamp);

    void setProcessTimestampMethod(ProcessTimestampMethod m);
    ProcessTimestampMethod getProcessTimestampMethod() const;
    
    RealTime getTimestampAdjustment() const;

    WindowType getWindowType() const;
    void setWindowType(WindowType type);

protected:
    Plugin *m_plugin;
    float m_inputSampleRate;
    int m_channels;
    int m_stepSize;
    int m_blockSize;
    float **m_freqbuf;
    Kiss::vamp_kiss_fft_scalar *m_ri;

    WindowType m_windowType;
    typedef Window<Kiss::vamp_kiss_fft_scalar> W;
    W *m_window;

    ProcessTimestampMethod m_method;
    int m_processCount;
    float **m_shiftBuffers;

    Kiss::vamp_kiss_fftr_cfg m_cfg;
    Kiss::vamp_kiss_fft_cpx *m_cbuf;

    FeatureSet processShiftingTimestamp(const float *const *inputBuffers, RealTime timestamp);
    FeatureSet processShiftingData(const float *const *inputBuffers, RealTime timestamp);

    size_t makeBlockSizeAcceptable(size_t) const;
    
    W::WindowType convertType(WindowType t) const;
};

PluginInputDomainAdapter::PluginInputDomainAdapter(Plugin *plugin) :
    PluginWrapper(plugin)
{
    m_impl = new Impl(plugin, m_inputSampleRate);
}

PluginInputDomainAdapter::~PluginInputDomainAdapter()
{
    delete m_impl;
}
  
bool
PluginInputDomainAdapter::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    return m_impl->initialise(channels, stepSize, blockSize);
}

void
PluginInputDomainAdapter::reset()
{
    m_impl->reset();
}

Plugin::InputDomain
PluginInputDomainAdapter::getInputDomain() const
{
    return TimeDomain;
}

size_t
PluginInputDomainAdapter::getPreferredStepSize() const
{
    return m_impl->getPreferredStepSize();
}

size_t
PluginInputDomainAdapter::getPreferredBlockSize() const
{
    return m_impl->getPreferredBlockSize();
}

Plugin::FeatureSet
PluginInputDomainAdapter::process(const float *const *inputBuffers, RealTime timestamp)
{
    return m_impl->process(inputBuffers, timestamp);
}

void
PluginInputDomainAdapter::setProcessTimestampMethod(ProcessTimestampMethod m)
{
    m_impl->setProcessTimestampMethod(m);
}

PluginInputDomainAdapter::ProcessTimestampMethod
PluginInputDomainAdapter::getProcessTimestampMethod() const
{
    return m_impl->getProcessTimestampMethod();
}

RealTime
PluginInputDomainAdapter::getTimestampAdjustment() const
{
    return m_impl->getTimestampAdjustment();
}

PluginInputDomainAdapter::WindowType
PluginInputDomainAdapter::getWindowType() const
{
    return m_impl->getWindowType();
}

void
PluginInputDomainAdapter::setWindowType(WindowType w)
{
    m_impl->setWindowType(w);
}


PluginInputDomainAdapter::Impl::Impl(Plugin *plugin, float inputSampleRate) :
    m_plugin(plugin),
    m_inputSampleRate(inputSampleRate),
    m_channels(0),
    m_stepSize(0),
    m_blockSize(0),
    m_freqbuf(0),
    m_ri(0),
    m_windowType(HanningWindow),
    m_window(0),
    m_method(ShiftTimestamp),
    m_processCount(0),
    m_shiftBuffers(0),
    m_cfg(0),
    m_cbuf(0)
{
}

PluginInputDomainAdapter::Impl::~Impl()
{
    // the adapter will delete the plugin

    if (m_shiftBuffers) {
        for (int c = 0; c < m_channels; ++c) {
            delete[] m_shiftBuffers[c];
        }
        delete[] m_shiftBuffers;
    }

    if (m_channels > 0) {
        for (int c = 0; c < m_channels; ++c) {
            delete[] m_freqbuf[c];
        }
        delete[] m_freqbuf;
        delete[] m_ri;
        if (m_cfg) {
            Kiss::vamp_kiss_fftr_free(m_cfg);
            m_cfg = 0;
            delete[] m_cbuf;
            m_cbuf = 0;
        }
        delete m_window;
    }
}

// for some visual studii apparently
#ifndef M_PI
#define M_PI 3.14159265358979232846
#endif
    
bool
PluginInputDomainAdapter::Impl::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (m_plugin->getInputDomain() == TimeDomain) {

        m_stepSize = int(stepSize);
        m_blockSize = int(blockSize);
        m_channels = int(channels);

        return m_plugin->initialise(channels, stepSize, blockSize);
    }

    if (blockSize < 2) {
        std::cerr << "ERROR: PluginInputDomainAdapter::initialise: blocksize < 2 not supported" << std::endl;
        return false;
    }                
        
    if (blockSize % 2) {
        std::cerr << "ERROR: PluginInputDomainAdapter::initialise: odd blocksize " << blockSize << " not supported" << std::endl;
        return false;
    }

    if (m_channels > 0) {
        for (int c = 0; c < m_channels; ++c) {
            delete[] m_freqbuf[c];
        }
        delete[] m_freqbuf;
        delete[] m_ri;
        if (m_cfg) {
            Kiss::vamp_kiss_fftr_free(m_cfg);
            m_cfg = 0;
            delete[] m_cbuf;
            m_cbuf = 0;
        }
        delete m_window;
    }

    m_stepSize = int(stepSize);
    m_blockSize = int(blockSize);
    m_channels = int(channels);

    m_freqbuf = new float *[m_channels];
    for (int c = 0; c < m_channels; ++c) {
        m_freqbuf[c] = new float[m_blockSize + 2];
    }
    m_ri = new Kiss::vamp_kiss_fft_scalar[m_blockSize];

    m_window = new W(convertType(m_windowType), m_blockSize);

    m_cfg = Kiss::vamp_kiss_fftr_alloc(m_blockSize, false, 0, 0);
    m_cbuf = new Kiss::vamp_kiss_fft_cpx[m_blockSize/2+1];

    m_processCount = 0;

    return m_plugin->initialise(channels, stepSize, m_blockSize);
}

void
PluginInputDomainAdapter::Impl::reset()
{
    m_processCount = 0;
    m_plugin->reset();
}

size_t
PluginInputDomainAdapter::Impl::getPreferredStepSize() const
{
    size_t step = m_plugin->getPreferredStepSize();

    if (step == 0 && (m_plugin->getInputDomain() == FrequencyDomain)) {
        step = getPreferredBlockSize() / 2;
    }

    return step;
}

size_t
PluginInputDomainAdapter::Impl::getPreferredBlockSize() const
{
    size_t block = m_plugin->getPreferredBlockSize();

    if (m_plugin->getInputDomain() == FrequencyDomain) {
        if (block == 0) {
            block = 1024;
        } else {
            block = makeBlockSizeAcceptable(block);
        }
    }

    return block;
}

size_t
PluginInputDomainAdapter::Impl::makeBlockSizeAcceptable(size_t blockSize) const
{
    if (blockSize < 2) {

        std::cerr << "WARNING: PluginInputDomainAdapter::initialise: blocksize < 2 not" << std::endl
                  << "supported, increasing from " << blockSize << " to 2" << std::endl;
        blockSize = 2;

    } else if (blockSize % 2) {
        
        std::cerr << "WARNING: PluginInputDomainAdapter::initialise: odd blocksize not" << std::endl
                  << "supported, increasing from " << blockSize << " to " << (blockSize+1) << std::endl;
        blockSize = blockSize+1;
    }

    return blockSize;
}

RealTime
PluginInputDomainAdapter::Impl::getTimestampAdjustment() const
{
    if (m_plugin->getInputDomain() == TimeDomain) {
        return RealTime::zeroTime;
    } else if (m_method == ShiftData || m_method == NoShift) {
        return RealTime::zeroTime;
    } else {
        return RealTime::frame2RealTime
            (m_blockSize/2, int(m_inputSampleRate + 0.5));
    }
}

void
PluginInputDomainAdapter::Impl::setProcessTimestampMethod(ProcessTimestampMethod m)
{
    m_method = m;
}

PluginInputDomainAdapter::ProcessTimestampMethod
PluginInputDomainAdapter::Impl::getProcessTimestampMethod() const
{
    return m_method;
}

void
PluginInputDomainAdapter::Impl::setWindowType(WindowType t)
{
    if (m_windowType == t) return;
    m_windowType = t;
    if (m_window) {
        delete m_window;
        m_window = new W(convertType(m_windowType), m_blockSize);
    }
}

PluginInputDomainAdapter::WindowType
PluginInputDomainAdapter::Impl::getWindowType() const
{
    return m_windowType;
}

PluginInputDomainAdapter::Impl::W::WindowType
PluginInputDomainAdapter::Impl::convertType(WindowType t) const
{
    switch (t) {
    case RectangularWindow:
        return W::RectangularWindow;
    case BartlettWindow:
        return W::BartlettWindow;
    case HammingWindow:
        return W::HammingWindow;
    case HanningWindow:
        return W::HanningWindow;
    case BlackmanWindow:
        return W::BlackmanWindow;
    case NuttallWindow:
        return W::NuttallWindow;
    case BlackmanHarrisWindow:
        return W::BlackmanHarrisWindow;
    default:
	return W::HanningWindow;
    }
}

Plugin::FeatureSet
PluginInputDomainAdapter::Impl::process(const float *const *inputBuffers,
                                        RealTime timestamp)
{
    if (m_plugin->getInputDomain() == TimeDomain) {
        return m_plugin->process(inputBuffers, timestamp);
    }

    if (m_method == ShiftTimestamp || m_method == NoShift) {
        return processShiftingTimestamp(inputBuffers, timestamp);
    } else {
        return processShiftingData(inputBuffers, timestamp);
    }
}

Plugin::FeatureSet
PluginInputDomainAdapter::Impl::processShiftingTimestamp(const float *const *inputBuffers,
                                                         RealTime timestamp)
{
    unsigned int roundedRate = 1;
    if (m_inputSampleRate > 0.f) {
        roundedRate = (unsigned int)round(m_inputSampleRate);
    }
    
    if (m_method == ShiftTimestamp) {
        // we may need to add one nsec if timestamp +
        // getTimestampAdjustment() rounds down
        timestamp = timestamp + getTimestampAdjustment();
        RealTime nsec(0, 1);
        if (RealTime::realTime2Frame(timestamp, roundedRate) <
            RealTime::realTime2Frame(timestamp + nsec, roundedRate)) {
            timestamp = timestamp + nsec;
        }
    }

    for (int c = 0; c < m_channels; ++c) {

        m_window->cut(inputBuffers[c], m_ri);

        for (int i = 0; i < m_blockSize/2; ++i) {
            // FFT shift
            Kiss::vamp_kiss_fft_scalar value = m_ri[i];
            m_ri[i] = m_ri[i + m_blockSize/2];
            m_ri[i + m_blockSize/2] = value;
        }

        Kiss::vamp_kiss_fftr(m_cfg, m_ri, m_cbuf);
        
        for (int i = 0; i <= m_blockSize/2; ++i) {
            m_freqbuf[c][i * 2] = float(m_cbuf[i].r);
            m_freqbuf[c][i * 2 + 1] = float(m_cbuf[i].i);
        }
    }

    return m_plugin->process(m_freqbuf, timestamp);
}

Plugin::FeatureSet
PluginInputDomainAdapter::Impl::processShiftingData(const float *const *inputBuffers,
                                                    RealTime timestamp)
{
    if (m_processCount == 0) {
        if (!m_shiftBuffers) {
            m_shiftBuffers = new float *[m_channels];
            for (int c = 0; c < m_channels; ++c) {
                m_shiftBuffers[c] = new float[m_blockSize + m_blockSize/2];
            }
        }
        for (int c = 0; c < m_channels; ++c) {
            for (int i = 0; i < m_blockSize + m_blockSize/2; ++i) {
                m_shiftBuffers[c][i] = 0.f;
            }
        }
    }

    for (int c = 0; c < m_channels; ++c) {
        for (int i = m_stepSize; i < m_blockSize + m_blockSize/2; ++i) {
            m_shiftBuffers[c][i - m_stepSize] = m_shiftBuffers[c][i];
        }
        for (int i = 0; i < m_blockSize; ++i) {
            m_shiftBuffers[c][i + m_blockSize/2] = inputBuffers[c][i];
        }
    }

    for (int c = 0; c < m_channels; ++c) {

        m_window->cut(m_shiftBuffers[c], m_ri);

        for (int i = 0; i < m_blockSize/2; ++i) {
            // FFT shift
            Kiss::vamp_kiss_fft_scalar value = m_ri[i];
            m_ri[i] = m_ri[i + m_blockSize/2];
            m_ri[i + m_blockSize/2] = value;
        }

        Kiss::vamp_kiss_fftr(m_cfg, m_ri, m_cbuf);
        
        for (int i = 0; i <= m_blockSize/2; ++i) {
            m_freqbuf[c][i * 2] = float(m_cbuf[i].r);
            m_freqbuf[c][i * 2 + 1] = float(m_cbuf[i].i);
        }
    }

    ++m_processCount;

    return m_plugin->process(m_freqbuf, timestamp);
}

}
        
}

_VAMP_SDK_HOSTSPACE_END(PluginInputDomainAdapter.cpp)

