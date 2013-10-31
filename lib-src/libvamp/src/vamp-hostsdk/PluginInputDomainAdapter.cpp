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


/**
 * If you want to compile using FFTW instead of the built-in FFT
 * implementation for the PluginInputDomainAdapter, define HAVE_FFTW3
 * in the Makefile.
 *
 * Be aware that FFTW is licensed under the GPL -- unlike this SDK,
 * which is provided under a more liberal BSD license in order to
 * permit use in closed source applications.  The use of FFTW would
 * mean that your code would need to be licensed under the GPL as
 * well.  Do not define this symbol unless you understand and accept
 * the implications of this.
 *
 * Parties such as Linux distribution packagers who redistribute this
 * SDK for use in other programs should _not_ define this symbol, as
 * it would change the effective licensing terms under which the SDK
 * was available to third party developers.
 *
 * The default is not to use FFTW, and to use the built-in FFT instead.
 * 
 * Note: The FFTW code uses FFTW_MEASURE, and so will perform badly on
 * its first invocation unless the host has saved and restored FFTW
 * wisdom (see the FFTW documentation).
 */
#ifdef HAVE_FFTW3
#include <fftw3.h>
#warning "Compiling with FFTW3 support will result in a GPL binary"
#else
#include "../vamp-sdk/FFTimpl.cpp"
#endif


_VAMP_SDK_HOSTSPACE_BEGIN(PluginInputDomainAdapter.cpp)

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

    double *m_ri;

    WindowType m_windowType;
    Window<double> *m_window;

    ProcessTimestampMethod m_method;
    int m_processCount;
    float **m_shiftBuffers;

#ifdef HAVE_FFTW3
    fftw_plan m_plan;
    fftw_complex *m_cbuf;
#else
    double *m_ro;
    double *m_io;
#endif

    FeatureSet processShiftingTimestamp(const float *const *inputBuffers, RealTime timestamp);
    FeatureSet processShiftingData(const float *const *inputBuffers, RealTime timestamp);

    size_t makeBlockSizeAcceptable(size_t) const;
    
    Window<double>::WindowType convertType(WindowType t) const;
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
#ifdef HAVE_FFTW3
    m_plan(0),
    m_cbuf(0)
#else
    m_ro(0),
    m_io(0)
#endif
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
#ifdef HAVE_FFTW3
        if (m_plan) {
            fftw_destroy_plan(m_plan);
            fftw_free(m_ri);
            fftw_free(m_cbuf);
            m_plan = 0;
        }
#else
        delete[] m_ri;
        delete[] m_ro;
        delete[] m_io;
#endif

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
        
    if (blockSize & (blockSize-1)) {
        std::cerr << "ERROR: PluginInputDomainAdapter::initialise: non-power-of-two\nblocksize " << blockSize << " not supported" << std::endl;
        return false;
    }

    if (m_channels > 0) {
        for (int c = 0; c < m_channels; ++c) {
            delete[] m_freqbuf[c];
        }
        delete[] m_freqbuf;
#ifdef HAVE_FFTW3
        if (m_plan) {
            fftw_destroy_plan(m_plan);
            fftw_free(m_ri);
            fftw_free(m_cbuf);
            m_plan = 0;
        }
#else
        delete[] m_ri;
        delete[] m_ro;
        delete[] m_io;
#endif
        delete m_window;
    }

    m_stepSize = int(stepSize);
    m_blockSize = int(blockSize);
    m_channels = int(channels);

    m_freqbuf = new float *[m_channels];
    for (int c = 0; c < m_channels; ++c) {
        m_freqbuf[c] = new float[m_blockSize + 2];
    }

    m_window = new Window<double>(convertType(m_windowType), m_blockSize);

#ifdef HAVE_FFTW3
    m_ri = (double *)fftw_malloc(blockSize * sizeof(double));
    m_cbuf = (fftw_complex *)fftw_malloc((blockSize/2 + 1) * sizeof(fftw_complex));
    m_plan = fftw_plan_dft_r2c_1d(blockSize, m_ri, m_cbuf, FFTW_MEASURE);
#else
    m_ri = new double[m_blockSize];
    m_ro = new double[m_blockSize];
    m_io = new double[m_blockSize];
#endif

    m_processCount = 0;

    return m_plugin->initialise(channels, stepSize, blockSize);
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
        
    } else if (blockSize & (blockSize-1)) {
            
#ifdef HAVE_FFTW3
        // not an issue with FFTW
#else

        // not a power of two, can't handle that with our built-in FFT
        // implementation

        size_t nearest = blockSize;
        size_t power = 0;
        while (nearest > 1) {
            nearest >>= 1;
            ++power;
        }
        nearest = 1;
        while (power) {
            nearest <<= 1;
            --power;
        }
        
        if (blockSize - nearest > (nearest*2) - blockSize) {
            nearest = nearest*2;
        }
        
        std::cerr << "WARNING: PluginInputDomainAdapter::initialise: non-power-of-two\nblocksize " << blockSize << " not supported, using blocksize " << nearest << " instead" << std::endl;
        blockSize = nearest;

#endif
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
        m_window = new Window<double>(convertType(m_windowType), m_blockSize);
    }
}

PluginInputDomainAdapter::WindowType
PluginInputDomainAdapter::Impl::getWindowType() const
{
    return m_windowType;
}

Window<double>::WindowType
PluginInputDomainAdapter::Impl::convertType(WindowType t) const
{
    switch (t) {
    case RectangularWindow:
        return Window<double>::RectangularWindow;
    case BartlettWindow:
        return Window<double>::BartlettWindow;
    case HammingWindow:
        return Window<double>::HammingWindow;
    case HanningWindow:
        return Window<double>::HanningWindow;
    case BlackmanWindow:
        return Window<double>::BlackmanWindow;
    case NuttallWindow:
        return Window<double>::NuttallWindow;
    case BlackmanHarrisWindow:
        return Window<double>::BlackmanHarrisWindow;
    default:
	return Window<double>::HanningWindow;
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
    if (m_method == ShiftTimestamp) {
        timestamp = timestamp + getTimestampAdjustment();
    }

    for (int c = 0; c < m_channels; ++c) {

        m_window->cut(inputBuffers[c], m_ri);

        for (int i = 0; i < m_blockSize/2; ++i) {
            // FFT shift
            double value = m_ri[i];
            m_ri[i] = m_ri[i + m_blockSize/2];
            m_ri[i + m_blockSize/2] = value;
        }

#ifdef HAVE_FFTW3
        fftw_execute(m_plan);

        for (int i = 0; i <= m_blockSize/2; ++i) {
            m_freqbuf[c][i * 2] = float(m_cbuf[i][0]);
            m_freqbuf[c][i * 2 + 1] = float(m_cbuf[i][1]);
        }
#else
        fft(m_blockSize, false, m_ri, 0, m_ro, m_io);

        for (int i = 0; i <= m_blockSize/2; ++i) {
            m_freqbuf[c][i * 2] = float(m_ro[i]);
            m_freqbuf[c][i * 2 + 1] = float(m_io[i]);
        }
#endif
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
            double value = m_ri[i];
            m_ri[i] = m_ri[i + m_blockSize/2];
            m_ri[i + m_blockSize/2] = value;
        }

#ifdef HAVE_FFTW3
        fftw_execute(m_plan);

        for (int i = 0; i <= m_blockSize/2; ++i) {
            m_freqbuf[c][i * 2] = float(m_cbuf[i][0]);
            m_freqbuf[c][i * 2 + 1] = float(m_cbuf[i][1]);
        }
#else
        fft(m_blockSize, false, m_ri, 0, m_ro, m_io);

        for (int i = 0; i <= m_blockSize/2; ++i) {
            m_freqbuf[c][i * 2] = float(m_ro[i]);
            m_freqbuf[c][i * 2 + 1] = float(m_io[i]);
        }
#endif
    }

    ++m_processCount;

    return m_plugin->process(m_freqbuf, timestamp);
}

#ifndef HAVE_FFTW3

#endif

}
        
}

_VAMP_SDK_HOSTSPACE_END(PluginInputDomainAdapter.cpp)

