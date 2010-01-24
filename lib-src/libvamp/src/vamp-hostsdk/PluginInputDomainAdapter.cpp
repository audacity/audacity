/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2007 Chris Cannam and QMUL.
  
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

    size_t getPreferredStepSize() const;
    size_t getPreferredBlockSize() const;

    FeatureSet process(const float *const *inputBuffers, RealTime timestamp);
    
    RealTime getTimestampAdjustment() const;

protected:
    Plugin *m_plugin;
    float m_inputSampleRate;
    int m_channels;
    int m_blockSize;
    float **m_freqbuf;

    double *m_ri;
    double *m_window;

#ifdef HAVE_FFTW3
    fftw_plan m_plan;
    fftw_complex *m_cbuf;
#else
    double *m_ro;
    double *m_io;
    void fft(unsigned int n, bool inverse,
             double *ri, double *ii, double *ro, double *io);
#endif

    size_t makeBlockSizeAcceptable(size_t) const;
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

RealTime
PluginInputDomainAdapter::getTimestampAdjustment() const
{
    return m_impl->getTimestampAdjustment();
}


PluginInputDomainAdapter::Impl::Impl(Plugin *plugin, float inputSampleRate) :
    m_plugin(plugin),
    m_inputSampleRate(inputSampleRate),
    m_channels(0),
    m_blockSize(0),
    m_freqbuf(0),
    m_ri(0),
    m_window(0),
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
        delete[] m_window;
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

        m_blockSize = int(blockSize);
        m_channels = int(channels);

        return m_plugin->initialise(channels, stepSize, blockSize);
    }

    if (blockSize < 2) {
        std::cerr << "ERROR: Vamp::HostExt::PluginInputDomainAdapter::Impl::initialise: blocksize < 2 not supported" << std::endl;
        return false;
    }                
        
    if (blockSize & (blockSize-1)) {
        std::cerr << "ERROR: Vamp::HostExt::PluginInputDomainAdapter::Impl::initialise: non-power-of-two\nblocksize " << blockSize << " not supported" << std::endl;
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
        delete[] m_window;
    }

    m_blockSize = int(blockSize);
    m_channels = int(channels);

    m_freqbuf = new float *[m_channels];
    for (int c = 0; c < m_channels; ++c) {
        m_freqbuf[c] = new float[m_blockSize + 2];
    }
    m_window = new double[m_blockSize];

    for (int i = 0; i < m_blockSize; ++i) {
        // Hanning window
        m_window[i] = (0.50 - 0.50 * cos((2.0 * M_PI * i) / m_blockSize));
    }

#ifdef HAVE_FFTW3
    m_ri = (double *)fftw_malloc(blockSize * sizeof(double));
    m_cbuf = (fftw_complex *)fftw_malloc((blockSize/2 + 1) * sizeof(fftw_complex));
    m_plan = fftw_plan_dft_r2c_1d(blockSize, m_ri, m_cbuf, FFTW_MEASURE);
#else
    m_ri = new double[m_blockSize];
    m_ro = new double[m_blockSize];
    m_io = new double[m_blockSize];
#endif

    return m_plugin->initialise(channels, stepSize, blockSize);
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

        std::cerr << "WARNING: Vamp::HostExt::PluginInputDomainAdapter::Impl::initialise: blocksize < 2 not" << std::endl
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
        
        std::cerr << "WARNING: Vamp::HostExt::PluginInputDomainAdapter::Impl::initialise: non-power-of-two\nblocksize " << blockSize << " not supported, using blocksize " << nearest << " instead" << std::endl;
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
    } else {
        return RealTime::frame2RealTime
            (m_blockSize/2, int(m_inputSampleRate + 0.5));
    }
}

Plugin::FeatureSet
PluginInputDomainAdapter::Impl::process(const float *const *inputBuffers,
                                        RealTime timestamp)
{
    if (m_plugin->getInputDomain() == TimeDomain) {
        return m_plugin->process(inputBuffers, timestamp);
    }

    // The timestamp supplied should be (according to the Vamp::Plugin
    // spec) the time of the start of the time-domain input block.
    // However, we want to pass to the plugin an FFT output calculated
    // from the block of samples _centred_ on that timestamp.
    // 
    // We have two options:
    // 
    // 1. Buffer the input, calculating the fft of the values at the
    // passed-in block minus blockSize/2 rather than starting at the
    // passed-in block.  So each time we call process on the plugin,
    // we are passing in the same timestamp as was passed to our own
    // process plugin, but not (the frequency domain representation
    // of) the same set of samples.  Advantages: avoids confusion in
    // the host by ensuring the returned values have timestamps
    // comparable with that passed in to this function (in fact this
    // is pretty much essential for one-value-per-block outputs);
    // consistent with hosts such as SV that deal with the
    // frequency-domain transform themselves.  Disadvantages: means
    // making the not necessarily correct assumption that the samples
    // preceding the first official block are all zero (or some other
    // known value).
    //
    // 2. Increase the passed-in timestamps by half the blocksize.  So
    // when we call process, we are passing in the frequency domain
    // representation of the same set of samples as passed to us, but
    // with a different timestamp.  Advantages: simplicity; avoids
    // iffy assumption mentioned above.  Disadvantages: inconsistency
    // with SV in cases where stepSize != blockSize/2; potential
    // confusion arising from returned timestamps being calculated
    // from the adjusted input timestamps rather than the original
    // ones (and inaccuracy where the returned timestamp is implied,
    // as in one-value-per-block).
    //
    // Neither way is ideal, but I don't think either is strictly
    // incorrect either.  I think this is just a case where the same
    // plugin can legitimately produce differing results from the same
    // input data, depending on how that data is packaged.
    // 
    // We'll go for option 2, adjusting the timestamps.  Note in
    // particular that this means some results can differ from those
    // produced by SV.

//    std::cerr << "PluginInputDomainAdapter: sampleRate " << m_inputSampleRate << ", blocksize " << m_blockSize << ", adjusting time from " << timestamp;

    timestamp = timestamp + getTimestampAdjustment();

//    std::cerr << " to " << timestamp << std::endl;

    for (int c = 0; c < m_channels; ++c) {

        for (int i = 0; i < m_blockSize; ++i) {
            m_ri[i] = double(inputBuffers[c][i]) * m_window[i];
        }

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

#ifndef HAVE_FFTW3

void
PluginInputDomainAdapter::Impl::fft(unsigned int n, bool inverse,
                                    double *ri, double *ii, double *ro, double *io)
{
    if (!ri || !ro || !io) return;

    unsigned int bits;
    unsigned int i, j, k, m;
    unsigned int blockSize, blockEnd;

    double tr, ti;

    if (n < 2) return;
    if (n & (n-1)) return;

    double angle = 2.0 * M_PI;
    if (inverse) angle = -angle;

    for (i = 0; ; ++i) {
	if (n & (1 << i)) {
	    bits = i;
	    break;
	}
    }

    static unsigned int tableSize = 0;
    static int *table = 0;

    if (tableSize != n) {

	delete[] table;

	table = new int[n];

	for (i = 0; i < n; ++i) {
	
	    m = i;

	    for (j = k = 0; j < bits; ++j) {
		k = (k << 1) | (m & 1);
		m >>= 1;
	    }

	    table[i] = k;
	}

	tableSize = n;
    }

    if (ii) {
	for (i = 0; i < n; ++i) {
	    ro[table[i]] = ri[i];
	    io[table[i]] = ii[i];
	}
    } else {
	for (i = 0; i < n; ++i) {
	    ro[table[i]] = ri[i];
	    io[table[i]] = 0.0;
	}
    }

    blockEnd = 1;

    for (blockSize = 2; blockSize <= n; blockSize <<= 1) {

	double delta = angle / (double)blockSize;
	double sm2 = -sin(-2 * delta);
	double sm1 = -sin(-delta);
	double cm2 = cos(-2 * delta);
	double cm1 = cos(-delta);
	double w = 2 * cm1;
	double ar[3], ai[3];

	for (i = 0; i < n; i += blockSize) {

	    ar[2] = cm2;
	    ar[1] = cm1;

	    ai[2] = sm2;
	    ai[1] = sm1;

	    for (j = i, m = 0; m < blockEnd; j++, m++) {

		ar[0] = w * ar[1] - ar[2];
		ar[2] = ar[1];
		ar[1] = ar[0];

		ai[0] = w * ai[1] - ai[2];
		ai[2] = ai[1];
		ai[1] = ai[0];

		k = j + blockEnd;
		tr = ar[0] * ro[k] - ai[0] * io[k];
		ti = ar[0] * io[k] + ai[0] * ro[k];

		ro[k] = ro[j] - tr;
		io[k] = io[j] - ti;

		ro[j] += tr;
		io[j] += ti;
	    }
	}

	blockEnd = blockSize;
    }

    if (inverse) {

	double denom = (double)n;

	for (i = 0; i < n; i++) {
	    ro[i] /= denom;
	    io[i] /= denom;
	}
    }
}

#endif

}
        
}

_VAMP_SDK_HOSTSPACE_END(PluginInputDomainAdapter.cpp)

