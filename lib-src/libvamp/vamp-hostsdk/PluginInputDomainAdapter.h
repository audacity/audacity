/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2007 Chris Cannam and QMUL.
  
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

#ifndef _VAMP_PLUGIN_INPUT_DOMAIN_ADAPTER_H_
#define _VAMP_PLUGIN_INPUT_DOMAIN_ADAPTER_H_

#include "hostguard.h"
#include "PluginWrapper.h"

_VAMP_SDK_HOSTSPACE_BEGIN(PluginInputDomainAdapter.h)

namespace Vamp {

namespace HostExt {

/**
 * \class PluginInputDomainAdapter PluginInputDomainAdapter.h <vamp-hostsdk/PluginInputDomainAdapter.h>
 * 
 * PluginInputDomainAdapter is a Vamp plugin adapter that converts
 * time-domain input into frequency-domain input for plugins that need
 * it.  This permits a host to use time- and frequency-domain plugins
 * interchangeably without needing to handle the conversion itself.
 *
 * This adapter uses a basic Hanning windowed FFT that supports
 * power-of-two block sizes only.  If a frequency domain plugin
 * requests a non-power-of-two blocksize, the adapter will adjust it
 * to a nearby power of two instead.  Thus, getPreferredBlockSize()
 * will always return a power of two if the wrapped plugin is a
 * frequency domain one.  If the plugin doesn't accept the adjusted
 * power of two block size, initialise() will fail.
 *
 * The adapter provides no way for the host to discover whether the
 * underlying plugin is actually a time or frequency domain plugin
 * (except that if the preferred block size is not a power of two, it
 * must be a time domain plugin).
 *
 * The FFT implementation is simple and self-contained, but unlikely
 * to be the fastest available: a host can usually do better if it
 * cares enough.
 *
 * In every respect other than its input domain handling, the
 * PluginInputDomainAdapter behaves identically to the plugin that it
 * wraps.  The wrapped plugin will be deleted when the wrapper is
 * deleted.
 *
 * \note This class was introduced in version 1.1 of the Vamp plugin SDK.
 */

class PluginInputDomainAdapter : public PluginWrapper
{
public:
    /**
     * Construct a PluginInputDomainAdapter wrapping the given plugin.
     * The adapter takes ownership of the plugin, which will be
     * deleted when the adapter is deleted.
     */
    PluginInputDomainAdapter(Plugin *plugin);
    virtual ~PluginInputDomainAdapter();
    
    bool initialise(size_t channels, size_t stepSize, size_t blockSize);

    InputDomain getInputDomain() const;

    size_t getPreferredStepSize() const;
    size_t getPreferredBlockSize() const;

    FeatureSet process(const float *const *inputBuffers, RealTime timestamp);

    /**
     * Return the amount by which the timestamps supplied to process()
     * are being incremented when they are passed to the plugin's own
     * process() implementation.
     *
     * The Vamp API mandates that the timestamp passed to the plugin
     * for time-domain input should be the time of the first sample in
     * the block, but the timestamp passed for frequency-domain input
     * should be the timestamp of the centre of the block.
     *
     * The PluginInputDomainAdapter adjusts its timestamps properly so
     * that the plugin receives correct times, but in some
     * circumstances (such as for establishing the correct timing of
     * implicitly-timed features, i.e. features without their own
     * timestamps) the host may need to be aware that this adjustment
     * is taking place.
     *
     * If the plugin requires time-domain input, this function will
     * return zero.  The result of calling this function before
     * initialise() has been called is undefined.
     */
    RealTime getTimestampAdjustment() const;

protected:
    class Impl;
    Impl *m_impl;
};

}

}

_VAMP_SDK_HOSTSPACE_END(PluginInputDomainAdapter.h)

#endif
