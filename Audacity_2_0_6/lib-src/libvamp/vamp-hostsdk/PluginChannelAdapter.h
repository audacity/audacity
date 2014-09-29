/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2009 Chris Cannam and QMUL.
  
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

#ifndef _VAMP_PLUGIN_CHANNEL_ADAPTER_H_
#define _VAMP_PLUGIN_CHANNEL_ADAPTER_H_

#include "hostguard.h"
#include "PluginWrapper.h"

_VAMP_SDK_HOSTSPACE_BEGIN(PluginChannelAdapter.h)

namespace Vamp {

namespace HostExt {

/**
 * \class PluginChannelAdapter PluginChannelAdapter.h <vamp-hostsdk/PluginChannelAdapter.h>
 *
 * PluginChannelAdapter is a Vamp plugin adapter that implements a
 * policy for management of plugins that expect a different number of
 * input channels from the number actually available in the source
 * audio data.
 *
 * A host using PluginChannelAdapter may ignore the getMinChannelCount
 * and getMaxChannelCount reported by the plugin, and still expect the
 * plugin to run.
 *
 * PluginChannelAdapter implements the following policy:
 *
 *  - If the plugin supports the provided number of channels directly,
 *  PluginChannelAdapter will just run the plugin as normal.
 *
 *  - If the plugin only supports exactly one channel but more than
 *  one channel is provided, PluginChannelAdapter will use the mean of
 *  the channels.  This ensures that the resulting values remain
 *  within the same magnitude range as expected for mono data.
 *
 *  - If the plugin requires more than one channel but exactly one is
 *  provided, the provided channel will be duplicated across all the
 *  plugin input channels.
 *
 * If none of the above apply:
 * 
 *  - If the plugin requires more channels than are provided, the
 *  minimum acceptable number of channels will be produced by adding
 *  empty (zero valued) channels to those provided.
 *
 *  - If the plugin requires fewer channels than are provided, the
 *  maximum acceptable number of channels will be produced by
 *  discarding the excess channels.
 *
 * Hosts requiring a different channel policy from the above will need
 * to implement it themselves, instead of using PluginChannelAdapter.
 *
 * Note that PluginChannelAdapter does not override the minimum and
 * maximum channel counts returned by the wrapped plugin.  The host
 * will need to be aware that it is using a PluginChannelAdapter, and
 * be prepared to ignore these counts as necessary.  (This contrasts
 * with the approach used in PluginInputDomainAdapter, which aims to
 * make the host completely unaware of which underlying input domain
 * is in fact in use.)
 * 
 * (The rationale for this is that a host may wish to use the
 * PluginChannelAdapter but still discriminate in some way on the
 * basis of the number of channels actually supported.  For example, a
 * simple stereo audio host may prefer to reject plugins that require
 * more than two channels on the grounds that doesn't actually
 * understand what they are for, rather than allow the channel adapter
 * to make a potentially meaningless channel conversion for them.)
 *
 * In every respect other than its management of channels, the
 * PluginChannelAdapter behaves identically to the plugin that it
 * wraps.  The wrapped plugin will be deleted when the wrapper is
 * deleted.
 *
 * \note This class was introduced in version 1.1 of the Vamp plugin SDK.
 */

class PluginChannelAdapter : public PluginWrapper
{
public:
    /**
     * Construct a PluginChannelAdapter wrapping the given plugin.
     * The adapter takes ownership of the plugin, which will be
     * deleted when the adapter is deleted.
     */
    PluginChannelAdapter(Plugin *plugin);
    virtual ~PluginChannelAdapter();

    bool initialise(size_t channels, size_t stepSize, size_t blockSize);

    FeatureSet process(const float *const *inputBuffers, RealTime timestamp);

    /**
     * Call process(), providing interleaved audio data with the
     * number of channels passed to initialise().  The adapter will
     * de-interleave into temporary buffers as appropriate before
     * calling process().
     *
     * \note This function was introduced in version 1.4 of the Vamp
     * plugin SDK.
     */
    FeatureSet processInterleaved(const float *inputBuffer, RealTime timestamp);

protected:
    class Impl;
    Impl *m_impl;
};

}

}

_VAMP_SDK_HOSTSPACE_END(PluginChannelAdapter.h)

#endif
