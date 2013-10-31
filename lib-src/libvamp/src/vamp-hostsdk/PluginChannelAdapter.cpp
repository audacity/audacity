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

#include <vamp-hostsdk/PluginChannelAdapter.h>

_VAMP_SDK_HOSTSPACE_BEGIN(PluginChannelAdapter.cpp)

namespace Vamp {

namespace HostExt {

class PluginChannelAdapter::Impl
{
public:
    Impl(Plugin *plugin);
    ~Impl();

    bool initialise(size_t channels, size_t stepSize, size_t blockSize);

    FeatureSet process(const float *const *inputBuffers, RealTime timestamp);
    FeatureSet processInterleaved(const float *inputBuffers, RealTime timestamp);

protected:
    Plugin *m_plugin;
    size_t m_blockSize;
    size_t m_inputChannels;
    size_t m_pluginChannels;
    float **m_buffer;
    float **m_deinterleave;
    const float **m_forwardPtrs;
};

PluginChannelAdapter::PluginChannelAdapter(Plugin *plugin) :
    PluginWrapper(plugin)
{
    m_impl = new Impl(plugin);
}

PluginChannelAdapter::~PluginChannelAdapter()
{
    delete m_impl;
}

bool
PluginChannelAdapter::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    return m_impl->initialise(channels, stepSize, blockSize);
}

PluginChannelAdapter::FeatureSet
PluginChannelAdapter::process(const float *const *inputBuffers,
                              RealTime timestamp)
{
    return m_impl->process(inputBuffers, timestamp);
}

PluginChannelAdapter::FeatureSet
PluginChannelAdapter::processInterleaved(const float *inputBuffers,
                                         RealTime timestamp)
{
    return m_impl->processInterleaved(inputBuffers, timestamp);
}

PluginChannelAdapter::Impl::Impl(Plugin *plugin) :
    m_plugin(plugin),
    m_blockSize(0),
    m_inputChannels(0),
    m_pluginChannels(0),
    m_buffer(0),
    m_deinterleave(0),
    m_forwardPtrs(0)
{
}

PluginChannelAdapter::Impl::~Impl()
{
    // the adapter will delete the plugin

    if (m_buffer) {
        if (m_inputChannels > m_pluginChannels) {
            delete[] m_buffer[0];
        } else {
            for (size_t i = 0; i < m_pluginChannels - m_inputChannels; ++i) {
                delete[] m_buffer[i];
            }
        }
        delete[] m_buffer;
        m_buffer = 0;
    }

    if (m_deinterleave) {
        for (size_t i = 0; i < m_inputChannels; ++i) {
            delete[] m_deinterleave[i];
        }
        delete[] m_deinterleave;
        m_deinterleave = 0;
    }

    if (m_forwardPtrs) {
        delete[] m_forwardPtrs;
        m_forwardPtrs = 0;
    }
}

bool
PluginChannelAdapter::Impl::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    m_blockSize = blockSize;

    size_t minch = m_plugin->getMinChannelCount();
    size_t maxch = m_plugin->getMaxChannelCount();

    m_inputChannels = channels;

    if (m_inputChannels < minch) {

        m_forwardPtrs = new const float *[minch];

        if (m_inputChannels > 1) {
            // We need a set of zero-valued buffers to add to the
            // forwarded pointers
            m_buffer = new float*[minch - channels];
            for (size_t i = 0; i < minch; ++i) {
                m_buffer[i] = new float[blockSize];
                for (size_t j = 0; j < blockSize; ++j) {
                    m_buffer[i][j] = 0.f;
                }
            }
        }

        m_pluginChannels = minch;

//        std::cerr << "PluginChannelAdapter::initialise: expanding " << m_inputChannels << " to " << m_pluginChannels << " for plugin" << std::endl;

    } else if (m_inputChannels > maxch) {

        // We only need m_buffer if we are mixing down to a single
        // channel -- otherwise we can just forward the same float* as
        // passed in to process(), expecting the excess to be ignored

        if (maxch == 1) {
            m_buffer = new float *[1];
            m_buffer[0] = new float[blockSize];

//            std::cerr << "PluginChannelAdapter::initialise: mixing " << m_inputChannels << " to mono for plugin" << std::endl;

        } else {
            
//            std::cerr << "PluginChannelAdapter::initialise: reducing " << m_inputChannels << " to " << m_pluginChannels << " for plugin" << std::endl;
        }

        m_pluginChannels = maxch;

    } else {
 
//        std::cerr << "PluginChannelAdapter::initialise: accepting given number of channels (" << m_inputChannels << ")" << std::endl;
        m_pluginChannels = m_inputChannels;
    }

    return m_plugin->initialise(m_pluginChannels, stepSize, blockSize);
}

PluginChannelAdapter::FeatureSet
PluginChannelAdapter::Impl::processInterleaved(const float *inputBuffers,
                                               RealTime timestamp)
{
    if (!m_deinterleave) {
        m_deinterleave = new float *[m_inputChannels];
        for (size_t i = 0; i < m_inputChannels; ++i) {
            m_deinterleave[i] = new float[m_blockSize];
        }
    }

    for (size_t i = 0; i < m_inputChannels; ++i) {
        for (size_t j = 0; j < m_blockSize; ++j) {
            m_deinterleave[i][j] = inputBuffers[j * m_inputChannels + i];
        }
    }

    return process(m_deinterleave, timestamp);
}

PluginChannelAdapter::FeatureSet
PluginChannelAdapter::Impl::process(const float *const *inputBuffers,
                                    RealTime timestamp)
{
//    std::cerr << "PluginChannelAdapter::process: " << m_inputChannels << " -> " << m_pluginChannels << " channels" << std::endl;

    if (m_inputChannels < m_pluginChannels) {

        if (m_inputChannels == 1) {
            for (size_t i = 0; i < m_pluginChannels; ++i) {
                m_forwardPtrs[i] = inputBuffers[0];
            }
        } else {
            for (size_t i = 0; i < m_inputChannels; ++i) {
                m_forwardPtrs[i] = inputBuffers[i];
            }
            for (size_t i = m_inputChannels; i < m_pluginChannels; ++i) {
                m_forwardPtrs[i] = m_buffer[i - m_inputChannels];
            }
        }

        return m_plugin->process(m_forwardPtrs, timestamp);

    } else if (m_inputChannels > m_pluginChannels) {

        if (m_pluginChannels == 1) {
            for (size_t j = 0; j < m_blockSize; ++j) {
                m_buffer[0][j] = inputBuffers[0][j];
            }
            for (size_t i = 1; i < m_inputChannels; ++i) {
                for (size_t j = 0; j < m_blockSize; ++j) {
                    m_buffer[0][j] += inputBuffers[i][j];
                }
            }
            for (size_t j = 0; j < m_blockSize; ++j) {
                m_buffer[0][j] /= m_inputChannels;
            }
            return m_plugin->process(m_buffer, timestamp);
        } else {
            return m_plugin->process(inputBuffers, timestamp);
        }

    } else {

        return m_plugin->process(inputBuffers, timestamp);
    }
}

}

}

_VAMP_SDK_HOSTSPACE_END(PluginChannelAdapter.cpp)


