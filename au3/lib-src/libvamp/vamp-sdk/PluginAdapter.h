/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006 Chris Cannam.
  
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

#ifndef _VAMP_PLUGIN_ADAPTER_H_
#define _VAMP_PLUGIN_ADAPTER_H_

#include <map>
#include <vamp/vamp.h>

#include "Plugin.h"

#include "plugguard.h"
_VAMP_SDK_PLUGSPACE_BEGIN(PluginAdapter.h)

namespace Vamp {

/**
 * \class PluginAdapterBase PluginAdapter.h <vamp-sdk/PluginAdapter.h>
 * 
 * PluginAdapter and PluginAdapterBase provide a wrapper class that a
 * plugin library can use to make its C++ Vamp::Plugin objects
 * available through the Vamp C API.
 * 
 * Almost all Vamp plugin libraries will want to make use of this.  To
 * do so, all they need to do is declare a PluginAdapter<T> for each
 * plugin class T in their library.  It's very simple, and you need to
 * know absolutely nothing about how it works in order to use it.
 * Just cut and paste from an existing plugin's discovery function.
 * \see vampGetPluginDescriptor
 */

class PluginAdapterBase
{
public:
    virtual ~PluginAdapterBase();

    /**
     * Return a VampPluginDescriptor describing the plugin that is
     * wrapped by this adapter.
     */
    const VampPluginDescriptor *getDescriptor();

protected:
    PluginAdapterBase();

    virtual Plugin *createPlugin(float inputSampleRate) = 0;

    class Impl;
    Impl *m_impl;
};

/**
 * \class PluginAdapter PluginAdapter.h <vamp-sdk/PluginAdapter.h>
 * 
 * PluginAdapter turns a PluginAdapterBase into a specific wrapper for
 * a particular plugin implementation.
 *
 * See PluginAdapterBase.
 */

template <typename P>
class PluginAdapter : public PluginAdapterBase
{
public:
    PluginAdapter() : PluginAdapterBase() { }
    virtual ~PluginAdapter() { }

protected:
    Plugin *createPlugin(float inputSampleRate) {
        P *p = new P(inputSampleRate);
        Plugin *plugin = dynamic_cast<Plugin *>(p);
        if (!plugin) {
            std::cerr << "ERROR: PluginAdapter::createPlugin: "
                      << "Template type is not a plugin!"
                      << std::endl;
            delete p;
            return 0;
        }
        return plugin;
    }
};
    
}

_VAMP_SDK_PLUGSPACE_END(PluginAdapter.h)

#endif

