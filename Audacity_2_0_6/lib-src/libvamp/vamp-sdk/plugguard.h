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

#ifndef _VAMP_SDK_PLUGGUARD_H_
#define _VAMP_SDK_PLUGGUARD_H_

/**
 * Normal usage should be:
 *
 * - Plugins include vamp-sdk/Plugin.h or vamp-sdk/PluginBase.h.
 *   These files include this header, which specifies an appropriate
 *   namespace for the plugin classes to avoid any risk of conflict
 *   with non-plugin class implementations in the host on load.
 *
 * - Hosts include vamp-hostsdk/Plugin.h, vamp-hostsdk/PluginBase.h,
 *   vamp-hostsdk/PluginHostAdapter, vamp-hostsdk/PluginLoader.h etc.
 *   These files include vamp-hostsdk/hostguard.h, which makes a note
 *   that we are in a host.  A file such as vamp-hostsdk/Plugin.h
 *   then simply includes vamp-sdk/Plugin.h, and this guard header
 *   takes notice of the fact that it has been included from a host
 *   and leaves the plugin namespace unset.
 *
 * Problems will occur when a host includes files directly from the
 * vamp-sdk directory.  There are two reasons this might happen:
 * mistake, perhaps owing to ignorance of the fact that this isn't
 * allowed (particularly since it was the normal mechanism in v1 of
 * the SDK); and a wish to incorporate plugin code directly into the
 * host rather than having to load it.
 *
 * What if the host does include a vamp-sdk header by mistake?  We can
 * catch it if it's included before something from vamp-hostsdk.  If
 * it's included after something from vamp-hostsdk, it will work OK
 * anyway.  The remaining problem case is where nothing from
 * vamp-hostsdk is included in the same file.  We can't catch that.
 */

#ifndef _VAMP_IN_HOSTSDK

#define _VAMP_IN_PLUGINSDK 1

#define VAMP_SDK_VERSION "2.5"
#define VAMP_SDK_MAJOR_VERSION 2
#define VAMP_SDK_MINOR_VERSION 5

#ifdef _VAMP_NO_PLUGIN_NAMESPACE
#define _VAMP_SDK_PLUGSPACE_BEGIN(h)
#define _VAMP_SDK_PLUGSPACE_END(h)
#else
#ifdef _VAMP_PLUGIN_IN_HOST_NAMESPACE
#define _VAMP_SDK_PLUGSPACE_BEGIN(h) \
	namespace _VampHost {

#define _VAMP_SDK_PLUGSPACE_END(h) \
	} \
	using namespace _VampHost;
#else
#define _VAMP_SDK_PLUGSPACE_BEGIN(h) \
	namespace _VampPlugin {

#define _VAMP_SDK_PLUGSPACE_END(h) \
	} \
	using namespace _VampPlugin;
#endif
#endif

#endif

#endif

