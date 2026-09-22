/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2015 Chris Cannam and QMUL.
  
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

/*
    This file defines a low-level API for enumerating and loading
    plugin libraries using C calling conventions. It could be used in
    C programs, or in languages with C-compatible foreign-function
    interfaces. Note that this works by calling to the C++ Vamp host
    SDK, so any program using this interface must still link against
    the rest of the Vamp plugin library and the C++ standard library.

    This is not the simplest or easiest interface for hosting Vamp
    plugins -- if you have the capability to use the C++ API, please
    do that instead. (Most programs should not even include this
    header.)

    The C and C++ interfaces provide different abstraction levels:

    In the C++ interface, the class PluginLoader provides a list of
    keys corresponding to the installed plugins (where a key combines
    the plugin's library name and plugin identifier into a single
    string) plus a method to load a single plugin based on its key
    (obtaining an instance of class Plugin). With the C++ interface
    you go straight from the key to a live instance of the plugin. The
    PluginLoader also provides various facilities to adapt the plugin
    based on your requirements (e.g. to do time- to frequency-domain
    conversion for you if the plugin requires it).

    This low-level C interface, on the other hand, deals only in
    plugin libraries and static descriptors, not in plugin
    instances. You can enumerate the installed libraries, getting just
    the base .soname of each library. Then you can retrieve each of
    the raw C plugin descriptors from a library, and use the
    descriptor (whose interface is defined in vamp/vamp.h) to
    instantiate the plugin.

    So this header corresponds to the first part of the PluginLoader
    class interface: finding and loading plugin libraries and
    retrieving plugin descriptors from them. But it does not do any of
    the rest, i.e. instantiating and adapting the plugins themselves.
    Although this makes the API appear very simple, it means the
    resulting plugins are relatively hard to use compared to those
    obtained by the PluginLoader API. There is no way to get to the
    full C++ abstraction using this API.

    This API is not thread-safe; use it from a single application
    thread, or guard access to it with a mutex.

    This header was introduced in version 2.6 of the Vamp plugin SDK.
*/

#ifndef VAMPHOST_C_H_INCLUDED
#define VAMPHOST_C_H_INCLUDED

#include <vamp/vamp.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct vhLibrary_t *vhLibrary;

/**
 * Return the number of Vamp plugin libraries discovered in the
 * installation path. This number will remain fixed after the first
 * call -- plugins are only discovered once, the first time this
 * function is called.
 */
extern int vhGetLibraryCount();

/**
 * Return the library name (base soname) of the library with the given
 * index, in the range 0..(vhGetLibraryCount()-1).
 */
extern const char *vhGetLibraryName(int library);

/**
 * Return the library index for the given library name, or -1 if the
 * name is not known.
 */
extern int vhGetLibraryIndex(const char *name);
    
/**
 * Load the library with the given index. If the library cannot be
 * loaded for any reason, the return value is 0; otherwise it is an
 * opaque pointer suitable for passing to other functions in this API.
 */
extern vhLibrary vhLoadLibrary(int library);

/**
 * Return the number of Vamp plugins in the given library.
 */
extern int vhGetPluginCount(vhLibrary library);

/**
 * Return a Vamp plugin descriptor for a plugin in a given
 * library. This simply calls the vampGetPluginDescriptor function in
 * that library with the given plugin index and returns the
 * result. See vamp/vamp.h for details about the plugin descriptor.
 */ 
extern const VampPluginDescriptor *vhGetPluginDescriptor(vhLibrary library,
							 int plugin);

/**
 * Unload a plugin library. Do not do this while any of its plugins
 * are still in use.
 */
extern void vhUnloadLibrary(vhLibrary);
    
#ifdef __cplusplus
}
#endif

#endif
