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

#include <vamp-hostsdk/host-c.h>

#include "Files.h"

#include <map>
#include <iostream>

#include <cstring>

using namespace std;

static vector<string> files;
static map<string, const char *> cnames;
static bool haveFiles = false;

struct vhLibrary_t {
    vhLibrary_t(void *h, VampGetPluginDescriptorFunction f)
        : handle(h), func(f), nplugins(0) { } 
    void *handle;
    VampGetPluginDescriptorFunction func;
    int nplugins;
};

static void initFilenames()
{
    if (!haveFiles) {
        files = Files::listLibraryFiles();
        for (size_t i = 0; i < files.size(); ++i) {
            cnames[files[i]] = strdup(Files::lcBasename(files[i]).c_str());
        }
        haveFiles = true;
    }
}

int vhGetLibraryCount()
{
    initFilenames();
    return int(files.size());
}

const char *vhGetLibraryName(int index)
{
    initFilenames();
    if (index >= 0 && index < int(files.size())) {
        return cnames[files[index]];
    }
    else return 0;
}

int vhGetLibraryIndex(const char *name)
{
    for (size_t i = 0; i < files.size(); ++i) {
        if (Files::lcBasename(name) == Files::lcBasename(files[i])) {
            return i;
        }
    }
    return -1;
}

vhLibrary vhLoadLibrary(int index)
{
    initFilenames();
    if (index < 0 || index >= int(files.size())) {
        return 0;
    }

    string fullPath = files[index];
    void *lib = Files::loadLibrary(fullPath);

    if (!lib) return 0;
            
    VampGetPluginDescriptorFunction func =
        (VampGetPluginDescriptorFunction)Files::lookupInLibrary
        (lib, "vampGetPluginDescriptor");
    if (!func) {
        cerr << "vhLoadLibrary: No vampGetPluginDescriptor function found in library \""
             << fullPath << "\"" << endl;
        Files::unloadLibrary(lib);
        return 0;
    }

    vhLibrary_t *vhl = new vhLibrary_t(lib, func);
    while (vhl->func(VAMP_API_VERSION, vhl->nplugins)) {
        ++vhl->nplugins;
    }
    return vhl;
}

int vhGetPluginCount(vhLibrary library)
{
    vhLibrary_t *vhl = static_cast<vhLibrary_t *>(library);
    if (vhl) return vhl->nplugins;
    else return 0;
}

const VampPluginDescriptor *vhGetPluginDescriptor(vhLibrary library,
                                                  int plugin)
{
    vhLibrary_t *vhl = static_cast<vhLibrary_t *>(library);
    if (vhl && plugin >= 0 && plugin < vhl->nplugins) {
        return vhl->func(VAMP_API_VERSION, plugin);
    } else {
        return 0;
    }
}

void vhUnloadLibrary(vhLibrary library)
{
    vhLibrary_t *vhl = static_cast<vhLibrary_t *>(library);
    if (vhl && vhl->handle) {
        Files::unloadLibrary(vhl->handle);
    }
    delete vhl;
}

