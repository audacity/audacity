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

#include <vamp-hostsdk/PluginHostAdapter.h>
#include <vamp-hostsdk/PluginLoader.h>
#include <vamp-hostsdk/PluginInputDomainAdapter.h>
#include <vamp-hostsdk/PluginChannelAdapter.h>
#include <vamp-hostsdk/PluginBufferingAdapter.h>

#include <fstream>
#include <cctype> // tolower

#include <cstring>

#ifdef _WIN32

#include <windows.h>
#include <tchar.h>
#define PLUGIN_SUFFIX "dll"

#else /* ! _WIN32 */

#include <dirent.h>
#include <dlfcn.h>

#ifdef __APPLE__
#define PLUGIN_SUFFIX "dylib"
#else /* ! __APPLE__ */
#define PLUGIN_SUFFIX "so"
#endif /* ! __APPLE__ */

#endif /* ! _WIN32 */

using namespace std;

_VAMP_SDK_HOSTSPACE_BEGIN(PluginLoader.cpp)

namespace Vamp {
	
namespace HostExt {

class PluginLoader::Impl
{
public:
    Impl();
    virtual ~Impl();

    PluginKeyList listPlugins();

    Plugin *loadPlugin(PluginKey key,
                       float inputSampleRate,
                       int adapterFlags);

    PluginKey composePluginKey(string libraryName, string identifier);

    PluginCategoryHierarchy getPluginCategory(PluginKey key);

    string getLibraryPathForPlugin(PluginKey key);

    static void setInstanceToClean(PluginLoader *instance);

protected:
    class PluginDeletionNotifyAdapter : public PluginWrapper {
    public:
        PluginDeletionNotifyAdapter(Plugin *plugin, Impl *loader);
        virtual ~PluginDeletionNotifyAdapter();
    protected:
        Impl *m_loader;
    };

    class InstanceCleaner {
    public:
        InstanceCleaner() : m_instance(0) { }
        ~InstanceCleaner() { delete m_instance; }
        void setInstance(PluginLoader *instance) { m_instance = instance; }
    protected:
        PluginLoader *m_instance;
    };

    virtual void pluginDeleted(PluginDeletionNotifyAdapter *adapter);

    map<PluginKey, string> m_pluginLibraryNameMap;
    bool m_allPluginsEnumerated;
    void enumeratePlugins(PluginKey forPlugin = "");

    map<PluginKey, PluginCategoryHierarchy> m_taxonomy;
    void generateTaxonomy();

    map<Plugin *, void *> m_pluginLibraryHandleMap;

    bool decomposePluginKey(PluginKey key,
                            string &libraryName, string &identifier);

    void *loadLibrary(string path);
    void unloadLibrary(void *handle);
    void *lookupInLibrary(void *handle, const char *symbol);

    string splicePath(string a, string b);
    vector<string> listFiles(string dir, string ext);
    
    static InstanceCleaner m_cleaner;
};

PluginLoader *
PluginLoader::m_instance = 0;

PluginLoader::Impl::InstanceCleaner
PluginLoader::Impl::m_cleaner;

PluginLoader::PluginLoader()
{
    m_impl = new Impl();
}

PluginLoader::~PluginLoader()
{
    delete m_impl;
}

PluginLoader *
PluginLoader::getInstance()
{
    if (!m_instance) {
        // The cleaner doesn't own the instance, because we leave the
        // instance pointer in the base class for binary backwards
        // compatibility reasons and to avoid waste
        m_instance = new PluginLoader();
        Impl::setInstanceToClean(m_instance);
    }
    return m_instance;
}

vector<PluginLoader::PluginKey>
PluginLoader::listPlugins() 
{
    return m_impl->listPlugins();
}

Plugin *
PluginLoader::loadPlugin(PluginKey key,
                         float inputSampleRate,
                         int adapterFlags)
{
    return m_impl->loadPlugin(key, inputSampleRate, adapterFlags);
}

PluginLoader::PluginKey
PluginLoader::composePluginKey(string libraryName, string identifier) 
{
    return m_impl->composePluginKey(libraryName, identifier);
}

PluginLoader::PluginCategoryHierarchy
PluginLoader::getPluginCategory(PluginKey key)
{
    return m_impl->getPluginCategory(key);
}

string
PluginLoader::getLibraryPathForPlugin(PluginKey key)
{
    return m_impl->getLibraryPathForPlugin(key);
}
 
PluginLoader::Impl::Impl() :
    m_allPluginsEnumerated(false)
{
}

PluginLoader::Impl::~Impl()
{
}

void
PluginLoader::Impl::setInstanceToClean(PluginLoader *instance)
{
    m_cleaner.setInstance(instance);
}

vector<PluginLoader::PluginKey>
PluginLoader::Impl::listPlugins() 
{
    if (!m_allPluginsEnumerated) enumeratePlugins();

    vector<PluginKey> plugins;
    for (map<PluginKey, string>::iterator mi = m_pluginLibraryNameMap.begin();
         mi != m_pluginLibraryNameMap.end(); ++mi) {
        plugins.push_back(mi->first);
    }

    return plugins;
}

void
PluginLoader::Impl::enumeratePlugins(PluginKey forPlugin)
{
    vector<string> path = PluginHostAdapter::getPluginPath();

    string libraryName, identifier;
    if (forPlugin != "") {
        if (!decomposePluginKey(forPlugin, libraryName, identifier)) {
            std::cerr << "WARNING: Vamp::HostExt::PluginLoader: Invalid plugin key \""
                      << forPlugin << "\" in enumerate" << std::endl;
            return;
        }
    }

    for (size_t i = 0; i < path.size(); ++i) {
        
        vector<string> files = listFiles(path[i], PLUGIN_SUFFIX);

        for (vector<string>::iterator fi = files.begin();
             fi != files.end(); ++fi) {
            
            if (libraryName != "") {
                // libraryName is lowercased and lacking an extension,
                // as it came from the plugin key
                string temp = *fi;
                for (size_t i = 0; i < temp.length(); ++i) {
                    temp[i] = tolower(temp[i]);
                }
                string::size_type pi = temp.find('.');
                if (pi == string::npos) {
                    if (libraryName != temp) continue;
                } else {
                    if (libraryName != temp.substr(0, pi)) continue;
                }
            }

            string fullPath = path[i];
            fullPath = splicePath(fullPath, *fi);
            void *handle = loadLibrary(fullPath);
            if (!handle) continue;
            
            VampGetPluginDescriptorFunction fn =
                (VampGetPluginDescriptorFunction)lookupInLibrary
                (handle, "vampGetPluginDescriptor");
            
            if (!fn) {
                if (forPlugin != "") {
                    cerr << "Vamp::HostExt::PluginLoader: No vampGetPluginDescriptor function found in library \""
                         << fullPath << "\"" << endl;
                }
                unloadLibrary(handle);
                continue;
            }
            
            int index = 0;
            const VampPluginDescriptor *descriptor = 0;
            bool found = false;
            
            while ((descriptor = fn(VAMP_API_VERSION, index))) {
                ++index;
                if (identifier != "") {
                    if (descriptor->identifier != identifier) continue;
                }
                found = true;
                PluginKey key = composePluginKey(*fi, descriptor->identifier);
//                std::cerr << "enumerate: " << key << " (path: " << fullPath << ")" << std::endl;
                if (m_pluginLibraryNameMap.find(key) ==
                    m_pluginLibraryNameMap.end()) {
                    m_pluginLibraryNameMap[key] = fullPath;
                }
            }

            if (!found && forPlugin != "") {
                cerr << "Vamp::HostExt::PluginLoader: Plugin \""
                     << identifier << "\" not found in library \""
                     << fullPath << "\"" << endl;
            }
            
            unloadLibrary(handle);
        }
    }

    if (forPlugin == "") m_allPluginsEnumerated = true;
}

PluginLoader::PluginKey
PluginLoader::Impl::composePluginKey(string libraryName, string identifier)
{
    string basename = libraryName;

    string::size_type li = basename.rfind('/');
    if (li != string::npos) basename = basename.substr(li + 1);

    li = basename.find('.');
    if (li != string::npos) basename = basename.substr(0, li);

    for (size_t i = 0; i < basename.length(); ++i) {
        basename[i] = tolower(basename[i]);
    }

    return basename + ":" + identifier;
}

bool
PluginLoader::Impl::decomposePluginKey(PluginKey key,
                                       string &libraryName,
                                       string &identifier)
{
    string::size_type ki = key.find(':');
    if (ki == string::npos) {
        return false;
    }

    libraryName = key.substr(0, ki);
    identifier = key.substr(ki + 1);
    return true;
}

PluginLoader::PluginCategoryHierarchy
PluginLoader::Impl::getPluginCategory(PluginKey plugin)
{
    if (m_taxonomy.empty()) generateTaxonomy();
    if (m_taxonomy.find(plugin) == m_taxonomy.end()) {
        return PluginCategoryHierarchy();
    }
    return m_taxonomy[plugin];
}

string
PluginLoader::Impl::getLibraryPathForPlugin(PluginKey plugin)
{
    if (m_pluginLibraryNameMap.find(plugin) == m_pluginLibraryNameMap.end()) {
        if (m_allPluginsEnumerated) return "";
        enumeratePlugins(plugin);
    }
    if (m_pluginLibraryNameMap.find(plugin) == m_pluginLibraryNameMap.end()) {
        return "";
    }
    return m_pluginLibraryNameMap[plugin];
}    

Plugin *
PluginLoader::Impl::loadPlugin(PluginKey key,
                               float inputSampleRate, int adapterFlags)
{
    string libname, identifier;
    if (!decomposePluginKey(key, libname, identifier)) {
        std::cerr << "Vamp::HostExt::PluginLoader: Invalid plugin key \""
                  << key << "\" in loadPlugin" << std::endl;
        return 0;
    }
        
    string fullPath = getLibraryPathForPlugin(key);
    if (fullPath == "") {
        std::cerr << "Vamp::HostExt::PluginLoader: No library found in Vamp path for plugin \"" << key << "\"" << std::endl;
        return 0;
    }
    
    void *handle = loadLibrary(fullPath);
    if (!handle) return 0;
    
    VampGetPluginDescriptorFunction fn =
        (VampGetPluginDescriptorFunction)lookupInLibrary
        (handle, "vampGetPluginDescriptor");

    if (!fn) {
        cerr << "Vamp::HostExt::PluginLoader: No vampGetPluginDescriptor function found in library \""
             << fullPath << "\"" << endl;
        unloadLibrary(handle);
        return 0;
    }

    int index = 0;
    const VampPluginDescriptor *descriptor = 0;

    while ((descriptor = fn(VAMP_API_VERSION, index))) {

        if (string(descriptor->identifier) == identifier) {

            Vamp::PluginHostAdapter *plugin =
                new Vamp::PluginHostAdapter(descriptor, inputSampleRate);

            Plugin *adapter = new PluginDeletionNotifyAdapter(plugin, this);

            m_pluginLibraryHandleMap[adapter] = handle;

            if (adapterFlags & ADAPT_INPUT_DOMAIN) {
                if (adapter->getInputDomain() == Plugin::FrequencyDomain) {
                    adapter = new PluginInputDomainAdapter(adapter);
                }
            }

            if (adapterFlags & ADAPT_BUFFER_SIZE) {
                adapter = new PluginBufferingAdapter(adapter);
            }

            if (adapterFlags & ADAPT_CHANNEL_COUNT) {
                adapter = new PluginChannelAdapter(adapter);
            }

            return adapter;
        }

        ++index;
    }

    cerr << "Vamp::HostExt::PluginLoader: Plugin \""
         << identifier << "\" not found in library \""
         << fullPath << "\"" << endl;

    return 0;
}

void
PluginLoader::Impl::generateTaxonomy()
{
//    cerr << "PluginLoader::Impl::generateTaxonomy" << endl;

    vector<string> path = PluginHostAdapter::getPluginPath();
    string libfragment = "/lib/";
    vector<string> catpath;

    string suffix = "cat";

    for (vector<string>::iterator i = path.begin();
         i != path.end(); ++i) {

        // It doesn't matter that we're using literal forward-slash in
        // this bit, as it's only relevant if the path contains
        // "/lib/", which is only meaningful and only plausible on
        // systems with forward-slash delimiters
        
        string dir = *i;
        string::size_type li = dir.find(libfragment);

        if (li != string::npos) {
            catpath.push_back
                (dir.substr(0, li)
                 + "/share/"
                 + dir.substr(li + libfragment.length()));
        }

        catpath.push_back(dir);
    }

    char buffer[1024];

    for (vector<string>::iterator i = catpath.begin();
         i != catpath.end(); ++i) {
        
        vector<string> files = listFiles(*i, suffix);

        for (vector<string>::iterator fi = files.begin();
             fi != files.end(); ++fi) {

            string filepath = splicePath(*i, *fi);
            ifstream is(filepath.c_str(), ifstream::in | ifstream::binary);

            if (is.fail()) {
//                cerr << "failed to open: " << filepath << endl;
                continue;
            }

//            cerr << "opened: " << filepath << endl;

            while (!!is.getline(buffer, 1024)) {

                string line(buffer);

//                cerr << "line = " << line << endl;

                string::size_type di = line.find("::");
                if (di == string::npos) continue;

                string id = line.substr(0, di);
                string encodedCat = line.substr(di + 2);

                if (id.substr(0, 5) != "vamp:") continue;
                id = id.substr(5);

                while (encodedCat.length() >= 1 &&
                       encodedCat[encodedCat.length()-1] == '\r') {
                    encodedCat = encodedCat.substr(0, encodedCat.length()-1);
                }

//                cerr << "id = " << id << ", cat = " << encodedCat << endl;

                PluginCategoryHierarchy category;
                string::size_type ai;
                while ((ai = encodedCat.find(" > ")) != string::npos) {
                    category.push_back(encodedCat.substr(0, ai));
                    encodedCat = encodedCat.substr(ai + 3);
                }
                if (encodedCat != "") category.push_back(encodedCat);

                m_taxonomy[id] = category;
            }
        }
    }
}    

void *
PluginLoader::Impl::loadLibrary(string path)
{
    void *handle = 0;
#ifdef _WIN32
#ifdef UNICODE
    int len = path.length() + 1; // cannot be more wchars than length in bytes of utf8 string
    wchar_t *buffer = new wchar_t[len];
    int rv = MultiByteToWideChar(CP_UTF8, 0, path.c_str(), len, buffer, len);
    if (rv <= 0) {
        cerr << "Vamp::HostExt::PluginLoader: Unable to convert library path \""
             << path << "\" to wide characters " << endl;
        delete[] buffer;
        return handle;
    }
    handle = LoadLibrary(buffer);
    delete[] buffer;
#else
    handle = LoadLibrary(path.c_str());
#endif
    if (!handle) {
        cerr << "Vamp::HostExt::PluginLoader: Unable to load library \""
             << path << "\"" << endl;
    }
#else
    handle = dlopen(path.c_str(), RTLD_LAZY | RTLD_LOCAL);
    if (!handle) {
        cerr << "Vamp::HostExt::PluginLoader: Unable to load library \""
             << path << "\": " << dlerror() << endl;
    }
#endif
    return handle;
}

void
PluginLoader::Impl::unloadLibrary(void *handle)
{
#ifdef _WIN32
    FreeLibrary((HINSTANCE)handle);
#else
    dlclose(handle);
#endif
}

void *
PluginLoader::Impl::lookupInLibrary(void *handle, const char *symbol)
{
#ifdef _WIN32
    return (void *)GetProcAddress((HINSTANCE)handle, symbol);
#else
    return (void *)dlsym(handle, symbol);
#endif
}

string
PluginLoader::Impl::splicePath(string a, string b)
{
#ifdef _WIN32
    return a + "\\" + b;
#else
    return a + "/" + b;
#endif
}

vector<string>
PluginLoader::Impl::listFiles(string dir, string extension)
{
    vector<string> files;

#ifdef _WIN32
    string expression = dir + "\\*." + extension;
#ifdef UNICODE
    int len = expression.length() + 1; // cannot be more wchars than length in bytes of utf8 string
    wchar_t *buffer = new wchar_t[len];
    int rv = MultiByteToWideChar(CP_UTF8, 0, expression.c_str(), len, buffer, len);
    if (rv <= 0) {
        cerr << "Vamp::HostExt::PluginLoader: Unable to convert wildcard path \""
             << expression << "\" to wide characters" << endl;
        delete[] buffer;
        return files;
    }
    WIN32_FIND_DATA data;
    HANDLE fh = FindFirstFile(buffer, &data);
    if (fh == INVALID_HANDLE_VALUE) {
        delete[] buffer;
        return files;
    }

    bool ok = true;
    while (ok) {
        wchar_t *fn = data.cFileName;
        int wlen = wcslen(fn) + 1;
        int maxlen = wlen * 6;
        char *conv = new char[maxlen];
        int rv = WideCharToMultiByte(CP_UTF8, 0, fn, wlen, conv, maxlen, 0, 0);
        if (rv > 0) {
            files.push_back(conv);
        }
        delete[] conv;
        ok = FindNextFile(fh, &data);
    }

    FindClose(fh);
    delete[] buffer;
#else
    WIN32_FIND_DATA data;
    HANDLE fh = FindFirstFile(expression.c_str(), &data);
    if (fh == INVALID_HANDLE_VALUE) return files;

    bool ok = true;
    while (ok) {
        files.push_back(data.cFileName);
        ok = FindNextFile(fh, &data);
    }

    FindClose(fh);
#endif
#else

    size_t extlen = extension.length();
    DIR *d = opendir(dir.c_str());
    if (!d) return files;
            
    struct dirent *e = 0;
    while ((e = readdir(d))) {
 
        if (!e->d_name) continue;
       
        size_t len = strlen(e->d_name);
        if (len < extlen + 2 ||
            e->d_name + len - extlen - 1 != "." + extension) {
            continue;
        }

        files.push_back(e->d_name);
    }

    closedir(d);
#endif

    return files;
}

void
PluginLoader::Impl::pluginDeleted(PluginDeletionNotifyAdapter *adapter)
{
    void *handle = m_pluginLibraryHandleMap[adapter];
    if (handle) unloadLibrary(handle);
    m_pluginLibraryHandleMap.erase(adapter);
}

PluginLoader::Impl::PluginDeletionNotifyAdapter::PluginDeletionNotifyAdapter(Plugin *plugin,
                                                                             Impl *loader) :
    PluginWrapper(plugin),
    m_loader(loader)
{
}

PluginLoader::Impl::PluginDeletionNotifyAdapter::~PluginDeletionNotifyAdapter()
{
    // We need to delete the plugin before calling pluginDeleted, as
    // the delete call may require calling through to the descriptor
    // (for e.g. cleanup) but pluginDeleted may unload the required
    // library for the call.  To prevent a double deletion when our
    // parent's destructor runs (after this one), be sure to set
    // m_plugin to 0 after deletion.
    delete m_plugin;
    m_plugin = 0;

    if (m_loader) m_loader->pluginDeleted(this);
}

}

}

_VAMP_SDK_HOSTSPACE_END(PluginLoader.cpp)

