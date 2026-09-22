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

#include <vamp-hostsdk/PluginHostAdapter.h>

#include "Files.h"

#include <cctype> // tolower

#include <cstring>

#ifdef _WIN32

#include <windows.h>
#include <tchar.h>
#define PLUGIN_SUFFIX "dll"

#else /* ! _WIN32 */

#include <cstdlib>
#include <dirent.h>
#include <dlfcn.h>

#ifdef __APPLE__
#define PLUGIN_SUFFIX "dylib"
#else /* ! __APPLE__ */
#define PLUGIN_SUFFIX "so"
#endif /* ! __APPLE__ */

#endif /* ! _WIN32 */

using namespace std;

vector<string>
Files::listLibraryFiles()
{
    return listLibraryFilesMatching(Filter());
}

vector<string>
Files::listLibraryFilesMatching(Filter filter)
{
    vector<string> path = Vamp::PluginHostAdapter::getPluginPath();
    vector<string> libraryFiles;

    // we match case-insensitively, but only with ascii range
    // characters (input strings are expected to be utf-8)
    vector<string> libraryNames;
    for (int j = 0; j < int(filter.libraryNames.size()); ++j) {
        string n = filter.libraryNames[j];
        for (size_t i = 0; i < n.length(); ++i) {
            if (!(n[i] & 0x80)) {
                n[i] = char(tolower(n[i]));
            }
        }
        libraryNames.push_back(n);
    }

    for (size_t i = 0; i < path.size(); ++i) {
        
        vector<string> files = listFiles(path[i], PLUGIN_SUFFIX);

        for (vector<string>::iterator fi = files.begin();
             fi != files.end(); ++fi) {

            // we match case-insensitively, but only with ascii range
            // characters (this string is expected to be utf-8)
            string cleaned = *fi;
            for (size_t j = 0; j < cleaned.length(); ++j) {
                if (!(cleaned[j] & 0x80)) {
                    cleaned[j] = char(tolower(cleaned[j]));
                }
            }

            // libraryName should be lacking an extension, as it is
            // supposed to have come from the plugin key
            string::size_type pi = cleaned.find('.');
            if (pi != string::npos) {
                cleaned = cleaned.substr(0, pi);
            }
            
            bool matched = false;

            switch (filter.type) {

            case Filter::All:
                matched = true;
                break;

            case Filter::Matching:
                for (int j = 0; j < int(libraryNames.size()); ++j) {
                    if (cleaned == libraryNames[j]) {
                        matched = true;
                        break;
                    }
                }
                break;

            case Filter::NotMatching:
                matched = true;
                for (int j = 0; j < int(libraryNames.size()); ++j) {
                    if (cleaned == libraryNames[j]) {
                        matched = false;
                        break;
                    }
                }
                break;
            }

            if (!matched) continue;
            
            string fullPath = path[i];
            fullPath = splicePath(fullPath, *fi);
	    libraryFiles.push_back(fullPath);
	}
    }

    return libraryFiles;
}

void *
Files::loadLibrary(string path)
{
    void *handle = 0;
#ifdef _WIN32
#ifdef UNICODE
    int wlen = MultiByteToWideChar(CP_UTF8, 0, path.c_str(), path.length(), 0, 0);
    if (wlen < 0) {
        cerr << "Vamp::HostExt: Unable to convert library path \""
             << path << "\" to wide characters " << endl;
        return handle;
    }
    wchar_t *buffer = new wchar_t[wlen+1];
    (void)MultiByteToWideChar(CP_UTF8, 0, path.c_str(), path.length(), buffer, wlen);
    buffer[wlen] = L'\0';
    handle = LoadLibrary(buffer);
    delete[] buffer;
#else
    handle = LoadLibrary(path.c_str());
#endif
    if (!handle) {
        cerr << "Vamp::HostExt: Unable to load library \""
             << path << "\": error code " << GetLastError() << endl;
    }
#else
    handle = dlopen(path.c_str(), RTLD_LAZY | RTLD_LOCAL);
    if (!handle) {
        cerr << "Vamp::HostExt: Unable to load library \""
             << path << "\": " << dlerror() << endl;
    }
#endif
    return handle;
}

void
Files::unloadLibrary(void *handle)
{
#ifdef _WIN32
    FreeLibrary((HINSTANCE)handle);
#else
    dlclose(handle);
#endif
}

void *
Files::lookupInLibrary(void *handle, const char *symbol)
{
#ifdef _WIN32
    return (void *)GetProcAddress((HINSTANCE)handle, symbol);
#else
    return (void *)dlsym(handle, symbol);
#endif
}

string
Files::lcBasename(string path)
{
    string basename(path);
	
    string::size_type li = basename.rfind('/');
    if (li != string::npos) basename = basename.substr(li + 1);

#ifdef _WIN32
    li = basename.rfind('\\');
    if (li != string::npos) basename = basename.substr(li + 1);
#endif

    li = basename.find('.');
    if (li != string::npos) basename = basename.substr(0, li);

    // case-insensitive, but only with ascii range characters (this
    // string is expected to be utf-8)
    for (size_t i = 0; i < basename.length(); ++i) {
        if (!(basename[i] & 0x80)) {
            basename[i] = char(tolower(basename[i]));
        }
    }

    return basename;
}

string
Files::splicePath(string a, string b)
{
#ifdef _WIN32
    return a + "\\" + b;
#else
    return a + "/" + b;
#endif
}

vector<string>
Files::listFiles(string dir, string extension)
{
    vector<string> files;

#ifdef _WIN32
    string expression = dir + "\\*." + extension;
#ifdef UNICODE
    int wlen = MultiByteToWideChar(CP_UTF8, 0, expression.c_str(), expression.length(), 0, 0);
    if (wlen < 0) {
        cerr << "Vamp::HostExt: Unable to convert wildcard path \""
             << expression << "\" to wide characters" << endl;
        return files;
    }
    wchar_t *buffer = new wchar_t[wlen+1];
    (void)MultiByteToWideChar(CP_UTF8, 0, expression.c_str(), expression.length(), buffer, wlen);
    buffer[wlen] = L'\0';
    WIN32_FIND_DATA data;
    HANDLE fh = FindFirstFile(buffer, &data);
    if (fh == INVALID_HANDLE_VALUE) {
        delete[] buffer;
        return files;
    }

    bool ok = true;
    while (ok) {
        wchar_t *fn = data.cFileName;
        int wlen = wcslen(fn);
        int len = WideCharToMultiByte(CP_UTF8, 0, fn, wlen, 0, 0, 0, 0);
        if (len < 0) {
            cerr << "Vamp::HostExt: Unable to convert wide char filename to utf-8" << endl;
            break;
        }
        char *conv = new char[len+1];
        (void)WideCharToMultiByte(CP_UTF8, 0, fn, wlen, conv, len, 0, 0);
        conv[len] = '\0';
        if (len > 0) {
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

bool
Files::isNonNative32Bit()
{
    // Return true if we are running on a system for which we should
    // use the VAMP_PATH_32 variable instead of VAMP_PATH. This will
    // be the case if we are a 32-bit executable but the OS is
    // natively 64-bit.
    //
    // This currently works only on Windows; other operating systems
    // will use VAMP_PATH always.
    
    if (sizeof(void *) == 8) {
        return false;
    }

#ifdef _WIN32
    BOOL wow64 = FALSE;
    BOOL (WINAPI *fnIsWow64Process)(HANDLE, PBOOL) =
        (BOOL (WINAPI *)(HANDLE, PBOOL)) GetProcAddress
        (GetModuleHandle(TEXT("kernel32")), "IsWow64Process");
    if (fnIsWow64Process) {
        if (fnIsWow64Process(GetCurrentProcess(), &wow64)) {
            if (wow64) {
                return true;
            } else {
                return false;
            }
        } else {
            cerr << "Vamp::HostExt: Unable to query process architecture"
                 << endl;
            return false;
        }
    } else {
        cerr << "Vamp::HostExt: Unable to query process architecture: "
             << "Function not available" << endl;
        return false;
    }
#endif

    return false;
}

bool
Files::getEnvUtf8(std::string variable, std::string &value)
{
    value = "";
    
#ifdef _WIN32
    int wvarlen = MultiByteToWideChar(CP_UTF8, 0,
                                      variable.c_str(), int(variable.length()),
                                      0, 0);
    if (wvarlen < 0) {
        cerr << "Vamp::HostExt: Unable to convert environment variable name "
             << variable << " to wide characters" << endl;
        return false;
    }
    
    wchar_t *wvarbuf = new wchar_t[wvarlen + 1];
    (void)MultiByteToWideChar(CP_UTF8, 0,
                              variable.c_str(), int(variable.length()),
                              wvarbuf, wvarlen);
    wvarbuf[wvarlen] = L'\0';
    
    wchar_t *wvalue = _wgetenv(wvarbuf);

    delete[] wvarbuf;

    if (!wvalue) {
        return false;
    }

    int wvallen = int(wcslen(wvalue));
    int vallen = WideCharToMultiByte(CP_UTF8, 0,
                                     wvalue, wvallen,
                                     0, 0, 0, 0);
    if (vallen < 0) {
        cerr << "Vamp::HostExt: Unable to convert environment value to UTF-8"
             << endl;
        return false;
    }

    char *val = new char[vallen + 1];
    (void)WideCharToMultiByte(CP_UTF8, 0,
                              wvalue, wvallen,
                              val, vallen, 0, 0);
    val[vallen] = '\0';

    value = val;

    delete[] val;
    return true;

#else

    char *val = getenv(variable.c_str());
    if (!val) {
        return false;
    }

    value = val;
    return true;
    
#endif
}
