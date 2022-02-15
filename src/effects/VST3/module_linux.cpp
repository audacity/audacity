//-----------------------------------------------------------------------------
// Project     : VST SDK
//
// Category    : Helpers
// Filename    : public.sdk/source/vst/hosting/module_linux.cpp
// Created by  : Steinberg, 08/2016
// Description : hosting module classes (linux implementation)
//
//-----------------------------------------------------------------------------
// LICENSE
// (c) 2021, Steinberg Media Technologies GmbH, All Rights Reserved
//-----------------------------------------------------------------------------
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
//   * Redistributions of source code must retain the above copyright notice,
//     this list of conditions and the following disclaimer.
//   * Redistributions in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//   * Neither the name of the Steinberg Media Technologies nor the names of its
//     contributors may be used to endorse or promote products derived from this
//     software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
// OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.
//-----------------------------------------------------------------------------

#include "public.sdk/source/vst/hosting/module.h"
#include "public.sdk/source/vst/utility/optional.h"
#include "public.sdk/source/vst/utility/stringconvert.h"
#include <algorithm>
#include <dlfcn.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>

#if (__cplusplus >= 201707L)
#if __has_include(<filesystem>)
#define USE_EXPERIMENTAL_FS 0
#elif __has_include(<experimental/filesystem>)
#define USE_EXPERIMENTAL_FS 1
#endif
#else
#define USE_EXPERIMENTAL_FS 1
#endif

#if USE_EXPERIMENTAL_FS == 1
#include <experimental/filesystem>
namespace filesystem = std::experimental::filesystem;
#else
#include <filesystem>
namespace filesystem = std::filesystem;
#endif

//------------------------------------------------------------------------
extern "C" {
using ModuleEntryFunc = bool (PLUGIN_API*) (void*);
using ModuleExitFunc = bool (PLUGIN_API*) ();
}

//------------------------------------------------------------------------
namespace VST3 {
namespace Hosting {

using Path = filesystem::path;

//------------------------------------------------------------------------
namespace {

//------------------------------------------------------------------------
Optional<std::string> getCurrentMachineName ()
{
	struct utsname unameData;

	int res = uname (&unameData);
	if (res != 0)
		return {};

	return {unameData.machine};
}

//------------------------------------------------------------------------
Optional<Path> getApplicationPath ()
{
	std::string appPath = "";

	pid_t pid = getpid ();
	char buf[10];
	sprintf (buf, "%d", pid);
	std::string _link = "/proc/";
	_link.append (buf);
	_link.append ("/exe");
	char proc[1024];
	int ch = readlink (_link.c_str (), proc, 1024);
	if (ch == -1)
		return {};

	proc[ch] = 0;
	appPath = proc;
	std::string::size_type t = appPath.find_last_of ("/");
	appPath = appPath.substr (0, t);

	return Path {appPath};
}

//------------------------------------------------------------------------
class LinuxModule : public Module
{
public:
	template <typename T>
	T getFunctionPointer (const char* name)
	{
		return reinterpret_cast<T> (dlsym (mModule, name));
	}

	~LinuxModule () override
	{
		factory = PluginFactory (nullptr);

		if (mModule)
		{
			if (auto moduleExit = getFunctionPointer<ModuleExitFunc> ("ModuleExit"))
				moduleExit ();

			dlclose (mModule);
		}
	}

	static Optional<Path> getSOPath (const std::string& inPath)
	{
		Path modulePath {inPath};
		if (!filesystem::is_directory (modulePath))
			return {};

		auto stem = modulePath.stem ();

		modulePath /= "Contents";
		if (!filesystem::is_directory (modulePath))
			return {};

		// use the Machine Hardware Name (from uname cmd-line) as prefix for "-linux"
		auto machine = getCurrentMachineName ();
		if (!machine)
			return {};

		modulePath /= *machine + "-linux";
		if (!filesystem::is_directory (modulePath))
			return {};

		stem.replace_extension (".so");
		modulePath /= stem;
		return Optional<Path> (std::move (modulePath));
	}

	bool load (const std::string& inPath, std::string& errorDescription) override
	{
		auto modulePath = getSOPath (inPath);
		if (!modulePath)
		{
			errorDescription = inPath + " is not a module directory.";
			return false;
		}

		mModule = dlopen (reinterpret_cast<const char*> (modulePath->generic_string ().data ()),
		                  RTLD_LAZY);
		if (!mModule)
		{
			errorDescription = "dlopen failed.\n";
			errorDescription += dlerror ();
			return false;
		}
		// ModuleEntry is mandatory
		auto moduleEntry = getFunctionPointer<ModuleEntryFunc> ("ModuleEntry");
		if (!moduleEntry)
		{
			errorDescription =
			    "The shared library does not export the required 'ModuleEntry' function";
			return false;
		}
		// ModuleExit is mandatory
		auto moduleExit = getFunctionPointer<ModuleExitFunc> ("ModuleExit");
		if (!moduleExit)
		{
			errorDescription =
			    "The shared library does not export the required 'ModuleExit' function";
			return false;
		}
		auto factoryProc = getFunctionPointer<GetFactoryProc> ("GetPluginFactory");
		if (!factoryProc)
		{
			errorDescription =
			    "The shared library does not export the required 'GetPluginFactory' function";
			return false;
		}

		if (!moduleEntry (mModule))
		{
			errorDescription = "Calling 'ModuleEntry' failed";
			return false;
		}
		auto f = Steinberg::FUnknownPtr<Steinberg::IPluginFactory> (owned (factoryProc ()));
		if (!f)
		{
			errorDescription = "Calling 'GetPluginFactory' returned nullptr";
			return false;
		}
		factory = PluginFactory (f);
		return true;
	}

	void* mModule {nullptr};
};

//------------------------------------------------------------------------
void findFilesWithExt (const std::string& path, const std::string& ext, Module::PathList& pathList,
                       bool recursive = true)
{
	try
	{
		for (auto& p : filesystem::directory_iterator (path))
		{
			if (p.path ().extension () == ext)
			{
				pathList.push_back (p.path ().generic_string ());
			}
			else if (recursive && p.status ().type () == filesystem::file_type::directory)
			{
				findFilesWithExt (p.path (), ext, pathList);
			}
		}
	}
	catch (...)
	{
	}
}

//------------------------------------------------------------------------
void findModules (const std::string& path, Module::PathList& pathList)
{
	findFilesWithExt (path, ".vst3", pathList);
}

//------------------------------------------------------------------------
} // anonymous

//------------------------------------------------------------------------
Module::Ptr Module::create (const std::string& path, std::string& errorDescription)
{
	auto _module = std::make_shared<LinuxModule> ();
	if (_module->load (path, errorDescription))
	{
		auto it = std::find_if (path.rbegin (), path.rend (),
		                        [] (const std::string::value_type& c) { return c == '/'; });
		_module->path = path;
		if (it != path.rend ())
			_module->name = {it.base (), path.end ()};
		return _module;
	}
	return nullptr;
}

//------------------------------------------------------------------------
Module::PathList Module::getModulePaths ()
{
	/* VST3 component locations on linux :
	 * User privately installed	: $HOME/.vst3/
	 * Distribution installed	: /usr/lib/vst3/
	 * Locally installed		: /usr/local/lib/vst3/
	 * Application				: /$APPFOLDER/vst3/
	 */

	const auto systemPaths = {"/usr/lib/vst3/", "/usr/local/lib/vst3/"};

	PathList list;
	if (auto homeDir = getenv ("HOME"))
	{
		filesystem::path homePath (homeDir);
		homePath /= ".vst3";
		findModules (homePath.generic_string (), list);
	}
	for (auto path : systemPaths)
		findModules (path, list);

	// application level
	auto appPath = getApplicationPath ();
	if (appPath)
	{
		*appPath /= "vst3";
		findModules (appPath->generic_string (), list);
	}

	return list;
}

//------------------------------------------------------------------------
Module::SnapshotList Module::getSnapshots (const std::string& modulePath)
{
	SnapshotList result;
	filesystem::path path (modulePath);
	path /= "Contents";
	path /= "Resources";
	path /= "Snapshots";
	PathList pngList;
	findFilesWithExt (path, ".png", pngList, false);
	for (auto& png : pngList)
	{
		filesystem::path p (png);
		auto filename = p.filename ().generic_string ();
		auto uid = Snapshot::decodeUID (filename);
		if (!uid)
			continue;
		auto scaleFactor = 1.;
		if (auto decodedScaleFactor = Snapshot::decodeScaleFactor (filename))
			scaleFactor = *decodedScaleFactor;

		Module::Snapshot::ImageDesc desc;
		desc.scaleFactor = scaleFactor;
		desc.path = std::move (png);
		bool found = false;
		for (auto& entry : result)
		{
			if (entry.uid != *uid)
				continue;
			found = true;
			entry.images.emplace_back (std::move (desc));
			break;
		}
		if (found)
			continue;
		Module::Snapshot snapshot;
		snapshot.uid = *uid;
		snapshot.images.emplace_back (std::move (desc));
		result.emplace_back (std::move (snapshot));
	}
	return result;
}

//------------------------------------------------------------------------
} // Hosting
} // VST3
