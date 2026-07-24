/*
 * Audacity: A Digital Audio Editor
 */
#include "pluginlibrary.h"

#if defined(_WIN32)
#include <windows.h>
#else
#include <dlfcn.h>
#endif

namespace au::audacityplugin {
namespace {
bool isInside(const std::filesystem::path& parent, const std::filesystem::path& child)
{
    auto childPart = child.begin();
    for (auto parentPart = parent.begin(); parentPart != parent.end(); ++parentPart, ++childPart) {
        if (childPart == child.end() || *parentPart != *childPart) {
            return false;
        }
    }
    return true;
}

bool resolveLibraryPath(const std::filesystem::path& bundlePath,
                        const std::string& relativePath,
                        std::filesystem::path& libraryPath,
                        std::string& error)
{
    try {
        const auto relative = std::filesystem::u8path(relativePath);
        libraryPath = std::filesystem::canonical(bundlePath / relative);
        if (!isInside(bundlePath, libraryPath)) {
            error = "entry path escapes the bundle";
            return false;
        }
        if (!std::filesystem::is_regular_file(libraryPath)) {
            error = "entry is not a regular file";
            return false;
        }
        return true;
    } catch (const std::exception& exception) {
        error = exception.what();
    } catch (...) {
        error = "unknown entry path validation failure";
    }
    return false;
}
} // namespace

PluginLibrary PluginLibrary::load(const std::filesystem::path& bundlePath,
                                  const std::string& relativePath,
                                  std::string& error)
{
    std::filesystem::path path;
    if (!resolveLibraryPath(bundlePath, relativePath, path, error)) {
        return {};
    }

#if defined(_WIN32)
    HMODULE module = LoadLibraryExW(path.c_str(), nullptr,
                                    LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR
                                    | LOAD_LIBRARY_SEARCH_DEFAULT_DIRS);
    if (!module) {
        error = "LoadLibraryExW failed: " + std::to_string(GetLastError());
        return {};
    }
    return PluginLibrary(reinterpret_cast<void*>(module));
#else
    void* module = ::dlopen(path.c_str(), RTLD_NOW | RTLD_LOCAL);
    if (!module) {
        const char* detail = ::dlerror();
        error = detail ? detail : "dlopen failed";
        return {};
    }
    return PluginLibrary(module);
#endif
}

PluginLibrary::~PluginLibrary()
{
    if (!m_handle) {
        return;
    }
#if defined(_WIN32)
    FreeLibrary(reinterpret_cast<HMODULE>(m_handle));
#else
    ::dlclose(m_handle);
#endif
}

PluginLibrary::PluginLibrary(PluginLibrary&& other) noexcept
    : m_handle(other.m_handle)
{
    other.m_handle = nullptr;
}

PluginLibrary::EntryPoint PluginLibrary::entryPoint(std::string& error) const
{
    if (!m_handle) {
        return nullptr;
    }
#if defined(_WIN32)
    const auto symbol = GetProcAddress(
        reinterpret_cast<HMODULE>(m_handle), "audacity_plugin_entry_v0");
    if (!symbol) {
        error = "audacity_plugin_entry_v0 is missing";
        return nullptr;
    }
    return reinterpret_cast<EntryPoint>(symbol);
#else
    ::dlerror();
    void* symbol = ::dlsym(m_handle, "audacity_plugin_entry_v0");
    if (const char* detail = ::dlerror()) {
        error = detail;
        return nullptr;
    }
    return reinterpret_cast<EntryPoint>(symbol);
#endif
}
} // namespace au::audacityplugin
