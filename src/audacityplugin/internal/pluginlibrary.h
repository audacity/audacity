/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <filesystem>
#include <string>

#define AUP_NO_ENTRY_DECL
#define AUP_EXPORT
#include "audacity_plugin_v0.h"
#undef AUP_EXPORT
#undef AUP_NO_ENTRY_DECL

namespace au::audacityplugin {
class PluginLibrary
{
public:
    using EntryPoint = const aup_plugin_v0 * (AUP_CALL*)();

    PluginLibrary() = default;
    ~PluginLibrary();
    PluginLibrary(PluginLibrary&& other) noexcept;
    PluginLibrary(const PluginLibrary&) = delete;
    PluginLibrary& operator=(const PluginLibrary&) = delete;

    static PluginLibrary load(const std::filesystem::path& bundlePath, const std::string& relativePath, std::string& error);

    EntryPoint entryPoint(std::string& error) const;

private:
    explicit PluginLibrary(void* handle)
        : m_handle(handle) {}

    void* m_handle = nullptr;
};
} // namespace au::audacityplugin
