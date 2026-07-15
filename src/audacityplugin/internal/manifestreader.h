/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <string>
#include <string_view>
#include <variant>

namespace au::audacityplugin {
enum class Platform {
    WindowsX64,
    WindowsArm64,
    MacX64,
    MacArm64,
    LinuxX64,
    LinuxArm64,
};

struct Manifest {
    std::string pluginId;
    std::string displayName;
    std::string vendor;
    std::string version;
    std::string entryPath;
};

struct ManifestError {
    std::string field;
    std::string message;
};

using ManifestReadResult = std::variant<Manifest, ManifestError>;

[[nodiscard]] ManifestReadResult readManifest(std::string_view json, Platform platform);
} // namespace au::audacityplugin
