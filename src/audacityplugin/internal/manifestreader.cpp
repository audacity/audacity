/*
 * Audacity: A Digital Audio Editor
 */
#include "manifestreader.h"

#include "abi.h"

#include "global/serialization/json.h"

#include <algorithm>
#include <string>
#include <string_view>

namespace au::audacityplugin {
namespace {
std::string_view platformKey(Platform platform)
{
    switch (platform) {
    case Platform::WindowsX64: return "windows-x86_64";
    case Platform::WindowsArm64: return "windows-arm64";
    case Platform::MacX64: return "macos-x86_64";
    case Platform::MacArm64: return "macos-arm64";
    case Platform::LinuxX64: return "linux-x86_64";
    case Platform::LinuxArm64: return "linux-arm64";
    }
    return {};
}

bool isLower(char value)
{
    return value >= 'a' && value <= 'z';
}

bool isDigit(char value)
{
    return value >= '0' && value <= '9';
}

bool isReverseDnsId(std::string_view id)
{
    if (id.size() < 3 || id.size() > 253) {
        return false;
    }

    size_t labelCount = 0;
    for (size_t begin = 0; begin <= id.size();) {
        const auto separator = id.find('.', begin);
        const auto end = separator == std::string_view::npos
                         ? id.size() : separator;
        const auto label = id.substr(begin, end - begin);
        if (label.empty() || label.size() > 63 || !isLower(label.front())
            || !(isLower(label.back()) || isDigit(label.back()))
            || !std::all_of(label.begin(), label.end(), [](char value) {
            return isLower(value) || isDigit(value) || value == '-';
        })) {
            return false;
        }
        ++labelCount;
        if (separator == std::string_view::npos) {
            break;
        }
        begin = separator + 1;
    }
    return labelCount >= 2;
}

bool hasVersion(const muse::JsonObject& root, const char* key, int expected)
{
    const auto value = root.value(key);
    return value.isNumber() && value.toDouble() == expected;
}

bool readString(const muse::JsonObject& root, const std::string& key,
                std::string& result)
{
    const auto value = root.value(key);
    if (!value.isString()) {
        return false;
    }
    result = value.toStdString();
    return !result.empty();
}
} // namespace

ManifestReadResult readManifest(std::string_view json, Platform platform)
{
    const auto entryKey = platformKey(platform);
    if (entryKey.empty()) {
        return ManifestError { {}, "Unsupported host platform" };
    }

    std::string parseError;
    const char* data = json.empty() ? "" : json.data();
    const auto document = muse::JsonDocument::fromJson(
        muse::ByteArray::fromRawData(data, json.size()), &parseError);
    if (!parseError.empty()) {
        return ManifestError { {}, parseError };
    }
    if (!document.isObject()) {
        return ManifestError { {}, "Manifest root must be a JSON object" };
    }

    const auto root = document.rootObject();
    if (!hasVersion(root, "schema_version", 1)) {
        return ManifestError { "/schema_version",
                               "Only manifest schema_version 1 is supported" };
    }

    const auto apiValue = root.value("api");
    if (!apiValue.isObject()) {
        return ManifestError { "/api", "api must be a JSON object" };
    }
    const auto api = apiValue.toObject();
    if (!hasVersion(api, "major", AUP_API_MAJOR)) {
        return ManifestError { "/api/major", "Unsupported Plugin API major version" };
    }
    if (!hasVersion(api, "revision", AUP_API_REVISION)) {
        return ManifestError { "/api/revision", "Unsupported Plugin API revision" };
    }

    Manifest manifest;
    if (!readString(root, "id", manifest.pluginId)
        || !isReverseDnsId(manifest.pluginId)) {
        return ManifestError { "/id",
                               "Plugin id must be a lowercase reverse-DNS identifier" };
    }
    if (!readString(root, "display_name", manifest.displayName)) {
        return ManifestError { "/display_name", "Plugin display name is missing" };
    }
    if (!readString(root, "vendor", manifest.vendor)) {
        return ManifestError { "/vendor", "Plugin vendor is missing" };
    }
    if (!readString(root, "version", manifest.version)) {
        return ManifestError { "/version", "Plugin version is missing" };
    }

    const auto entryValue = root.value("entry");
    if (!entryValue.isObject()) {
        return ManifestError { "/entry", "entry must be a JSON object" };
    }
    const auto entries = entryValue.toObject();
    if (!readString(entries, std::string { entryKey }, manifest.entryPath)) {
        return ManifestError { "/entry/" + std::string { entryKey },
                               "Manifest has no entry for the current platform" };
    }

    return manifest;
}
} // namespace au::audacityplugin
