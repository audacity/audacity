/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "internal/manifestreader.h"

#include <string>
#include <string_view>
#include <variant>

namespace au::audacityplugin {
namespace {
std::string validManifest()
{
    return
        R"({
        "schema_version": 1,
        "api": { "major": 0, "revision": 1 },
        "id": "org.audacityteam.example",
        "display_name": "Example",
        "vendor": "Audacity",
        "version": "1.0",
        "description": "Test plugin",
        "entry": {
            "macos-arm64": "lib/example.dylib",
            "linux-x86_64": "lib/example.so"
        }
    })";
}

void replace(std::string& text, std::string_view from, std::string_view to)
{
    const auto position = text.find(from);
    ASSERT_NE(position, std::string::npos);
    text.replace(position, from.size(), to);
}

const ManifestError& error(const ManifestReadResult& result)
{
    EXPECT_TRUE(std::holds_alternative<ManifestError>(result));
    return std::get<ManifestError>(result);
}

TEST(ManifestReaderTests, ReadsCurrentPlatformEntry)
{
    const auto result = readManifest(validManifest(), Platform::MacArm64);
    ASSERT_TRUE(std::holds_alternative<Manifest>(result));
    const auto& manifest = std::get<Manifest>(result);
    EXPECT_EQ(manifest.pluginId, "org.audacityteam.example");
    EXPECT_EQ(manifest.displayName, "Example");
    EXPECT_EQ(manifest.vendor, "Audacity");
    EXPECT_EQ(manifest.version, "1.0");
    EXPECT_EQ(manifest.entryPath, "lib/example.dylib");
}

TEST(ManifestReaderTests, RequiresExactSchemaAndApiVersions)
{
    auto manifest = validManifest();
    replace(manifest, "\"schema_version\": 1", "\"schema_version\": 2");
    EXPECT_EQ(error(readManifest(manifest, Platform::MacArm64)).field,
              "/schema_version");

    manifest = validManifest();
    replace(manifest, "\"revision\": 1", "\"revision\": 2");
    EXPECT_EQ(error(readManifest(manifest, Platform::MacArm64)).field,
              "/api/revision");
}

TEST(ManifestReaderTests, IgnoresUnknownPropertiesAndRejectsMissingValues)
{
    auto manifest = validManifest();
    replace(manifest, "\"description\": \"Test plugin\",",
            "\"surprise\": true,");
    EXPECT_TRUE(std::holds_alternative<Manifest>(
                    readManifest(manifest, Platform::MacArm64)));

    manifest = validManifest();
    replace(manifest, "\"vendor\": \"Audacity\",", "");
    EXPECT_EQ(error(readManifest(manifest, Platform::MacArm64)).field,
              "/vendor");
}

TEST(ManifestReaderTests, ValidatesIdentity)
{
    auto manifest = validManifest();
    replace(manifest, "org.audacityteam.example", "Invalid Id");
    EXPECT_EQ(error(readManifest(manifest, Platform::MacArm64)).field, "/id");
}

TEST(ManifestReaderTests, RequiresAnEntryForTheCurrentPlatform)
{
    const auto result = readManifest(validManifest(), Platform::WindowsX64);
    EXPECT_EQ(error(result).field, "/entry/windows-x86_64");
}
} // namespace
} // namespace au::audacityplugin
