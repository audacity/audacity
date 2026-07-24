/*
 * Audacity: A Digital Audio Editor
 */
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "global/iglobalconfiguration.h"
#include "global/modularity/ioc.h"
#include "global/tests/mocks/globalconfigurationmock.h"
#include "internal/audacitypluginhost.h"
#include "internal/manifestreader.h"

#include <atomic>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <string>

namespace au::audacityplugin {
namespace {
using ::testing::NiceMock;
using ::testing::Return;

Platform currentTestPlatform()
{
#if defined(_WIN32) && (defined(_M_ARM64) || defined(__aarch64__))
    return Platform::WindowsArm64;
#elif defined(_WIN32)
    return Platform::WindowsX64;
#elif defined(__APPLE__) && defined(__aarch64__)
    return Platform::MacArm64;
#elif defined(__APPLE__)
    return Platform::MacX64;
#elif defined(__linux__) && defined(__aarch64__)
    return Platform::LinuxArm64;
#else
    return Platform::LinuxX64;
#endif
}

const char* currentPlatformKey()
{
    switch (currentTestPlatform()) {
    case Platform::WindowsX64: return "windows-x86_64";
    case Platform::WindowsArm64: return "windows-arm64";
    case Platform::MacX64: return "macos-x86_64";
    case Platform::MacArm64: return "macos-arm64";
    case Platform::LinuxX64: return "linux-x86_64";
    case Platform::LinuxArm64: return "linux-arm64";
    }
    return "";
}

std::filesystem::path temporaryRoot()
{
    static std::atomic<uint64_t> serial { 0 };
    const auto now = std::chrono::high_resolution_clock::now()
                     .time_since_epoch().count();
    return std::filesystem::temp_directory_path()
           / ("audacity-plugin-host-test-" + std::to_string(now) + "-"
              + std::to_string(serial.fetch_add(1)));
}

class AudacityPluginHostTests : public ::testing::Test
{
protected:
    static constexpr const char* MODULE = "audacityplugin_host_tests";

    void SetUp() override
    {
        m_root = temporaryRoot();
        m_applicationData = m_root / "application";
        m_userData = m_root / "user";
        std::filesystem::create_directories(m_applicationData / "plugins");
        std::filesystem::create_directories(m_userData / "plugins");

        m_sourceBundle = std::filesystem::canonical(
            std::filesystem::u8path(AUP_TEST_PLUGIN_BUNDLE_PATH));
        std::ifstream input(m_sourceBundle / "plugin.json", std::ios::binary);
        ASSERT_TRUE(input);
        const std::string json((std::istreambuf_iterator<char>(input)), {});
        const auto parsed = readManifest(json, currentTestPlatform());
        const auto* manifest = std::get_if<Manifest>(&parsed);
        ASSERT_NE(manifest, nullptr);
        m_sourceEntry = manifest->entryPath;

        muse::modularity::globalIoc()->unregister<muse::IGlobalConfiguration>(MODULE);
        m_configuration
            = std::make_shared<NiceMock<muse::GlobalConfigurationMock> >();
        ON_CALL(*m_configuration, appDataPath())
        .WillByDefault(Return(muse::io::path_t(m_applicationData.u8string())));
        ON_CALL(*m_configuration, userAppDataPath())
        .WillByDefault(Return(muse::io::path_t(m_userData.u8string())));
        muse::modularity::globalIoc()->registerExport<muse::IGlobalConfiguration>(
            MODULE, m_configuration);
    }

    void TearDown() override
    {
        muse::modularity::globalIoc()->unregister<muse::IGlobalConfiguration>(MODULE);
        std::error_code ignored;
        std::filesystem::remove_all(m_root, ignored);
    }

    std::filesystem::path installBundle(
        const std::filesystem::path& dataRoot,
        const std::filesystem::path& relativeBundle,
        const std::string& pluginId,
        const std::string& displayName)
    {
        const auto destination = dataRoot / "plugins" / relativeBundle;
        const auto entry = std::filesystem::u8path(m_sourceEntry);
        std::filesystem::create_directories((destination / entry).parent_path());
        std::filesystem::copy_file(
            m_sourceBundle / entry, destination / entry,
            std::filesystem::copy_options::overwrite_existing);

        std::ofstream manifest(destination / "plugin.json", std::ios::binary);
        if (!manifest) {
            throw std::runtime_error("could not create plugin.json");
        }
        manifest << "{\n"
                 << "  \"schema_version\": 1,\n"
                 << "  \"api\": { \"major\": 0, \"revision\": 1 },\n"
                 << "  \"id\": \"" << pluginId << "\",\n"
                 << "  \"display_name\": \"" << displayName << "\",\n"
                 << "  \"vendor\": \"Audacity tests\",\n"
                 << "  \"version\": \"1\",\n"
                 << "  \"entry\": { \"" << currentPlatformKey() << "\": \""
                 << m_sourceEntry << "\" }\n"
                 << "}\n";
        if (!manifest) {
            throw std::runtime_error("could not write plugin.json");
        }
        return destination;
    }

    std::filesystem::path m_root;
    std::filesystem::path m_applicationData;
    std::filesystem::path m_userData;
    std::filesystem::path m_sourceBundle;
    std::string m_sourceEntry;
    std::shared_ptr<NiceMock<muse::GlobalConfigurationMock> > m_configuration;
};

TEST_F(AudacityPluginHostTests, DiscoversImmediateBundlesOnceAtStartup)
{
    constexpr auto pluginId = "org.audacity.tests.immediate";
    const auto installed = installBundle(
        m_applicationData, "Immediate.audacity4-plugin", pluginId, "Immediate");
    installBundle(
        m_applicationData,
        std::filesystem::path("nested") / "Nested.audacity4-plugin",
        "org.audacity.tests.nested", "Nested");

    AudacityPluginHost host;
    host.initialize();

    ASSERT_EQ(host.effects().size(), 1U);
    EXPECT_EQ(host.effects().front().pluginId, pluginId);
    EXPECT_EQ(host.effects().front().effectId, "gain");

    installBundle(m_userData, "Added.audacity4-plugin",
                  "org.audacity.tests.added", "Added");
    std::filesystem::remove(installed / "plugin.json");

    EXPECT_EQ(host.effects().size(), 1U);
    EXPECT_EQ(host.createInstance(pluginId, "gain", { 0.0, 44100 }).status,
              Status::Ok);
    EXPECT_EQ(host.createInstance(
                  "org.audacity.tests.added", "gain", { 0.0, 44100 }).status,
              Status::InvalidArgument);
}

TEST_F(AudacityPluginHostTests, UserBundleOverridesApplicationBundle)
{
    constexpr auto pluginId = "org.audacity.tests.precedence";
    installBundle(m_applicationData, "Application.audacity4-plugin",
                  pluginId, "Application");
    const auto userBundle = installBundle(
        m_userData, "User.audacity4-plugin", pluginId, "User");

    AudacityPluginHost host;
    host.initialize();

    ASSERT_EQ(host.effects().size(), 1U);
    EXPECT_EQ(host.effects().front().pluginName, "User");
    EXPECT_EQ(host.effects().front().bundlePath,
              std::filesystem::canonical(userBundle).u8string());
}

TEST_F(AudacityPluginHostTests, DuplicateUserIdsFallBackToApplicationBundle)
{
    constexpr auto pluginId = "org.audacity.tests.duplicates";
    installBundle(m_applicationData, "Application.audacity4-plugin",
                  pluginId, "Application");
    installBundle(m_userData, "First.audacity4-plugin", pluginId, "First");
    installBundle(m_userData, "Second.audacity4-plugin", pluginId, "Second");

    AudacityPluginHost host;
    host.initialize();

    ASSERT_EQ(host.effects().size(), 1U);
    EXPECT_EQ(host.effects().front().pluginName, "Application");
}
} // namespace
} // namespace au::audacityplugin
