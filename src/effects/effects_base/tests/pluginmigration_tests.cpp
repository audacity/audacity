/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "global/serialization/json.h"
#include "global/modularity/ioc.h"

#include "audioplugins/internal/knownaudiopluginsmigrationregister.h"

#include "effects/builtin/internal/builtintypes.h"

#include "../internal/knownaudiopluginsconfigurator.h"

using namespace muse;
using namespace muse::audioplugins;
using namespace au::effects;

// Covers the Audacity-specific v2 -> v3 known-audio-plugins migration installed
// by KnownAudioPluginsConfigurator: pre-revamp caches stored builtin effects as
// the "NativeEffect" resource type, which no longer maps to a family (it would
// resolve to EffectFamily::Unknown and trip the effectsutils asserts). The
// migration renames it to the current builtin type.
class EffectsBase_PluginMigrationTests : public ::testing::Test
{
protected:
    static constexpr const char* MODULE = "effects_base_tests";

    std::shared_ptr<KnownAudioPluginsMigrationRegister> m_register;

    void SetUp() override
    {
        // The configurator resolves IKnownAudioPluginsMigrationRegister through
        // GlobalInject; export a fresh concrete register so init() installs the
        // production migration into something we can drive directly.
        muse::modularity::globalIoc()->unregister<IKnownAudioPluginsMigrationRegister>(MODULE);
        m_register = std::make_shared<KnownAudioPluginsMigrationRegister>();
        muse::modularity::globalIoc()->registerExport<IKnownAudioPluginsMigrationRegister>(MODULE, m_register);

        KnownAudioPluginsConfigurator configurator;
        configurator.init();
    }

    void TearDown() override
    {
        muse::modularity::globalIoc()->unregister<IKnownAudioPluginsMigrationRegister>(MODULE);
    }

    static JsonObject makeEntry(const std::string& type, const std::string& id, const std::string& path)
    {
        JsonObject attrs;
        attrs.set("title", std::string("Amplify"));

        JsonObject meta;
        meta.set("id", id);
        meta.set("type", type);
        meta.set("vendor", std::string("Audacity"));
        meta.set("attributes", attrs);

        JsonObject obj;
        obj.set("meta", meta);
        obj.set("path", path);
        obj.set("state", std::string("Validated"));
        return obj;
    }

    static std::string metaTypeAt(const JsonArray& arr, size_t i)
    {
        return arr.at(i).toObject().value("meta").toObject().value("type").toStdString();
    }
};

TEST_F(EffectsBase_PluginMigrationTests, V2ToV3_RenamesLegacyNativeEffectToBuiltIn)
{
    const std::string builtinType(builtin::AUDIO_RESOURCE_TYPE_NAME);
    // Guard the premise: the rename only matters because the names differ.
    ASSERT_NE(builtinType, std::string("NativeEffect"));

    // [GIVEN] a cache holding a legacy "NativeEffect" builtin entry
    JsonArray plugins;
    plugins << makeEntry("NativeEffect", "Effect_Audacity_Audacity_Amplify_Built-in Effect: Amplify",
                         "Built-in Effect: Amplify");

    // [WHEN] migrating from v2 to v3
    Ret ret = m_register->migrate(2, 3, plugins);

    // [THEN] the resource type is rewritten to the current builtin type
    ASSERT_TRUE(ret);
    ASSERT_EQ(plugins.size(), size_t(1));
    EXPECT_EQ(metaTypeAt(plugins, 0), builtinType);
}

TEST_F(EffectsBase_PluginMigrationTests, V2ToV3_LeavesOtherPluginTypesUntouched)
{
    JsonArray plugins;
    plugins << makeEntry("VstPlugin", "v", "/x/a.vst3");
    plugins << makeEntry("AudioUnit", "a", "/x/b.component");
    plugins << makeEntry("NyquistPlugin", "n", "/x/c.ny");

    Ret ret = m_register->migrate(2, 3, plugins);

    ASSERT_TRUE(ret);
    EXPECT_EQ(metaTypeAt(plugins, 0), "VstPlugin");
    EXPECT_EQ(metaTypeAt(plugins, 1), "AudioUnit");
    EXPECT_EQ(metaTypeAt(plugins, 2), "NyquistPlugin");
}

TEST_F(EffectsBase_PluginMigrationTests, V2ToV3_PreservesSurroundingFields)
{
    JsonArray plugins;
    plugins << makeEntry("NativeEffect", "the-id", "the-path");

    Ret ret = m_register->migrate(2, 3, plugins);

    ASSERT_TRUE(ret);
    const JsonObject obj = plugins.at(0).toObject();
    const JsonObject meta = obj.value("meta").toObject();
    EXPECT_EQ(obj.value("path").toStdString(), "the-path");
    EXPECT_EQ(obj.value("state").toStdString(), "Validated");
    EXPECT_EQ(meta.value("id").toStdString(), "the-id");
    EXPECT_EQ(meta.value("vendor").toStdString(), "Audacity");
    EXPECT_EQ(meta.value("attributes").toObject().value("title").toStdString(), "Amplify");
}

TEST_F(EffectsBase_PluginMigrationTests, FullChain_LegacyBareArrayCacheReachesV3WithBuiltInType)
{
    const std::string builtinType(builtin::AUDIO_RESOURCE_TYPE_NAME);

    // [GIVEN] a v0-shaped entry (legacy bare array: carries "enabled" and the
    // pre-revamp "NativeEffect" type)
    JsonObject meta;
    meta.set("id", std::string("Effect_Audacity_Audacity_Amplify_Built-in Effect: Amplify"));
    meta.set("type", std::string("NativeEffect"));

    JsonObject obj;
    obj.set("meta", meta);
    obj.set("path", std::string("Built-in Effect: Amplify"));
    obj.set("enabled", true);

    JsonArray plugins;
    plugins << obj;

    // [WHEN] migrating the whole chain v0 -> current
    Ret ret = m_register->migrate(0, CURRENT_KNOWN_AUDIO_PLUGINS_VERSION, plugins);

    // [THEN] the type is renamed and the v1 -> v2 enabled -> state step also ran
    ASSERT_TRUE(ret);
    ASSERT_EQ(plugins.size(), size_t(1));
    const JsonObject migrated = plugins.at(0).toObject();
    EXPECT_EQ(migrated.value("meta").toObject().value("type").toStdString(), builtinType);
    EXPECT_FALSE(migrated.contains("enabled"));
    EXPECT_EQ(migrated.value("state").toStdString(), "Validated");
}
