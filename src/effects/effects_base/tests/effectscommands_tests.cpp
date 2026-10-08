/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "../effectscommands.h"

using namespace au::effects;
using namespace muse::rcommand;

TEST(EffectsBase_EffectsCommandsTests, EffectIdWithUriDelimiters_SurvivesCommandStringRoundTrip)
{
    const EffectId id = EffectId::fromStdString("Effect_VST3_Vendor_Name_C:\\Plug-ins/what?.vst3;100% #done");

    const Command openCommand = makeEffectOpenCommand(id);
    const Command applyCommand = makeEffectApplyCommand(id);

    EXPECT_EQ(effectIdFromCommand(Command(openCommand.toString())), id);
    EXPECT_EQ(effectIdFromCommand(Command(applyCommand.toString())), id);
}

TEST(EffectsBase_EffectsCommandsTests, EffectId_IsASinglePathSegment)
{
    const EffectId id = EffectId::fromStdString("Effect_Nyquist_Audacity_ShelfFilter_/usr/share/audacity/ShelfFilter.ny");

    const std::vector<std::string> segments = makeEffectOpenCommand(id).pathSegments();

    ASSERT_EQ(segments.size(), 3u);
    EXPECT_EQ(segments[0], "effects");
    EXPECT_EQ(segments[1], "open");
    EXPECT_EQ(decodeEffectId(segments[2]), id);
}

TEST(EffectsBase_EffectsCommandsTests, EffectIdFromCommand_IsEmptyForNonEffectCommands)
{
    EXPECT_TRUE(effectIdFromCommand(EFFECTS_REPEAT_LAST_EFFECT_COMMAND).empty());
    EXPECT_TRUE(effectIdFromCommand(ApplyPresetCommand::id).empty());
}

TEST(EffectsBase_EffectsCommandsTests, ApplyPreset_SchemaAndParamsComeFromTheStruct)
{
    const InputSchema schema = inputSchema<ApplyPresetCommand>();
    ASSERT_EQ(schema.args.size(), 2u);
    EXPECT_EQ(schema.args.at("instanceId").type, DataType::Integer);
    EXPECT_EQ(schema.args.at("presetId").type, DataType::String);

    const Params params = toParams(ApplyPresetCommand { .instanceId = 7, .presetId = "Bright" });
    EXPECT_EQ(params.at("instanceId").toInt(), 7);
    EXPECT_EQ(params.at("presetId").toString(), "Bright");

    //! NOTE: the legacy action bridge delivers URI-encoded (string) values
    ApplyPresetCommand fromQuery;
    std::string err;
    ASSERT_TRUE(fromParams(Params { { "instanceId", muse::Val("7") }, { "presetId", muse::Val("Bright") } }, fromQuery, err)) << err;
    EXPECT_EQ(fromQuery.instanceId, 7);
    EXPECT_EQ(fromQuery.presetId, "Bright");
}
