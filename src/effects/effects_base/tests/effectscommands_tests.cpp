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
    EXPECT_TRUE(effectIdFromCommand(RepeatLastEffectCommand::id).empty());
    EXPECT_TRUE(effectIdFromCommand(ApplyPresetCommand::id).empty());
}

TEST(EffectsBase_EffectsCommandsTests, CommandInfo_ComesEntirelyFromTheStruct)
{
    const CommandInfo repeat = makeCommandInfo<RepeatLastEffectCommand>();
    EXPECT_EQ(repeat.command, RepeatLastEffectCommand::id);
    EXPECT_TRUE(repeat.inputSchema.args.empty());
    EXPECT_EQ(repeat.decoration.iconCode, muse::ui::IconCode::Code::NONE);

    const CommandInfo toggle = makeCommandInfo<ToggleVendorUiCommand>();
    EXPECT_EQ(toggle.decoration.checkable, Checkable::Yes);
    EXPECT_EQ(toggle.inputSchema.args.at("effectId").type, DataType::String);

    const CommandInfo del = makeCommandInfo<DeletePresetCommand>();
    EXPECT_EQ(del.decoration.iconCode, muse::ui::IconCode::Code::DELETE_TANK);
}

TEST(EffectsBase_EffectsCommandsTests, ApplyPreset_SchemaAndParamsComeFromTheStruct)
{
    const InputSchema schema = inputSchema<ApplyPresetCommand>();
    ASSERT_EQ(schema.args.size(), 2u);
    EXPECT_EQ(schema.args.at("instanceId").type, DataType::Int);
    EXPECT_EQ(schema.args.at("presetId").type, DataType::String);

    const Params params = toInputParams(ApplyPresetCommand { .instanceId = 7, .presetId = "Bright" });
    EXPECT_EQ(params.at("instanceId").toInt(), 7);
    EXPECT_EQ(params.at("presetId").toString(), "Bright");

    //! NOTE: a CommandQuery delivers its values as strings
    ApplyPresetCommand fromQuery;
    ASSERT_TRUE(fromInputParams(Params { { "instanceId", muse::Val("7") }, { "presetId", muse::Val("Bright") } }, fromQuery));
    EXPECT_EQ(fromQuery.instanceId, 7);
    EXPECT_EQ(fromQuery.presetId, "Bright");

    ApplyPresetCommand missing;
    EXPECT_FALSE(fromInputParams(Params { { "presetId", muse::Val("Bright") } }, missing));

    ApplyPresetCommand unknown;
    EXPECT_FALSE(fromInputParams(Params { { "instanceId", muse::Val(7) }, { "presetId", muse::Val("Bright") },
                                     { "preset", muse::Val("Bright") } }, unknown));
}
