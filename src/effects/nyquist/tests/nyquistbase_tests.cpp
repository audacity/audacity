/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "au3-nyquist-effects/NyquistBase.h"

class TestNyquistBase final : public NyquistBase
{
public:
    TestNyquistBase()
        : NyquistBase(NYQUIST_WORKER_ID)
    {
    }

    bool ParseScript(const wxString& script)
    {
        return ParseCommand(script);
    }
};

static void ParseTypeLine(
    TestNyquistBase& effect, const wxString& typeLine)
{
    wxString script("$nyquist plug-in\n");
    script += typeLine;
    script += wxString("\n(return 1)\n");

    EXPECT_TRUE(effect.ParseScript(script));
}

TEST(NyquistBaseTests, ParsesToolEffectTypeAndGroup)
{
    TestNyquistBase effect;

    ParseTypeLine(effect, "$type tool analyze nogroup");

    EXPECT_EQ(EffectTypeAnalyze, effect.GetType());
    EXPECT_EQ(EffectTypeTool, effect.GetClassification());
    EXPECT_EQ(EffectGroup::None, effect.GetGroup());
}

TEST(NyquistBaseTests, ParsesToolGroupWithoutSubtype)
{
    TestNyquistBase effect;

    ParseTypeLine(effect, "$type tool nogroup");

    EXPECT_EQ(EffectTypeTool, effect.GetType());
    EXPECT_EQ(EffectTypeTool, effect.GetClassification());
    EXPECT_EQ(EffectGroup::None, effect.GetGroup());
}

TEST(NyquistBaseTests, ParsesGroupedToolSubtype)
{
    TestNyquistBase effect;

    ParseTypeLine(effect, "$type tool process volumeandcompression");

    EXPECT_EQ(EffectTypeProcess, effect.GetType());
    EXPECT_EQ(EffectTypeTool, effect.GetClassification());
    EXPECT_EQ(EffectGroup::VolumeAndCompression, effect.GetGroup());
}

TEST(NyquistBaseTests, ParsesGroupedToolGenerateSubtype)
{
    TestNyquistBase effect;

    ParseTypeLine(effect, "$type tool generate eqandfilters");

    EXPECT_EQ(EffectTypeGenerate, effect.GetType());
    EXPECT_EQ(EffectTypeTool, effect.GetClassification());
    EXPECT_EQ(EffectGroup::EqAndFilters, effect.GetGroup());
}

TEST(NyquistBaseTests, ParsesNonToolGroup)
{
    TestNyquistBase effect;

    ParseTypeLine(effect, "$type analyze nogroup");

    EXPECT_EQ(EffectTypeAnalyze, effect.GetType());
    EXPECT_EQ(EffectTypeAnalyze, effect.GetClassification());
    EXPECT_EQ(EffectGroup::None, effect.GetGroup());
}

TEST(NyquistBaseTests, ParsesBareToolType)
{
    TestNyquistBase effect;

    ParseTypeLine(effect, "$type tool");

    EXPECT_EQ(EffectTypeTool, effect.GetType());
    EXPECT_EQ(EffectTypeTool, effect.GetClassification());
    EXPECT_EQ(EffectGroup::Unspecified, effect.GetGroup());
}

TEST(NyquistBaseTests, ParsesToolAnalyzeSubtypeWithoutGroup)
{
    TestNyquistBase effect;

    ParseTypeLine(effect, "$type tool analyze");

    EXPECT_EQ(EffectTypeAnalyze, effect.GetType());
    EXPECT_EQ(EffectTypeTool, effect.GetClassification());
    EXPECT_EQ(EffectGroup::Unspecified, effect.GetGroup());
}

TEST(NyquistBaseTests, ParsesToolProcessSubtypeWithoutGroup)
{
    TestNyquistBase effect;

    ParseTypeLine(effect, "$type tool process");

    EXPECT_EQ(EffectTypeProcess, effect.GetType());
    EXPECT_EQ(EffectTypeTool, effect.GetClassification());
    EXPECT_EQ(EffectGroup::Unspecified, effect.GetGroup());
}
