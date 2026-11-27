/*
 * Audacity: A Digital Audio Editor
 */
#include "common/settingaccess.h"

#include <gtest/gtest.h>

#include <cmath>

namespace au::effects {
namespace {
struct TestSettings
{
    double gain = 0.0;
    int stages = 2;
    bool wetOnly = false;
};

constexpr EffectParameter Gain { &TestSettings::gain, L"Gain", 1.5, 0.1, 10.0, 0.01, 2 };
constexpr EffectParameter Stages { &TestSettings::stages, L"Stages", 2, 2, 24, 2 };
constexpr EffectParameter WetOnly { &TestSettings::wetOnly, L"WetOnly", false, false, true, 1 };
}

TEST(SettingAccessTests, ReportsTheParameterDescription)
{
    const SettingAccess<TestSettings> access = makeSettingAccess(Gain);

    EXPECT_DOUBLE_EQ(access.min, 0.1);
    EXPECT_DOUBLE_EQ(access.max, 10.0);
    EXPECT_DOUBLE_EQ(access.step, 0.01);
    EXPECT_DOUBLE_EQ(access.def, 1.5);
    EXPECT_EQ(access.decimals, 2);
}

TEST(SettingAccessTests, ReadsAndWritesTheSetting)
{
    const SettingAccess<TestSettings> access = makeSettingAccess(Gain);
    TestSettings settings;

    access.set(settings, 2.25);

    EXPECT_DOUBLE_EQ(settings.gain, 2.25);
    EXPECT_DOUBLE_EQ(access.get(settings), 2.25);
}

TEST(SettingAccessTests, KeepsTheValueInTheRange)
{
    const SettingAccess<TestSettings> access = makeSettingAccess(Gain);
    TestSettings settings;

    access.set(settings, 0.0);
    EXPECT_DOUBLE_EQ(settings.gain, 0.1);

    access.set(settings, 99.0);
    EXPECT_DOUBLE_EQ(settings.gain, 10.0);
}

TEST(SettingAccessTests, RoundsAnIntegerSetting)
{
    const SettingAccess<TestSettings> access = makeSettingAccess(Stages);
    TestSettings settings;

    access.set(settings, 7.6);

    EXPECT_EQ(settings.stages, 8);
    EXPECT_DOUBLE_EQ(access.get(settings), 8.0);
}

TEST(SettingAccessTests, TreatsABooleanSettingAsZeroOrOne)
{
    const SettingAccess<TestSettings> access = makeSettingAccess(WetOnly);
    TestSettings settings;

    EXPECT_DOUBLE_EQ(access.min, 0.0);
    EXPECT_DOUBLE_EQ(access.max, 1.0);
    EXPECT_DOUBLE_EQ(access.get(settings), 0.0);

    access.set(settings, 1.0);
    EXPECT_TRUE(settings.wetOnly);
    EXPECT_DOUBLE_EQ(access.get(settings), 1.0);

    access.set(settings, 0.0);
    EXPECT_FALSE(settings.wetOnly);
}

TEST(SettingAccessTests, AdjustsTheValueOnceItIsInTheRange)
{
    const SettingAccess<TestSettings> access
        = makeSettingAccess(Stages, [](double value) { return static_cast<double>(std::lround(value) & ~1); });
    TestSettings settings;

    access.set(settings, 7.0);
    EXPECT_EQ(settings.stages, 6);

    access.set(settings, 99.0);
    EXPECT_EQ(settings.stages, 24);

    access.set(settings, 1.0);
    EXPECT_EQ(settings.stages, 2);
}
}
