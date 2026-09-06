/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/inputchannelselectionsettings.h"

namespace au::au3audio {
TEST(InputChannelSelectionSettingsTests, RoundTripsNestedChannelGroups)
{
    const audio::InputChannelSelection selection {
        { { 0 } }, { { 2, 3 } }, { { 7 } },
    };

    EXPECT_EQ(details::inputChannelSelectionFromVal(
                  details::inputChannelSelectionToVal(selection)), selection);
}

TEST(InputChannelSelectionSettingsTests, DropsMalformedGroupsWithoutThrowing)
{
    const muse::Val value(muse::ValList {
            muse::Val(muse::ValList { muse::Val(-1) }),
            muse::Val(muse::ValList {}),
            muse::Val(muse::ValList { muse::Val("not a channel") }),
            muse::Val(muse::ValList { muse::Val(2), muse::Val(3) }),
        });

    EXPECT_EQ(details::inputChannelSelectionFromVal(value),
              audio::InputChannelSelection({ { { 2, 3 } } }));
}

TEST(InputChannelSelectionSettingsTests, EmptyNewSettingMigratesLegacyCount)
{
    EXPECT_EQ(details::inputChannelSelectionFromSettings(
                  muse::Val(muse::ValList {}), 2, 4),
              audio::InputChannelSelection({ { { 0, 1 } } }));
}

TEST(InputChannelSelectionSettingsTests, CapacityReductionClampsLegacyCountBeforeMigration)
{
    const muse::Val emptySelection(muse::ValList {});
    for (const int legacyCount : { -1, 0, 1, 2, 3, 4 }) {
        SCOPED_TRACE(legacyCount);
        const audio::InputChannelSelection expected = legacyCount <= 1
                                                      ? audio::InputChannelSelection { { { 0 } } }
        : audio::InputChannelSelection { { { 0, 1 } } };
        EXPECT_EQ(details::inputChannelSelectionFromSettings(emptySelection, legacyCount, 2), expected);
        EXPECT_EQ(details::inputChannelSelectionFromSettings(emptySelection, legacyCount, 1),
                  audio::InputChannelSelection({ { { 0 } } }));
        EXPECT_TRUE(details::inputChannelSelectionFromSettings(emptySelection, legacyCount, 0).empty());
    }
}

TEST(InputChannelSelectionSettingsTests, CapacityReductionClampsPersistedPreset)
{
    const auto value = details::inputChannelSelectionToVal(audio::legacyInputChannelSelection(4));

    EXPECT_EQ(details::inputChannelSelectionFromSettings(value, 1, 2),
              audio::InputChannelSelection({ { { 0, 1 } } }));
}

TEST(InputChannelSelectionSettingsTests, CapacityReductionKeepsPersistedCustomMonosSeparate)
{
    const audio::InputChannelSelection twoMonos { { { 0 } }, { { 1 } } };
    const auto savedTwoMonos = details::inputChannelSelectionToVal(twoMonos);
    EXPECT_EQ(details::inputChannelSelectionFromSettings(savedTwoMonos, 2, 2), twoMonos);

    const auto savedCustomRoute = details::inputChannelSelectionToVal(
        audio::InputChannelSelection { { { 0 } }, { { 1 } }, { { 4 } } });
    const auto reduced = details::inputChannelSelectionFromSettings(savedCustomRoute, 3, 2);
    EXPECT_EQ(reduced, twoMonos);
    EXPECT_EQ(details::inputChannelSelectionFromSettings(details::inputChannelSelectionToVal(reduced), 2, 2), twoMonos);
}

TEST(InputChannelSelectionSettingsTests, NewSettingTakesPrecedenceOverLegacyCount)
{
    const auto value = details::inputChannelSelectionToVal(
        audio::InputChannelSelection({ { { 2, 3 } } }));

    EXPECT_EQ(details::inputChannelSelectionFromSettings(value, 1, 4),
              audio::InputChannelSelection({ { { 2, 3 } } }));
}

TEST(InputChannelSelectionSettingsTests, NormalizesPersistedSelectionForDeviceCapacity)
{
    const auto value = details::inputChannelSelectionToVal(
        audio::InputChannelSelection({ { { 0 } }, { { 4, 5 } } }));

    EXPECT_EQ(details::inputChannelSelectionFromSettings(value, 6, 4),
              audio::InputChannelSelection({ { { 0 } } }));
    EXPECT_TRUE(details::inputChannelSelectionFromSettings(value, 6, 0).empty());
}
}
