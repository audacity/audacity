/*
* Audacity: A Digital Audio Editor
*/

#include <gtest/gtest.h>

#include "audio/inputchannelselection.h"

using namespace au::audio;

TEST(InputChannelSelectionTests, OffersEveryMonoAndConventionalStereoGroup)
{
    const InputChannelSelection expected {
        { { 0 } }, { { 1 } }, { { 2 } }, { { 3 } },
        { { 0, 1 } }, { { 2, 3 } },
    };
    EXPECT_EQ(availableInputChannelGroups(4), expected);
}

TEST(InputChannelSelectionTests, AvailableGroupsHandleZeroOneAndOddCapacities)
{
    EXPECT_TRUE(availableInputChannelGroups(0).empty());
    EXPECT_EQ(availableInputChannelGroups(1),
              InputChannelSelection({ { { 0 } } }));
    EXPECT_EQ(availableInputChannelGroups(3),
              InputChannelSelection({ { { 0 } }, { { 1 } }, { { 2 } },
                                        { { 0, 1 } } }));
}

TEST(InputChannelSelectionTests, ApplicationPolicyRejectsNonConventionalStereoPairs)
{
    EXPECT_TRUE(isValidInputChannelGroup({ { 3 } }, 4));
    EXPECT_TRUE(isValidInputChannelGroup({ { 0, 1 } }, 4));
    EXPECT_FALSE(isValidInputChannelGroup({ { 1, 2 } }, 4));
    EXPECT_FALSE(isValidInputChannelGroup({ { 3, 4 } }, 4));
    EXPECT_FALSE(isValidInputChannelGroup({ { 0, 0 } }, 4));
    EXPECT_FALSE(isValidInputChannelGroup({}, 4));
}

TEST(InputChannelSelectionTests, SelectingStereoPairAtomicallyReplacesOverlappingMonos)
{
    const InputChannelSelection selection { { { 0 } }, { { 1 } }, { { 3 } } };
    const InputChannelSelection expected { { { 0, 1 } }, { { 3 } } };
    EXPECT_EQ(toggleInputChannelGroup(selection, { { 0, 1 } }, 4), expected);
}

TEST(InputChannelSelectionTests, SelectingMonoAtomicallyReplacesOverlappingStereoPair)
{
    const InputChannelSelection selection { { { 0, 1 } }, { { 2, 3 } } };
    const InputChannelSelection expected { { { 0 } }, { { 2, 3 } } };
    EXPECT_EQ(toggleInputChannelGroup(selection, { { 0 } }, 4), expected);
}

TEST(InputChannelSelectionTests, CannotRemoveLastGroup)
{
    const InputChannelSelection selection { { { 2 } } };
    EXPECT_EQ(toggleInputChannelGroup(selection, { { 2 } }, 4), selection);
}

TEST(InputChannelSelectionTests, CanRemoveOneOfSeveralGroups)
{
    const InputChannelSelection selection { { { 0 } }, { { 2, 3 } } };
    EXPECT_EQ(toggleInputChannelGroup(selection, { { 0 } }, 4),
              InputChannelSelection({ { { 2, 3 } } }));
}

TEST(InputChannelSelectionTests, InvalidToggleLeavesNormalizedSelectionUnchanged)
{
    const InputChannelSelection selection { { { 2, 3 } }, { { 0 } } };
    EXPECT_EQ(toggleInputChannelGroup(selection, { { 1, 2 } }, 4),
              InputChannelSelection({ { { 0 } }, { { 2, 3 } } }));
}

TEST(InputChannelSelectionTests, NormalizationOrdersGroupsAndDropsOutOfRangeOnDeviceChange)
{
    const InputChannelSelection selection { { { 4 } }, { { 2, 3 } }, { { 0 } } };
    const InputChannelSelection expected { { { 0 } }, { { 2, 3 } } };
    EXPECT_EQ(normalizeInputChannelSelection(selection, 4), expected);
}

TEST(InputChannelSelectionTests, CapacityReductionPreservesLegacyPresetLayouts)
{
    const struct {
        int preset;
        int capacity;
        InputChannelSelection expected;
    } cases[] {
        { 3, 2, { { { 0, 1 } } } },
        { 4, 2, { { { 0, 1 } } } },
        { 4, 3, { { { 0 } }, { { 1 } }, { { 2 } } } },
        { 4, 1, { { { 0 } } } },
        { 2, 1, { { { 0 } } } },
        { 4, 0, {} },
    };

    for (const auto& testCase : cases) {
        SCOPED_TRACE(::testing::Message() << "preset " << testCase.preset << ", capacity " << testCase.capacity);
        EXPECT_EQ(normalizeInputChannelSelection(legacyInputChannelSelection(testCase.preset), testCase.capacity),
                  testCase.expected);
    }
}

TEST(InputChannelSelectionTests, CapacityChangesDoNotExpandPresetsOrRestoreDiscardedInputs)
{
    const auto preset = legacyInputChannelSelection(4);
    EXPECT_EQ(normalizeInputChannelSelection(preset, 4), preset);
    EXPECT_EQ(normalizeInputChannelSelection(preset, 8), preset);

    const auto reduced = normalizeInputChannelSelection(preset, 2);
    EXPECT_EQ(normalizeInputChannelSelection(reduced, 2),
              InputChannelSelection({ { { 0, 1 } } }));
    EXPECT_EQ(normalizeInputChannelSelection(reduced, 4),
              InputChannelSelection({ { { 0, 1 } } }));
}

TEST(InputChannelSelectionTests, CapacityReductionPreservesCustomGrouping)
{
    const struct {
        InputChannelSelection selection;
        int capacity;
        InputChannelSelection expected;
    } cases[] {
        { { { { 0 } }, { { 1 } } }, 2, { { { 0 } }, { { 1 } } } },
        { { { { 0 } }, { { 1 } }, { { 4 } } }, 2, { { { 0 } }, { { 1 } } } },
        { { { { 2, 3 } } }, 2, { { { 0 } } } },
        { { { { 0 } }, { { 2, 3 } }, { { 6 } } }, 4, { { { 0 } }, { { 2, 3 } } } },
    };

    for (const auto& testCase : cases) {
        SCOPED_TRACE(::testing::PrintToString(testCase.selection));
        EXPECT_EQ(normalizeInputChannelSelection(testCase.selection, testCase.capacity), testCase.expected);
    }
}

TEST(InputChannelSelectionTests, NormalizationFallsBackToFirstMonoWhenNothingRemains)
{
    EXPECT_EQ(normalizeInputChannelSelection({ { { 6 } } }, 2),
              InputChannelSelection({ { { 0 } } }));
    EXPECT_TRUE(normalizeInputChannelSelection({ { { 0 } } }, 0).empty());
}

TEST(InputChannelSelectionTests, NormalizationRejectsMalformedGroupsAndResolvesOverlap)
{
    const InputChannelSelection selection {
        { { 1, 2 } }, // not a conventional stereo pair
        { { 0, 1 } },
        { { 0 } },
        { { 2, 3, 4 } },
        { { 3 } },
    };
    const InputChannelSelection expected { { { 0 } }, { { 3 } } };
    EXPECT_EQ(normalizeInputChannelSelection(selection, 4), expected);
}

TEST(InputChannelSelectionTests, LegacyChannelCountsPreserveTrackLayout)
{
    EXPECT_TRUE(legacyInputChannelSelection(-1).empty());
    EXPECT_TRUE(legacyInputChannelSelection(0).empty());
    EXPECT_EQ(legacyInputChannelSelection(1), InputChannelSelection({ { { 0 } } }));
    EXPECT_EQ(legacyInputChannelSelection(2), InputChannelSelection({ { { 0, 1 } } }));
    EXPECT_EQ(legacyInputChannelSelection(4),
              InputChannelSelection({ { { 0 } }, { { 1 } }, { { 2 } }, { { 3 } } }));
}

TEST(InputChannelSelectionTests, ComputesLogicalCountFlatteningAndPortAudioPrefixWidth)
{
    const InputChannelSelection selection { { { 0 } }, { { 2, 3 } } };
    EXPECT_EQ(inputChannelCount(selection), 3u);
    EXPECT_EQ(flattenInputChannelSelection(selection),
              std::vector<InputChannelIndex>({ 0, 2, 3 }));
    EXPECT_EQ(inputChannelStreamWidth(selection), 4u);

    EXPECT_EQ(inputChannelCount({}), 0u);
    EXPECT_TRUE(flattenInputChannelSelection({}).empty());
    EXPECT_EQ(inputChannelStreamWidth({}), 0u);
}
