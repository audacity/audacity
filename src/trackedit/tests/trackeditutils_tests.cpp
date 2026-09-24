/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "trackedit/trackeditutils.h"

using namespace au::trackedit;

TEST(TrackeditUtilsTests, ItemTrackIdsListsEachTrackOnceInItemOrder)
{
    //! [GIVEN] Clips on wave tracks 3 and 1, and labels on label tracks 5 and 6, each listed more than once
    const ItemKeys items {
        ClipKeyList { { 3, 10 }, { 1, 11 }, { 3, 12 } },
        LabelKeyList { { 5, 20 }, { 6, 21 }, { 5, 22 } }
    };

    //! [THEN] Every track appears once, clips' tracks first, in the order the items list them
    EXPECT_EQ(utils::itemTrackIds(items), (TrackIdList { 3, 1, 5, 6 }));
}

TEST(TrackeditUtilsTests, ItemTrackIdsIsEmptyWithoutItems)
{
    EXPECT_TRUE(utils::itemTrackIds(ItemKeys {}).empty());
}

TEST(TrackeditUtilsTests, FirstItemOnTrackReturnsTheFirstItemListedForThatTrack)
{
    //! [GIVEN] Two clips on wave track 1 and two labels on label track 2
    const TrackItemKey firstClip { 1, 10 };
    const TrackItemKey firstLabel { 2, 20 };
    const ItemKeys items {
        ClipKeyList { firstClip, { 1, 11 } },
        LabelKeyList { firstLabel, { 2, 21 } }
    };

    //! [THEN] Each track yields its first listed item, and a track without items nothing
    EXPECT_EQ(utils::firstItemOnTrack(items, 1), firstClip);
    EXPECT_EQ(utils::firstItemOnTrack(items, 2), firstLabel);
    EXPECT_FALSE(utils::firstItemOnTrack(items, 3).has_value());
}
