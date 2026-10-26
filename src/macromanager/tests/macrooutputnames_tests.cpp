/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "macromanager/internal/macrooutputnames.h"

using namespace muse;
using namespace au::macromanager;

using ::testing::ElementsAre;

TEST(MacroManager_MacroOutputNamesTests, KeepsDistinctBaseNames)
{
    EXPECT_THAT(uniqueOutputNames({ "/a/one.wav", "/a/two.flac" }), ElementsAre("one", "two"));
}

TEST(MacroManager_MacroOutputNamesTests, AddsSuffixToSameBaseNameInDifferentFolders)
{
    EXPECT_THAT(uniqueOutputNames({ "/a/take.wav", "/b/take.flac", "/c/take.mp3" }), ElementsAre("take", "take_2", "take_3"));
}

TEST(MacroManager_MacroOutputNamesTests, AddsSuffixToNamesThatDifferOnlyInCase)
{
    EXPECT_THAT(uniqueOutputNames({ "/a/Take.wav", "/b/take.wav" }), ElementsAre("Take", "take_2"));
}

TEST(MacroManager_MacroOutputNamesTests, AddsSuffixToNamesThatEscapeToTheSameFileName)
{
    EXPECT_THAT(uniqueOutputNames({ "/a/my take.wav", "/b/my_take.wav" }), ElementsAre("my take", "my_take_2"));
}

TEST(MacroManager_MacroOutputNamesTests, AddsSuffixWhenGeneratedNameIsAlsoAnInputName)
{
    EXPECT_THAT(uniqueOutputNames({ "/a/take.wav", "/b/take.wav", "/c/take_2.wav" }), ElementsAre("take", "take_2", "take_2_2"));
}
