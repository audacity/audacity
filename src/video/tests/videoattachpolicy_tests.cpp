/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "videoattachpolicy.h"

using namespace au::video;

// ---------------------------------------------------------------------------
// Recognising a video file by name. No FFmpeg, no fixtures, no project.
// ---------------------------------------------------------------------------

TEST(VideoAttachPolicyTests, RecognisesKnownExtensions)
{
    EXPECT_TRUE(hasVideoExtension("/media/take1.mp4"));
    EXPECT_TRUE(hasVideoExtension("/media/take1.mkv"));
    EXPECT_TRUE(hasVideoExtension("/media/broadcast.ts"));
    EXPECT_TRUE(hasVideoExtension("clip.webm"));
}

TEST(VideoAttachPolicyTests, IgnoresAudioAndOtherFiles)
{
    for (const char* path : { "/audio/take1.wav", "/audio/take1.mp3",
                              "/audio/take1.flac", "/audio/take1.aiff",
                              "/notes/session.txt", "/projects/mix.aup4" }) {
        EXPECT_FALSE(hasVideoExtension(path)) << path;
    }
}

TEST(VideoAttachPolicyTests, IsCaseInsensitive)
{
    EXPECT_TRUE(hasVideoExtension("/media/TAKE1.MP4"));
    EXPECT_TRUE(hasVideoExtension("/media/Take1.MoV"));
}

TEST(VideoAttachPolicyTests, HandlesNamesWithoutAnExtension)
{
    EXPECT_FALSE(hasVideoExtension("/media/rushes"));
    EXPECT_FALSE(hasVideoExtension(""));
    EXPECT_FALSE(hasVideoExtension("/media/take1."));
}

TEST(VideoAttachPolicyTests, ADotInADirectoryNameIsNotAnExtension)
{
    // "/shoot.2026/rushes" has a dot, but not in the file name.
    EXPECT_FALSE(hasVideoExtension("/shoot.2026/rushes"));
    EXPECT_TRUE(hasVideoExtension("/shoot.2026/rushes.mp4"));
}

TEST(VideoAttachPolicyTests, TheFilterCoversEveryKnownExtension)
{
    const std::string filter = videoFileFilter();
    for (const std::string& ext : videoFileExtensions()) {
        EXPECT_NE(filter.find("*." + ext), std::string::npos) << ext;
    }
}

// ---------------------------------------------------------------------------
// Deciding whether an import should also attach a picture.
// ---------------------------------------------------------------------------

TEST(VideoAttachPolicyTests, AttachesTheVideoAnImportBroughtIn)
{
    const auto chosen = videoToAttachAfterImport({ "/media/take1.mp4" }, false);
    ASSERT_TRUE(chosen.has_value());
    EXPECT_EQ(*chosen, "/media/take1.mp4");
}

TEST(VideoAttachPolicyTests, PicksTheVideoOutOfAMixedImport)
{
    const auto chosen = videoToAttachAfterImport(
        { "/audio/boom.wav", "/media/take1.mov", "/audio/lav.wav" }, false);
    ASSERT_TRUE(chosen.has_value());
    EXPECT_EQ(*chosen, "/media/take1.mov");
}

TEST(VideoAttachPolicyTests, DoesNothingForAnAudioOnlyImport)
{
    EXPECT_FALSE(videoToAttachAfterImport(
                     { "/audio/a.wav", "/audio/b.mp3" }, false).has_value());
}

TEST(VideoAttachPolicyTests, DoesNothingForAnEmptyImport)
{
    EXPECT_FALSE(videoToAttachAfterImport({}, false).has_value());
}

TEST(VideoAttachPolicyTests, NeverReplacesAnExistingAttachment)
{
    // The user chose the attached video deliberately. Importing something
    // else must not swap the picture out from under them.
    EXPECT_FALSE(videoToAttachAfterImport({ "/media/take2.mp4" }, true).has_value());
}

TEST(VideoAttachPolicyTests, DeclinesToChooseBetweenSeveralVideos)
{
    // Picking one of these would be guessing, and the wrong guess is worse
    // than doing nothing: the user can attach the one they meant in a click.
    EXPECT_FALSE(videoToAttachAfterImport(
                     { "/media/camA.mp4", "/media/camB.mp4" }, false).has_value());

    EXPECT_FALSE(videoToAttachAfterImport(
                     { "/audio/boom.wav", "/media/camA.mp4",
                       "/media/camB.mov" }, false).has_value());
}

TEST(VideoAttachPolicyTests, TheSameFileTwiceIsStillTwoVideos)
{
    // Two entries means the caller asked for two imports; declining is right
    // even when they name the same file, because the second would duplicate.
    EXPECT_FALSE(videoToAttachAfterImport(
                     { "/media/take1.mp4", "/media/take1.mp4" }, false).has_value());
}
