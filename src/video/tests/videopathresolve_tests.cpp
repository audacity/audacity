/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <set>
#include <string>

#include "internal/videopathresolve.h"

using namespace au::video;

namespace {
//! A pretend filesystem, so none of this touches a disk.
class Files
{
public:
    Files(std::initializer_list<std::string> present)
        : m_present(present) {}

    std::function<bool (const std::string&)> checker() const
    {
        return [this](const std::string& path) {
            return m_present.count(path) > 0;
        };
    }

private:
    std::set<std::string> m_present;
};
}

// ---------------------------------------------------------------------------
// Resolving
// ---------------------------------------------------------------------------

TEST(VideoPathResolveTests, PrefersTheRelativePathWhenBothResolve)
{
    // The project and its media were copied somewhere else together. The
    // absolute path still points at the original, which is the wrong file to
    // pick even though it opens.
    const Files files { "/new/place/media/clip.mp4", "/old/place/media/clip.mp4" };

    EXPECT_EQ(resolveVideoPath("/old/place/media/clip.mp4", "media/clip.mp4",
                               "/new/place", files.checker()),
              "/new/place/media/clip.mp4");
}

TEST(VideoPathResolveTests, FallsBackToAbsoluteWhenTheRelativeIsGone)
{
    // Media kept centrally, project moved on its own.
    const Files files { "/library/clip.mp4" };

    EXPECT_EQ(resolveVideoPath("/library/clip.mp4", "media/clip.mp4",
                               "/somewhere/else", files.checker()),
              "/library/clip.mp4");
}

TEST(VideoPathResolveTests, ReturnsEmptyWhenNeitherResolves)
{
    const Files files { "/unrelated/other.mp4" };

    EXPECT_TRUE(resolveVideoPath("/gone/clip.mp4", "media/clip.mp4",
                                 "/project", files.checker()).empty());
}

TEST(VideoPathResolveTests, AnUnsavedProjectCanOnlyUseTheAbsolutePath)
{
    // Before a first save there is no project directory to be relative to.
    const Files files { "/library/clip.mp4" };

    EXPECT_EQ(resolveVideoPath("/library/clip.mp4", "media/clip.mp4",
                               "", files.checker()),
              "/library/clip.mp4");
}

TEST(VideoPathResolveTests, HandlesAnEmptyRelativePath)
{
    const Files files { "/library/clip.mp4" };

    EXPECT_EQ(resolveVideoPath("/library/clip.mp4", "", "/project", files.checker()),
              "/library/clip.mp4");
}

TEST(VideoPathResolveTests, HandlesAnEmptyAbsolutePath)
{
    const Files files { "/project/media/clip.mp4" };

    EXPECT_EQ(resolveVideoPath("", "media/clip.mp4", "/project", files.checker()),
              "/project/media/clip.mp4");
}

TEST(VideoPathResolveTests, EverythingEmptyResolvesToNothing)
{
    const Files files {};
    EXPECT_TRUE(resolveVideoPath("", "", "", files.checker()).empty());
}

TEST(VideoPathResolveTests, ToleratesAMissingExistsFunction)
{
    EXPECT_TRUE(resolveVideoPath("/a/b.mp4", "b.mp4", "/a", nullptr).empty());
}

TEST(VideoPathResolveTests, NormalisesAwayParentSegments)
{
    // A relative path that walks up out of the project directory and back
    // down has to resolve to the plain path, not to something with ".." left
    // in it that no filesystem call would match.
    const Files files { "/work/media/clip.mp4" };

    EXPECT_EQ(resolveVideoPath("", "../media/clip.mp4", "/work/project",
                               files.checker()),
              "/work/media/clip.mp4");
}

// ---------------------------------------------------------------------------
// Making a relative path
// ---------------------------------------------------------------------------

TEST(VideoPathResolveTests, MakesAPathRelativeToTheProject)
{
    EXPECT_EQ(makeRelativeVideoPath("/work/project", "/work/project/media/clip.mp4"),
              "media/clip.mp4");
}

TEST(VideoPathResolveTests, WalksUpWhenTheMediaSitsBesideTheProject)
{
    EXPECT_EQ(makeRelativeVideoPath("/work/project", "/work/media/clip.mp4"),
              "../media/clip.mp4");
}

TEST(VideoPathResolveTests, GivesNothingWithoutAProjectDirectory)
{
    EXPECT_TRUE(makeRelativeVideoPath("", "/work/media/clip.mp4").empty());
}

TEST(VideoPathResolveTests, GivesNothingWithoutAnAbsolutePath)
{
    EXPECT_TRUE(makeRelativeVideoPath("/work/project", "").empty());
}

TEST(VideoPathResolveTests, RelativeThenResolveRoundTrips)
{
    const std::string projectDir = "/work/project";
    const std::string absolute = "/work/project/media/clip.mp4";

    const std::string relative = makeRelativeVideoPath(projectDir, absolute);
    ASSERT_FALSE(relative.empty());

    const Files files { absolute };
    EXPECT_EQ(resolveVideoPath(absolute, relative, projectDir, files.checker()),
              absolute);
}

TEST(VideoPathResolveTests, RoundTripsThroughAMove)
{
    // Record where it was, then move project and media together and confirm
    // the stored relative path still finds it.
    const std::string relative =
        makeRelativeVideoPath("/before", "/before/media/clip.mp4");
    ASSERT_EQ(relative, "media/clip.mp4");

    const Files moved { "/after/media/clip.mp4" };
    EXPECT_EQ(resolveVideoPath("/before/media/clip.mp4", relative, "/after",
                               moved.checker()),
              "/after/media/clip.mp4");
}
