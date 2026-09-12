// SPDX-License-Identifier: GPL-3.0-only
// MuseScore-CLA-applies

#include <gtest/gtest.h>

#include "internal/projectpathutils.h"

using au::project::forceAup4Extension;

TEST(ProjectPathUtilsTests, ReplacesMissingOrDifferentExtension)
{
    EXPECT_EQ(forceAup4Extension("/projects/session"), "/projects/session.aup4");
    EXPECT_EQ(forceAup4Extension("/projects/session.wav"), "/projects/session.aup4");
    EXPECT_EQ(forceAup4Extension("/projects.with.dots/session"), "/projects.with.dots/session.aup4");
}

TEST(ProjectPathUtilsTests, PreservesOneProjectExtension)
{
    EXPECT_EQ(forceAup4Extension("/projects/session.aup4"), "/projects/session.aup4");
}

TEST(ProjectPathUtilsTests, CollapsesRepeatedProjectExtensions)
{
    EXPECT_EQ(forceAup4Extension("/projects/session.aup4.aup4"), "/projects/session.aup4");
    EXPECT_EQ(forceAup4Extension("/projects/session.AUP4.aup4"), "/projects/session.aup4");
}
