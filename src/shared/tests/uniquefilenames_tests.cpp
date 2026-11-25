/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "shared/io/uniquefilenames.h"

using namespace au::shared;

TEST(UniqueFileNamesTests, KeepFirstOccurrence)
{
    UniqueFileNames names;
    EXPECT_EQ(names.registerName("Vocals"), "Vocals");
}

TEST(UniqueFileNamesTests, NumberDuplicates)
{
    UniqueFileNames names;
    names.registerName("Vocals");
    EXPECT_EQ(names.registerName("Vocals"), "Vocals-2");
    EXPECT_EQ(names.registerName("Vocals"), "Vocals-3");
}

TEST(UniqueFileNamesTests, IgnoreCase)
{
    UniqueFileNames names;
    names.registerName("Vocals");
    EXPECT_EQ(names.registerName("VOCALS"), "VOCALS-2");
}

TEST(UniqueFileNamesTests, SkipTakenSuffixes)
{
    UniqueFileNames names;
    names.registerName("Vocals");
    names.registerName("Vocals-2");
    EXPECT_EQ(names.registerName("Vocals"), "Vocals-3");
}

TEST(UniqueFileNamesTests, IgnoreUnicodeCase)
{
    UniqueFileNames names;
    names.registerName("Ä");
    EXPECT_EQ(names.registerName("ä"), "ä-2");
    names.registerName("Ünïcødé");
    EXPECT_EQ(names.registerName("ünïcødé"), "ünïcødé-2");
}

TEST(UniqueFileNamesTests, TreatCanonicallyEquivalentNamesAsDuplicates)
{
    UniqueFileNames names;
    names.registerName("\u00e9");
    EXPECT_EQ(names.registerName("e\u0301"), "e\u0301-2");
}
