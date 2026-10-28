/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <algorithm>
#include <map>

#include "au3-tags/Tags.h"
#include "mod-ffmpeg/FFmpegMetadata.h"

//! The mapping between Audacity tags and FFmpeg metadata keys, without FFmpeg.
//! metadataroundtrip_tests.cpp checks the FFmpeg formats end to end.

namespace {
Tags makeTags(const std::vector<std::pair<wxString, wxString> >& entries)
{
    Tags tags;
    tags.Clear();
    for (const auto& [name, value] : entries) {
        tags.SetTag(name, value);
    }
    return tags;
}

std::map<std::string, wxString> toMap(const FFmpegMetadata::Entries& entries)
{
    return { entries.begin(), entries.end() };
}

FFmpegMetadata::Lookup lookupIn(const std::map<std::string, wxString>& metadata)
{
    return [metadata](const std::string& key) -> std::optional<wxString> {
        const auto it = metadata.find(key);
        if (it == metadata.end()) {
            return std::nullopt;
        }
        return it->second;
    };
}

Tags importTags(const std::map<std::string, wxString>& metadata)
{
    Tags tags;
    tags.Clear();
    FFmpegMetadata::ImportTags(lookupIn(metadata), tags);
    return tags;
}
}

TEST(FFmpegMetadataTests, ExportsStandardTagsUnderGenericKeys)
{
    const Tags tags = makeTags({
        { TAG_TITLE, wxT("Title") },
        { TAG_ARTIST, wxT("Artist") },
        { TAG_ALBUM, wxT("Album") },
        { TAG_TRACK, wxT("3") },
        { TAG_YEAR, wxT("2026") },
        { TAG_GENRE, wxT("Rock") },
        { TAG_COMMENTS, wxT("A comment") },
        { TAG_COPYRIGHT, wxT("(c) Someone") },
    });

    const std::map<std::string, wxString> expected {
        { "title", wxT("Title") },
        { "artist", wxT("Artist") },
        { "album", wxT("Album") },
        { "track", wxT("3") },
        { "date", wxT("2026") },
        { "genre", wxT("Rock") },
        { "comment", wxT("A comment") },
        { "copyright", wxT("(c) Someone") },
    };
    EXPECT_EQ(toMap(FFmpegMetadata::ExportEntries(tags)), expected);
}

TEST(FFmpegMetadataTests, MatchesStandardTagNamesCaseInsensitively)
{
    //! Importers keep the spelling a file uses, e.g. "genre" in Vorbis comments
    const Tags tags = makeTags({ { wxT("genre"), wxT("Rock") }, { wxT("copyright"), wxT("(c) Someone") } });

    const std::map<std::string, wxString> expected { { "genre", wxT("Rock") }, { "copyright", wxT("(c) Someone") } };
    EXPECT_EQ(toMap(FFmpegMetadata::ExportEntries(tags)), expected);
}

TEST(FFmpegMetadataTests, ExportsAdditionalTagsUnderTheirOwnNames)
{
    const Tags tags = makeTags({ { wxT("Mood"), wxT("Happy") } });

    const std::map<std::string, wxString> expected { { "Mood", wxT("Happy") } };
    EXPECT_EQ(toMap(FFmpegMetadata::ExportEntries(tags)), expected);
}

TEST(FFmpegMetadataTests, SkipsEmptyTags)
{
    const Tags tags = makeTags({ { TAG_TITLE, wxT("") }, { wxT("Mood"), wxT("") }, { TAG_ARTIST, wxT("Artist") } });

    const std::map<std::string, wxString> expected { { "artist", wxT("Artist") } };
    EXPECT_EQ(toMap(FFmpegMetadata::ExportEntries(tags)), expected);
}

TEST(FFmpegMetadataTests, ExportsStandardTagsAfterAdditionalOnes)
{
    //! [GIVEN] An additional DATE tag, which FFmpeg treats as the same key as Year
    const Tags tags = makeTags({
        { wxT("DATE"), wxT("2026-10-28") },
        { TAG_YEAR, wxT("2026") },
        { wxT("Mood"), wxT("Happy") },
        { TAG_TITLE, wxT("Title") },
    });

    //! [THEN] The standard tags come last, so their values are the ones FFmpeg keeps
    const FFmpegMetadata::Entries entries = FFmpegMetadata::ExportEntries(tags);
    const auto isStandard = [](const auto& entry) { return entry.first == "date" || entry.first == "title"; };
    EXPECT_TRUE(std::is_partitioned(entries.begin(), entries.end(), [&](const auto& entry) { return !isStandard(entry); }));
    EXPECT_EQ(std::count_if(entries.begin(), entries.end(), isStandard), 2);
}

TEST(FFmpegMetadataTests, ImportsGenericKeys)
{
    const Tags tags = importTags({
        { "title", wxT("Title") },
        { "artist", wxT("Artist") },
        { "album", wxT("Album") },
        { "track", wxT("3") },
        { "date", wxT("2026") },
        { "genre", wxT("Rock") },
        { "comment", wxT("A comment") },
        { "copyright", wxT("(c) Someone") },
    });

    EXPECT_EQ(tags.GetTag(TAG_TITLE), wxT("Title"));
    EXPECT_EQ(tags.GetTag(TAG_ARTIST), wxT("Artist"));
    EXPECT_EQ(tags.GetTag(TAG_ALBUM), wxT("Album"));
    EXPECT_EQ(tags.GetTag(TAG_TRACK), wxT("3"));
    EXPECT_EQ(tags.GetTag(TAG_YEAR), wxT("2026"));
    EXPECT_EQ(tags.GetTag(TAG_GENRE), wxT("Rock"));
    EXPECT_EQ(tags.GetTag(TAG_COMMENTS), wxT("A comment"));
    EXPECT_EQ(tags.GetTag(TAG_COPYRIGHT), wxT("(c) Someone"));
}

TEST(FFmpegMetadataTests, ImportsKeysOlderVersionsExported)
{
    //! Audacity used to export Artist and Year as author and year for all but AAC
    const Tags tags = importTags({ { "author", wxT("Artist") }, { "year", wxT("2026") } });

    EXPECT_EQ(tags.GetTag(TAG_ARTIST), wxT("Artist"));
    EXPECT_EQ(tags.GetTag(TAG_YEAR), wxT("2026"));
}

TEST(FFmpegMetadataTests, PrefersGenericKeysOverOlderOnes)
{
    const Tags tags = importTags({
        { "artist", wxT("New artist") },
        { "author", wxT("Old artist") },
        { "date", wxT("2026") },
        { "year", wxT("1999") },
    });

    EXPECT_EQ(tags.GetTag(TAG_ARTIST), wxT("New artist"));
    EXPECT_EQ(tags.GetTag(TAG_YEAR), wxT("2026"));
}

TEST(FFmpegMetadataTests, ImportsNothingFromEmptyMetadata)
{
    Tags tags = importTags({});

    EXPECT_TRUE(tags.IsEmpty());
}
