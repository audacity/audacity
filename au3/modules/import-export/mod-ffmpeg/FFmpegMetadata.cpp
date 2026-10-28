/*
* Audacity: A Digital Audio Editor
*/
#include "FFmpegMetadata.h"

#include <map>

#include "au3-tags/Tags.h"

namespace {
struct CaseInsensitiveLess {
    bool operator()(const wxString& a, const wxString& b) const { return a.CmpNoCase(b) < 0; }
};

const std::map<wxString, std::string, CaseInsensitiveLess> GENERIC_KEYS {
    { TAG_TITLE, "title" },
    { TAG_ARTIST, "artist" },
    { TAG_ALBUM, "album" },
    { TAG_TRACK, "track" },
    { TAG_YEAR, "date" },
    { TAG_GENRE, "genre" },
    { TAG_COMMENTS, "comment" },
    { TAG_COPYRIGHT, "copyright" },
};

void readTag(const FFmpegMetadata::Lookup& lookup, Tags& tags, const wxString& tag, const std::vector<std::string>& keys)
{
    for (const std::string& key : keys) {
        if (const std::optional<wxString> value = lookup(key)) {
            tags.SetTag(tag, *value);
            return;
        }
    }
}
}

FFmpegMetadata::Entries FFmpegMetadata::ExportEntries(const Tags& tags)
{
    Entries additional;
    Entries standard;
    for (const auto& [name, value] : tags.GetRange()) {
        if (value.empty()) {
            continue;
        }

        const auto it = GENERIC_KEYS.find(name);
        if (it == GENERIC_KEYS.end()) {
            additional.emplace_back(name.ToStdString(), value);
        } else {
            standard.emplace_back(it->second, value);
        }
    }

    additional.insert(additional.end(), standard.begin(), standard.end());
    return additional;
}

void FFmpegMetadata::ImportTags(const Lookup& lookup, Tags& tags)
{
    readTag(lookup, tags, TAG_TITLE, { "title" });
    readTag(lookup, tags, TAG_COMMENTS, { "comment" });
    readTag(lookup, tags, TAG_ALBUM, { "album" });
    readTag(lookup, tags, TAG_TRACK, { "track" });
    readTag(lookup, tags, TAG_GENRE, { "genre" });
    readTag(lookup, tags, TAG_ARTIST, { "artist", "author" });
    readTag(lookup, tags, TAG_YEAR, { "date", "year" });
}
