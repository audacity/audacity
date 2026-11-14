/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <wx/string.h>

class Tags;

namespace FFmpegMetadata {
using Entries = std::vector<std::pair<std::string, wxString> >;

//! The metadata entries to write for the tags. Standard tags use FFmpeg's generic
//! keys, which each muxer maps to its own tag names, and come last so they win
//! over additional tags FFmpeg treats as the same key, e.g. DATE.
Entries ExportEntries(const Tags& tags);

//! Returns the value stored under the key, if any
using Lookup = std::function<std::optional<wxString>(const std::string& key)>;

//! Reads the tags from FFmpeg's generic keys, falling back to the names older
//! Audacity versions exported
void ImportTags(const Lookup& lookup, Tags& tags);
}
