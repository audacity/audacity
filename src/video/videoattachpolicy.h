/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOATTACHPOLICY_H
#define AU_VIDEO_VIDEOATTACHPOLICY_H

#include <algorithm>
#include <cctype>
#include <optional>
#include <string>
#include <vector>

//! Deciding what an import should do about a picture.
//!
//! Header-only on purpose: the project module consults this, and making it a
//! linked symbol would give project a link edge on video that breaks whenever
//! the video module is compiled out. Everything here is pure string logic, so
//! there is nothing to link.
namespace au::video {
//! Extensions the panel offers and recognises. One list, because it was
//! written out twice before this and the copies had already begun to differ.
inline const std::vector<std::string>& videoFileExtensions()
{
    static const std::vector<std::string> extensions {
        "avi", "mp4", "mkv", "mov", "flv", "wmv", "asf", "webm", "mpg",
        "mpeg", "m4v", "ts", "gxf", "mxf", "nut", "dv", "3gp", "3g2", "mj2"
    };
    return extensions;
}

//! Space-separated glob form for a file dialog filter, e.g. "*.mp4 *.mkv".
inline std::string videoFileFilter()
{
    std::string filter;
    for (const std::string& ext : videoFileExtensions()) {
        if (!filter.empty()) {
            filter += ' ';
        }
        filter += "*." + ext;
    }
    return filter;
}

inline bool hasVideoExtension(const std::string& path)
{
    if (path.empty()) {
        return false;
    }

    const size_t dot = path.find_last_of('.');
    if (dot == std::string::npos) {
        return false;
    }

    // A dot in a directory name is not an extension.
    const size_t slash = path.find_last_of("/\\");
    if (slash != std::string::npos && dot < slash) {
        return false;
    }

    std::string ext = path.substr(dot + 1);
    std::transform(ext.begin(), ext.end(), ext.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    if (ext.empty()) {
        return false;
    }

    const auto& known = videoFileExtensions();
    return std::find(known.begin(), known.end(), ext) != known.end();
}

//! Which file just imported, if any, should also have its picture attached.
//!
//! Importing a video already puts its sound on the timeline, so showing the
//! picture at the same time is what the user meant, and Detach undoes it in
//! one click. Deliberately conservative: only when nothing is attached yet, so
//! a deliberate attachment is never replaced, and only when exactly one of the
//! imported files carries video, so a bulk import does not pick a winner on
//! the user's behalf.
//!
//! The reverse - attaching a picture and importing its audio - is not done
//! here. That mutates the document and can silently duplicate a take already
//! on the timeline, which is a far worse thing to get wrong.
inline std::optional<std::string> videoToAttachAfterImport(
    const std::vector<std::string>& importedPaths, bool alreadyAttached)
{
    if (alreadyAttached) {
        return std::nullopt;
    }

    std::optional<std::string> candidate;

    for (const std::string& path : importedPaths) {
        if (!hasVideoExtension(path)) {
            continue;
        }
        if (candidate.has_value()) {
            // More than one: picking a winner would be guessing.
            return std::nullopt;
        }
        candidate = path;
    }

    return candidate;
}
}

#endif // AU_VIDEO_VIDEOATTACHPOLICY_H
