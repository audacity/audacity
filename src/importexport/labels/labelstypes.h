/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::importexport {
enum class FileType {
    UNDEFINED,
    TEXT,
    SUBRIP,
    WEBVTT
};

struct FileFilter {
    FileType type = FileType::UNDEFINED;
    std::string title;
};
}
