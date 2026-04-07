/*
* Audacity: A Digital Audio Editor
*/

#include "filenamebpmparser.h"

#include <regex>

#include "log.h"

std::optional<double> au::importexport::getBpmFromFilename(const std::string& filename)
{
    // Ported from au3/libraries/au3-music-information-retrieval/MusicInformationRetrieval.cpp
    static const std::regex bpmRegex {
        R"((?:.*(?:_|-|\s|\.|/|\\))?(\d+)(?:_|-|\s|\.)?bpm(?:(?:_|-|\s|\.).*)?)",
        std::regex::icase
    };
    std::smatch matches;
    if (std::regex_match(filename, matches, bpmRegex)) {
        try {
            const auto value = std::stoi(matches[1]);
            if (value >= 30 && value <= 300) {
                return static_cast<double>(value);
            }
        } catch (const std::invalid_argument&) {
            IF_ASSERT_FAILED(false) {
                return std::nullopt;
            }
        } catch (const std::out_of_range&) {
            IF_ASSERT_FAILED(false) {
                return std::nullopt;
            }
        }
    }
    return std::nullopt;
}
