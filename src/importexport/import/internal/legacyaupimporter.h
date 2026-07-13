/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "io/path.h"

#include <cstddef>
#include <memory>
#include <string>
#include <vector>

class QTemporaryDir;

namespace au::importexport {
class LegacyAupImporter
{
public:
    struct Clip
    {
        muse::io::path_t filePath;
        double startTime = 0.0;
    };

    struct Track
    {
        std::string title;
        int channels = 1;
        std::vector<Clip> clips;
    };

    struct Label
    {
        double startTime = 0.0;
        double endTime = 0.0;
        std::string title;
    };

    struct LabelTrack
    {
        std::string title;
        std::vector<Label> labels;
    };

    struct Result
    {
        bool success = false;
        std::string error;
        std::vector<std::string> warnings;
        std::vector<Track> tracks;
        std::vector<LabelTrack> labelTracks;
    };

    LegacyAupImporter();
    ~LegacyAupImporter();

    static bool isLegacyAupFile(const muse::io::path_t& filePath);

    Result resolve(const muse::io::path_t& filePath);

private:
    struct Block;
    struct SourceClip;
    struct SourceTrack;

    Result parse(const muse::io::path_t& filePath, std::vector<SourceTrack>& sourceTracks, int& projectRate);
    bool render(Result& result, const std::vector<SourceTrack>& sourceTracks, int projectRate);

    std::vector<float> readBlock(const Block& block, std::vector<std::string>& warnings) const;
    bool writeMonoClip(const SourceTrack& track, const SourceClip& clip, const muse::io::path_t& path,
                       std::vector<std::string>& warnings) const;
    bool writeStereoClip(const SourceTrack& leftTrack, const SourceClip& leftClip, const SourceTrack& rightTrack,
                         const SourceClip& rightClip, const muse::io::path_t& path, std::vector<std::string>& warnings) const;

    muse::io::path_t makeTempWavPath(size_t trackIndex, size_t clipIndex) const;

    std::unique_ptr<QTemporaryDir> m_tempDir;
};
}
