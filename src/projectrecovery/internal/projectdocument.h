/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "binaryxml.h"

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace au::projectrecovery {
struct WaveBlock {
    int64_t start = 0;
    //! A value of 0 or less is silence of that many samples, without a row
    int64_t blockId = 0;
    //! The start of the next block, or the sample count of the sequence, minus the start
    int64_t length = 0;
    //! Where the blockid value starts in the dict and the doc, read as one stream
    size_t blockIdOffset = 0;
};

struct Sequence {
    int64_t maxSamples = 0;
    uint32_t sampleFormat = 0;
    int64_t numSamples = 0;
    std::vector<WaveBlock> blocks;
};

struct WaveClip {
    std::string name;
    double offset = 0;
    double trimLeft = 0;
    double trimRight = 0;
    double stretchRatio = 1;
    //! The tempo attributes of files from before 4.0.1, which the stretch ratio did not include
    std::optional<double> rawAudioTempo;
    std::optional<double> clipTempo;
    //! True when the attribute is absent, as in WaveClip
    bool stretchToMatchTempo = true;
    //! One sequence per channel
    std::vector<Sequence> sequences;
    std::vector<WaveClip> cutLines;
};

struct WaveTrack {
    std::string name;
    int channel = 0;
    int64_t linked = 0;
    double rate = 0;
    std::vector<WaveClip> clips;
};

struct ProjectDocument {
    std::string version;
    std::string audacityVersion;
    double rate = 0;
    //! ProjectTimeSignature uses 120 when the attribute is absent
    double tempo = 120;
    std::vector<WaveTrack> waveTracks;
};

//! The parts of the document that recovery needs; other elements are ignored
std::optional<ProjectDocument> readProjectDocument(const XmlDocument& document);

//! The stretch that WaveClip applies after it reads the clip
double effectiveStretchRatio(const WaveClip& clip, const ProjectDocument& project);
}
