/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <variant>

#include "../exportcommands.h"

namespace au::importexport {
struct WavSettings {
    WavEncoding encoding = WavEncodingContract::def;
};

struct Mp3PresetSettings {
    Mp3Preset preset = Mp3PresetContract::def;
};

struct Mp3VbrSettings {
    Mp3VbrQuality quality = Mp3VbrQualityContract::def;
};

struct Mp3AbrSettings {
    Mp3BitRate bitRate = Mp3BitRateContract::def;
};

struct Mp3CbrSettings {
    Mp3BitRate bitRate = Mp3BitRateContract::def;
};

struct FlacSettings {
    FlacBitDepth bitDepth = FlacBitDepthContract::def;
    FlacLevel level = FlacLevelContract::def;
};

struct OggSettings {
    OggQuality quality = OggQualityContract::def;
};

using ExportFormatSettings = std::variant<WavSettings, Mp3PresetSettings, Mp3VbrSettings, Mp3AbrSettings, Mp3CbrSettings,
                                          FlacSettings, OggSettings>;
}
