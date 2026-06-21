#include "overwriteoriginalsettings.h"

#include <string>

#include "au3-import-export/ExportOptionsEditor.h"

namespace au::importexport {
namespace {
constexpr auto FLACOptionIDBitDepth = 0;
constexpr auto MP3OptionIDMode = 0;
constexpr auto MP3OptionIDQualityVBR = 2;
constexpr auto WavPackOptionIDBitDepth = 1;
constexpr auto WavPackOptionIDHybridMode = 2;
constexpr auto WavPackOptionIDCreateCorrection = 3;

bool applyMp3CodecSettings(::ExportOptionsEditor& editor, const QVariantMap& codecSettings)
{
    // MP3ExportOptionsEditor::GetName() returns "mp3"; its option ids are
    // MP3OptionIDMode == 0 and MP3OptionIDQualityVBR == 2 in ExportMP3.cpp.
    if (editor.GetName() != "mp3") {
        return false;
    }

    const auto vbrPreset = Mp3VbrPresetForBitRate(codecSettings.value("bitRate").toDouble());
    if (!vbrPreset) {
        return false;
    }

    if (!editor.SetValue(MP3OptionIDMode, std::string("VBR"))) {
        return false;
    }
    if (!editor.SetValue(MP3OptionIDQualityVBR, *vbrPreset)) {
        return false;
    }
    return true;
}
}

std::optional<int> Mp3VbrPresetForBitRate(double bitRate)
{
    if (bitRate <= 0.0) {
        return std::nullopt;
    }

    const double kbps = bitRate > 1000.0 ? bitRate / 1000.0 : bitRate;
    if (kbps >= 230.0) {
        return 0;
    }
    if (kbps >= 200.0) {
        return 1;
    }
    if (kbps >= 180.0) {
        return 2;
    }
    if (kbps >= 170.0) {
        return 3;
    }
    if (kbps >= 140.0) {
        return 4;
    }
    if (kbps >= 120.0) {
        return 5;
    }
    return 6;
}

bool ApplyCodecSettingsToExportOptions(::ExportOptionsEditor& editor, const QVariantMap& codecSettings)
{
    const auto format = codecSettings.value("format").toString();
    if (format == "MP3") {
        return applyMp3CodecSettings(editor, codecSettings);
    }
    if (format == "FLAC") {
        const int depth = codecSettings.value("bitDepth").toInt();
        return (depth == 16 || depth == 24) && editor.SetValue(FLACOptionIDBitDepth, std::to_string(depth));
    }

    if (format == "WavPack") {
        return editor.SetValue(WavPackOptionIDBitDepth, codecSettings.value("bitDepth").toInt())
               && editor.SetValue(WavPackOptionIDHybridMode, false)
               && editor.SetValue(WavPackOptionIDCreateCorrection, false);
    }
    return true;
}
}
