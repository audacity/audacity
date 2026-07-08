#pragma once

#include <optional>

#include <QVariantMap>

class ExportOptionsEditor;

namespace au::importexport {
std::optional<int> Mp3VbrPresetForBitRate(double bitRate);
void ApplyCodecSettingsToExportOptions(::ExportOptionsEditor& editor, const QVariantMap& codecSettings);
}
