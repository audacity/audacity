/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ExportPlugin.h"

inline unsigned DefaultWavPackChannelMask(unsigned channels)
{
    if (channels <= 2) {
        return 0x5 - channels;
    }
    return channels <= 18 ? (1U << channels) - 1 : 0x3FFFF;
}

class ExportWavPack final : public ExportPlugin
{
public:

    ExportWavPack();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    std::vector<std::string> GetMimeTypes(int) const override;

    bool ParseConfig(int formatIndex, const std::string& config, ExportProcessor::Parameters& parameters) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};
