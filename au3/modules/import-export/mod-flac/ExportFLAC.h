/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ExportPlugin.h"

class ExportFLAC final : public ExportPlugin
{
public:

    ExportFLAC();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    bool ParseConfig(int, const std::string& config, ExportProcessor::Parameters& parameters) const override;

    std::vector<std::string> GetMimeTypes(int) const override;

    // Required

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};
