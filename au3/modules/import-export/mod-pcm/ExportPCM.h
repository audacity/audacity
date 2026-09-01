/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ExportPlugin.h"

class ExportPCM final : public ExportPlugin
{
public:

    ExportPCM();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int index) const override;

    std::vector<std::string> GetMimeTypes(int formatIndex) const override;

    bool ParseConfig(int formatIndex, const std::string&, ExportProcessor::Parameters& parameters) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    /**
     *
     * @param format Control whether we are doing a "preset" export to a popular
     * file type, or giving the user full control over libsndfile.
     */
    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};
