/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ImportPlugin.h"

class FFmpegImportPlugin final : public ImportPlugin
{
public:
    FFmpegImportPlugin();
    ~FFmpegImportPlugin();

    wxString GetPluginStringID() override;
    TranslatableString GetPluginFormatDescription() override;
    TranslatableString FailureHint() const override;

    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};
