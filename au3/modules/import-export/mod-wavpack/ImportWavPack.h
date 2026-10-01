/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ImportPlugin.h"

class WavPackImportPlugin final : public ImportPlugin
{
public:
    WavPackImportPlugin();
    ~WavPackImportPlugin();

    wxString GetPluginStringID() override;
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};
