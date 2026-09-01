/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ImportPlugin.h"

class PCMImportPlugin final : public ImportPlugin
{
public:
    PCMImportPlugin();
    ~PCMImportPlugin();

    wxString GetPluginStringID() override;
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};
