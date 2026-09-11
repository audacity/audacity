/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ImportPlugin.h"

class FLACImportPlugin final : public ImportPlugin
{
public:
    FLACImportPlugin();
    ~FLACImportPlugin() { }

    wxString GetPluginStringID() override { return wxT("libflac"); }
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*)  override;
};
