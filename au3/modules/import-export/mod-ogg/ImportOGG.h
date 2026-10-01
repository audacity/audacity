/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ImportPlugin.h"

class OggImportPlugin final : public ImportPlugin
{
public:
    OggImportPlugin();
    ~OggImportPlugin() { }

    wxString GetPluginStringID() override { return wxT("liboggvorbis"); }
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};
