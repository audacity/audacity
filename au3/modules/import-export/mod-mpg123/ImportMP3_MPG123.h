/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ImportPlugin.h"

class MP3ImportPlugin final : public ImportPlugin
{
public:
    MP3ImportPlugin();

    wxString GetPluginStringID() override;

    TranslatableString GetPluginFormatDescription() override;

    std::unique_ptr<ImportFileHandle> Open(const FilePath& Filename, AudacityProject*) override;
};
