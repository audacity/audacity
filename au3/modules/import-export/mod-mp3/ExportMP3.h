/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.h

  Dominic Mazzoni

**********************************************************************/

#pragma once

#include "au3-import-export/ExportPlugin.h"
#include "au3-strings/TranslatableString.h"

enum MP3RateMode : unsigned {
    MODE_SET = 0,
    MODE_VBR,
    MODE_ABR,
    MODE_CBR,
};

#if defined(__WXMSW__) || defined(__WXMAC__)
#define MP3_EXPORT_BUILT_IN 1
#endif

//----------------------------------------------------------------------------
// Get MP3 library version
//----------------------------------------------------------------------------
TranslatableString GetMP3Version(bool prompt);

class ExportMP3 final : public ExportPlugin
{
public:

    ExportMP3();

    bool CheckFileName(wxFileName& filename, int format) const override;

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;

    std::vector<std::string> GetMimeTypes(int) const override;

    bool ParseConfig(
        int formatIndex, const std::string& config, ExportProcessor::Parameters& parameters) const override;
};
