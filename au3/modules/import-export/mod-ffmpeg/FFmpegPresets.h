/**********************************************************************

   Audacity: A Digital Audio Editor

   FFmpegPresets.h

   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   LRN

   Vitaly Sverchinsky split from ExportFFmpegDialogs.h

**********************************************************************/

#pragma once

#include <unordered_map>
#include "XMLTagHandler.h"

class FFmpegPreset
{
public:
    FFmpegPreset();
    ~FFmpegPreset();

    wxString mPresetName;
    wxArrayString mControlState;
};

using FFmpegPresetMap = std::unordered_map<wxString, FFmpegPreset>;

class ExportFFmpegOptions;

class FFmpegPresets final : XMLTagHandler
{
public:
    FFmpegPresets();
    ~FFmpegPresets() override;

    void GetPresetList(wxArrayString& list);
    void LoadPreset(ExportFFmpegOptions* parent, wxString& name);
    bool SavePreset(ExportFFmpegOptions* parent, wxString& name);
    void DeletePreset(wxString& name);
    bool OverwriteIsOk(wxString& name);
    FFmpegPreset* FindPreset(wxString& name);

    void ImportPresets(wxString& filename);
    void ExportPresets(wxString& filename);

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;
    void WriteXMLHeader(XMLWriter& xmlFile) const;
    void WriteXML(XMLWriter& xmlFile) const;

private:

    FFmpegPresetMap mPresets;
    FFmpegPreset* mPreset; // valid during XML parsing only
    bool mAbortImport; // tells importer to ignore the rest of the import
};
