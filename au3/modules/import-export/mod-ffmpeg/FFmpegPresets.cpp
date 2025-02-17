/**********************************************************************

   Audacity: A Digital Audio Editor

   FFmpegPresets.cpp

   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   LRN

   Vitaly Sverchinsky split from ExportFFmpegDialogs.cpp

**********************************************************************/

#include "FFmpegPresets.h"

#include <wx/spinctrl.h>
#include <wx/listbox.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/textctrl.h>

#include "FFmpegDefines.h"
#include "ExportFFmpegOptions.h"

#include "XMLFileReader.h"
#include "AudacityMessageBox.h"

FFmpegPreset::FFmpegPreset()
{
    mControlState.resize(FELastID - FEFirstID);
}

FFmpegPreset::~FFmpegPreset()
{
}

FFmpegPresets::FFmpegPresets()
{
    mPreset = NULL;
    mAbortImport = false;

    XMLFileReader xmlfile;
    wxFileName xmlFileName(FileNames::DataDir(), wxT("ffmpeg_presets.xml"));
    xmlfile.Parse(this, xmlFileName.GetFullPath());
}

FFmpegPresets::~FFmpegPresets()
{
    // We're in a destructor!  Don't let exceptions out!
    GuardedCall([&] {
        wxFileName xmlFileName { FileNames::DataDir(), wxT("ffmpeg_presets.xml") };
        XMLFileWriter writer {
            xmlFileName.GetFullPath(), XO("Error Saving FFmpeg Presets") };
        WriteXMLHeader(writer);
        WriteXML(writer);
        writer.Commit();
    });
}

void FFmpegPresets::ImportPresets(wxString& filename)
{
    mPreset = NULL;
    mAbortImport = false;

    FFmpegPresetMap savePresets = mPresets;

    XMLFileReader xmlfile;
    bool success = xmlfile.Parse(this, filename);
    if (!success || mAbortImport) {
        mPresets = savePresets;
    }
}

void FFmpegPresets::ExportPresets(wxString& filename)
{
    GuardedCall([&] {
        XMLFileWriter writer { filename, XO("Error Saving FFmpeg Presets") };
        WriteXMLHeader(writer);
        WriteXML(writer);
        writer.Commit();
    });
}

void FFmpegPresets::GetPresetList(wxArrayString& list)
{
    list.clear();
    FFmpegPresetMap::iterator iter;
    for (iter = mPresets.begin(); iter != mPresets.end(); ++iter) {
        list.push_back(iter->second.mPresetName);
    }

    std::sort(list.begin(), list.end());
}

void FFmpegPresets::DeletePreset(wxString& name)
{
    FFmpegPresetMap::iterator iter = mPresets.find(name);
    if (iter != mPresets.end()) {
        mPresets.erase(iter);
    }
}

FFmpegPreset* FFmpegPresets::FindPreset(wxString& name)
{
    FFmpegPresetMap::iterator iter = mPresets.find(name);
    if (iter != mPresets.end()) {
        return &iter->second;
    }

    return NULL;
}

// return false if overwrite was not allowed.
bool FFmpegPresets::OverwriteIsOk(wxString& name)
{
    FFmpegPreset* preset = FindPreset(name);
    if (preset) {
        auto query = XO("Overwrite preset '%s'?").Format(name);
        int action = AudacityMessageBox(
            query,
            XO("Confirm Overwrite"),
            wxYES_NO | wxCENTRE);
        if (action == wxNO) {
            return false;
        }
    }
    return true;
}

bool FFmpegPresets::SavePreset(ExportFFmpegOptions* parent, wxString& name)
{
    wxString format;
    wxString codec;
    FFmpegPreset* preset;

    {
        wxWindow* wnd;
        wxListBox* lb;

        wnd = dynamic_cast<wxWindow*>(parent)->FindWindowById(FEFormatID, parent);
        lb = dynamic_cast<wxListBox*>(wnd);
        if (lb->GetSelection() < 0) {
            AudacityMessageBox(XO("Please select format before saving a profile"));
            return false;
        }
        format = lb->GetStringSelection();

        wnd = dynamic_cast<wxWindow*>(parent)->FindWindowById(FECodecID, parent);
        lb = dynamic_cast<wxListBox*>(wnd);
        if (lb->GetSelection() < 0) {
            /* i18n-hint: "codec" is short for a "coder-decoder" algorithm */
            AudacityMessageBox(XO("Please select codec before saving a profile"));
            return false;
        }
        codec = lb->GetStringSelection();
    }

    preset = &mPresets[name];
    preset->mPresetName = name;

    wxSpinCtrl* sc;
    wxTextCtrl* tc;
    wxCheckBox* cb;
    wxChoice* ch;

    for (int id = FEFirstID; id < FELastID; id++) {
        wxWindow* wnd = dynamic_cast<wxWindow*>(parent)->FindWindowById(id, parent);
        if (wnd != NULL) {
            switch (id) {
            case FEFormatID:
                preset->mControlState[id - FEFirstID] = format;
                break;
            case FECodecID:
                preset->mControlState[id - FEFirstID] = codec;
                break;
            // Spin control
            case FEBitrateID:
            case FEQualityID:
            case FESampleRateID:
            case FECutoffID:
            case FEFrameSizeID:
            case FEBufSizeID:
            case FECompLevelID:
            case FELPCCoeffsID:
            case FEMinPredID:
            case FEMaxPredID:
            case FEMinPartOrderID:
            case FEMaxPartOrderID:
            case FEMuxRateID:
            case FEPacketSizeID:
                sc = dynamic_cast<wxSpinCtrl*>(wnd);
                preset->mControlState[id - FEFirstID] = wxString::Format(wxT("%d"), sc->GetValue());
                break;
            // Text control
            case FELanguageID:
            case FETagID:
                tc = dynamic_cast<wxTextCtrl*>(wnd);
                preset->mControlState[id - FEFirstID] = tc->GetValue();
                break;
            // Choice
            case FEProfileID:
            case FEPredOrderID:
                ch = dynamic_cast<wxChoice*>(wnd);
                preset->mControlState[id - FEFirstID] = wxString::Format(wxT("%d"), ch->GetSelection());
                break;
            // Check box
            case FEUseLPCID:
            case FEBitReservoirID:
            case FEVariableBlockLenID:
                cb = dynamic_cast<wxCheckBox*>(wnd);
                preset->mControlState[id - FEFirstID] = wxString::Format(wxT("%d"), cb->GetValue());
                break;
            }
        }
    }
    return true;
}

void FFmpegPresets::LoadPreset(ExportFFmpegOptions* parent, wxString& name)
{
    FFmpegPreset* preset = FindPreset(name);
    if (!preset) {
        AudacityMessageBox(XO("Preset '%s' does not exist.").Format(name));
        return;
    }

    wxListBox* lb;
    wxSpinCtrl* sc;
    wxTextCtrl* tc;
    wxCheckBox* cb;
    wxChoice* ch;

    for (int id = FEFirstID; id < FELastID; id++) {
        wxWindow* wnd = parent->FindWindowById(id, parent);
        if (wnd != NULL) {
            wxString readstr;
            long readlong;
            bool readbool;
            switch (id) {
            // Listbox
            case FEFormatID:
            case FECodecID:
                lb = dynamic_cast<wxListBox*>(wnd);
                readstr = preset->mControlState[id - FEFirstID];
                readlong = lb->FindString(readstr);
                if (readlong > -1) {
                    lb->Select(readlong);
                }
                break;
            // Spin control
            case FEBitrateID:
            case FEQualityID:
            case FESampleRateID:
            case FECutoffID:
            case FEFrameSizeID:
            case FEBufSizeID:
            case FECompLevelID:
            case FELPCCoeffsID:
            case FEMinPredID:
            case FEMaxPredID:
            case FEMinPartOrderID:
            case FEMaxPartOrderID:
            case FEMuxRateID:
            case FEPacketSizeID:
                sc = dynamic_cast<wxSpinCtrl*>(wnd);
                preset->mControlState[id - FEFirstID].ToLong(&readlong);
                sc->SetValue(readlong);
                break;
            // Text control
            case FELanguageID:
            case FETagID:
                tc = dynamic_cast<wxTextCtrl*>(wnd);
                tc->SetValue(preset->mControlState[id - FEFirstID]);
                break;
            // Choice
            case FEProfileID:
            case FEPredOrderID:
                ch = dynamic_cast<wxChoice*>(wnd);
                preset->mControlState[id - FEFirstID].ToLong(&readlong);
                if (readlong > -1) {
                    ch->Select(readlong);
                }
                break;
            // Check box
            case FEUseLPCID:
            case FEBitReservoirID:
            case FEVariableBlockLenID:
                cb = dynamic_cast<wxCheckBox*>(wnd);
                preset->mControlState[id - FEFirstID].ToLong(&readlong);
                if (readlong) {
                    readbool = true;
                } else {
                    readbool = false;
                }
                cb->SetValue(readbool);
                break;
            }
        }
    }
}

bool FFmpegPresets::HandleXMLTag(const std::string_view& tag, const AttributesList& attrs)
{
    if (mAbortImport) {
        return false;
    }

    if (tag == "ffmpeg_presets") {
        return true;
    }

    if (tag == "preset") {
        for (auto pair : attrs) {
            auto attr = pair.first;
            auto value = pair.second;

            if (attr == "name") {
                wxString strValue = value.ToWString();
                mPreset = FindPreset(strValue);

                if (mPreset) {
                    auto query = XO("Replace preset '%s'?").Format(strValue);
                    int action = AudacityMessageBox(
                        query,
                        XO("Confirm Overwrite"),
                        wxYES_NO | wxCANCEL | wxCENTRE);
                    if (action == wxCANCEL) {
                        mAbortImport = true;
                        return false;
                    }
                    if (action == wxNO) {
                        mPreset = NULL;
                        return false;
                    }
                    *mPreset = FFmpegPreset();
                } else {
                    mPreset = &mPresets[strValue];
                }

                mPreset->mPresetName = strValue;
            }
        }
        return true;
    }

    if (tag == "setctrlstate" && mPreset) {
        long id = -1;
        for (auto pair : attrs) {
            auto attr = pair.first;
            auto value = pair.second;

            if (attr == "id") {
                for (long i = FEFirstID; i < FELastID; i++) {
                    if (!wxStrcmp(FFmpegExportCtrlIDNames[i - FEFirstID], value.ToWString())) {
                        id = i;
                    }
                }
            } else if (attr == "state") {
                if (id > FEFirstID && id < FELastID) {
                    mPreset->mControlState[id - FEFirstID] = value.ToWString();
                }
            }
        }
        return true;
    }

    return false;
}

XMLTagHandler* FFmpegPresets::HandleXMLChild(const std::string_view& tag)
{
    if (mAbortImport) {
        return NULL;
    }

    if (tag == "preset") {
        return this;
    } else if (tag == "setctrlstate") {
        return this;
    }
    return NULL;
}

void FFmpegPresets::WriteXMLHeader(XMLWriter& xmlFile) const
// may throw
{
    xmlFile.Write(wxT("<?xml "));
    xmlFile.Write(wxT("version=\"1.0\" "));
    xmlFile.Write(wxT("standalone=\"no\" "));
    xmlFile.Write(wxT("?>\n"));

    wxString dtdName = wxT("-//audacityffmpegpreset-1.0.0//DTD//EN");
    wxString dtdURI
        =wxT("http://audacity.sourceforge.net/xml/audacityffmpegpreset-1.0.0.dtd");

    xmlFile.Write(wxT("<!DOCTYPE "));
    xmlFile.Write(wxT("project "));
    xmlFile.Write(wxT("PUBLIC "));
    xmlFile.Write(wxT("\"-//audacityffmpegpreset-1.0.0//DTD//EN\" "));
    xmlFile.Write(wxT("\"http://audacity.sourceforge.net/xml/audacityffmpegpreset-1.0.0.dtd\" "));
    xmlFile.Write(wxT(">\n"));
}

void FFmpegPresets::WriteXML(XMLWriter& xmlFile) const
// may throw
{
    xmlFile.StartTag(wxT("ffmpeg_presets"));
    xmlFile.WriteAttr(wxT("version"), wxT("1.0"));
    FFmpegPresetMap::const_iterator iter;
    for (iter = mPresets.begin(); iter != mPresets.end(); ++iter) {
        auto preset = &iter->second;
        xmlFile.StartTag(wxT("preset"));
        xmlFile.WriteAttr(wxT("name"), preset->mPresetName);
        for (long i = FEFirstID + 1; i < FELastID; i++) {
            xmlFile.StartTag(wxT("setctrlstate"));
            xmlFile.WriteAttr(wxT("id"), wxString(FFmpegExportCtrlIDNames[i - FEFirstID]));
            xmlFile.WriteAttr(wxT("state"), preset->mControlState[i - FEFirstID]);
            xmlFile.EndTag(wxT("setctrlstate"));
        }
        xmlFile.EndTag(wxT("preset"));
    }
    xmlFile.EndTag(wxT("ffmpeg_presets"));
}
