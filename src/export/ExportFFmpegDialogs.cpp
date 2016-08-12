/**********************************************************************

   Audacity: A Digital Audio Editor

   ExportFFmpegDialogs.cpp

   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   LRN

******************************************************************//**

\class ExportFFmpegAC3Options
\brief Options dialog for FFmpeg exporting of AC3 format.

*//***************************************************************//**

\class ExportFFmpegAACOptions
\brief Options dialog for FFmpeg exporting of AAC format.

*//***************************************************************//**

\class ExportFFmpegAMRNBOptions
\brief Options dialog for FFmpeg exporting of AMRNB format.

*//***************************************************************//**

\class ExportFFmpegWMAOptions
\brief Options dialog for FFmpeg exporting of WMA format.

*//***************************************************************//**

\class ExportFFmpegOptions
\brief Options dialog for Custom FFmpeg export format.

*//*******************************************************************/

#include "../Audacity.h"   // keep ffmpeg before wx because they interact
#include "../FFmpeg.h"     // and Audacity.h before FFmpeg for config*.h

#include "ExportFFmpegDialogs.h"

#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/listbox.h>
#include <wx/window.h>
#include <wx/spinctrl.h>
#include <wx/combobox.h>
#include <wx/listimpl.cpp>

#include "../FileFormats.h"
#include "../Internat.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"

#include "Export.h"

#if defined(USE_FFMPEG)

extern FFmpegLibs *FFmpegLibsInst();

/// This construction defines a enumeration of UI element IDs, and a static
/// array of their string representations (this way they're always synchronized).
/// Do not store the enumerated values in external files, as they may change;
/// the strings may be stored.
#define FFMPEG_EXPORT_CTRL_ID_ENTRIES \
   FFMPEG_EXPORT_CTRL_ID_FIRST_ENTRY(FEFirstID, 20000), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEFormatID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FECodecID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEBitrateID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEQualityID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FESampleRateID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FELanguageID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FETagID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FECutoffID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEFrameSizeID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEBufSizeID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEProfileID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FECompLevelID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEUseLPCID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FELPCCoeffsID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEMinPredID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEMaxPredID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEPredOrderID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEMinPartOrderID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEMaxPartOrderID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEMuxRateID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEPacketSizeID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEBitReservoirID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEVariableBlockLenID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FELastID), \
 \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEFormatLabelID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FECodecLabelID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEFormatNameID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FECodecNameID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEPresetID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FESavePresetID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FELoadPresetID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEDeletePresetID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEAllFormatsID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEAllCodecsID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEImportPresetsID), \
   FFMPEG_EXPORT_CTRL_ID_ENTRY(FEExportPresetsID) \

// First the enumeration
#define FFMPEG_EXPORT_CTRL_ID_FIRST_ENTRY(name, num)  name = num
#define FFMPEG_EXPORT_CTRL_ID_ENTRY(name)             name

enum FFmpegExportCtrlID {
   FFMPEG_EXPORT_CTRL_ID_ENTRIES
};

// Now the string representations
#undef FFMPEG_EXPORT_CTRL_ID_FIRST_ENTRY
#define FFMPEG_EXPORT_CTRL_ID_FIRST_ENTRY(name, num)  wxT(#name)
#undef FFMPEG_EXPORT_CTRL_ID_ENTRY
#define FFMPEG_EXPORT_CTRL_ID_ENTRY(name)             wxT(#name)
static const wxChar *FFmpegExportCtrlIDNames[] = {
   FFMPEG_EXPORT_CTRL_ID_ENTRIES
};

#undef FFMPEG_EXPORT_CTRL_ID_ENTRIES
#undef FFMPEG_EXPORT_CTRL_ID_ENTRY
#undef FFMPEG_EXPORT_CTRL_ID_FIRST_ENTRY

//----------------------------------------------------------------------------
// ExportFFmpegAC3Options Class
//----------------------------------------------------------------------------

// This initialises content for the static const member variables defined in
// ExportFFmpegDialogs.h (note no static keyword - important!)
const int ExportFFmpegAC3Options::iAC3BitRates[] = { 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 320000, 384000, 448000, 512000, 576000, 640000 };
const int ExportFFmpegAC3Options::iAC3SampleRates[] = { 32000, 44100, 48000, 0 };

ExportFFmpegAC3Options::ExportFFmpegAC3Options(wxWindow *parent, int WXUNUSED(format))
:  wxPanelWrapper(parent, wxID_ANY)
{
   for (unsigned int i=0; i < (sizeof(iAC3BitRates)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(_("%i kbps"),iAC3BitRates[i]/1000));
      mBitRateLabels.Add(iAC3BitRates[i]);
   }

   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

ExportFFmpegAC3Options::~ExportFFmpegAC3Options()
{
   TransferDataFromWindow();
}

///
///
void ExportFFmpegAC3Options::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxCENTER);
      {
         S.StartMultiColumn(2, wxCENTER);
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/AC3BitRate"),
               160000, mBitRateNames, mBitRateLabels);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}

///
///
bool ExportFFmpegAC3Options::TransferDataToWindow()
{
   return true;
}

///
///
bool ExportFFmpegAC3Options::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Flush();

   return true;
}

//----------------------------------------------------------------------------
// ExportFFmpegAACOptions Class
//----------------------------------------------------------------------------

ExportFFmpegAACOptions::ExportFFmpegAACOptions(wxWindow *parent, int WXUNUSED(format))
:  wxPanelWrapper(parent, wxID_ANY)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

ExportFFmpegAACOptions::~ExportFFmpegAACOptions()
{
   TransferDataFromWindow();
}

///
///
void ExportFFmpegAACOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetSizerProportion(1);
         S.StartMultiColumn(2, wxCENTER);
         {
            S.SetStretchyCol(1);
            S.Prop(1).TieSlider(_("Quality:"),wxT("/FileFormats/AACQuality"),100,500,10);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}

///
///
bool ExportFFmpegAACOptions::TransferDataToWindow()
{
   return true;
}

///
///
bool ExportFFmpegAACOptions::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Flush();

   return true;
}

//----------------------------------------------------------------------------
// ExportFFmpegAMRNBOptions Class
//----------------------------------------------------------------------------

/// Bit Rates supported by libAMR-NB encoder
/// Sample Rate is always 8 kHz
int ExportFFmpegAMRNBOptions::iAMRNBBitRate[] =
{ 4750, 5150, 5900, 6700, 7400, 7950, 10200, 12200 };

ExportFFmpegAMRNBOptions::ExportFFmpegAMRNBOptions(wxWindow *parent, int WXUNUSED(format))
:  wxPanelWrapper(parent, wxID_ANY)
{
   for (unsigned int i=0; i < (sizeof(iAMRNBBitRate)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(_("%.2f kbps"),(float)iAMRNBBitRate[i]/1000));
      mBitRateLabels.Add(iAMRNBBitRate[i]);
   }

   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

ExportFFmpegAMRNBOptions::~ExportFFmpegAMRNBOptions()
{
   TransferDataFromWindow();
}

///
///
void ExportFFmpegAMRNBOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxCENTER);
      {
         S.StartMultiColumn(2, wxCENTER);
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/AMRNBBitRate"),
               12200, mBitRateNames, mBitRateLabels);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}

///
///
bool ExportFFmpegAMRNBOptions::TransferDataToWindow()
{
   return true;
}

///
///
bool ExportFFmpegAMRNBOptions::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Flush();

   return true;
}

//----------------------------------------------------------------------------
// ExportFFmpegWMAOptions Class
//----------------------------------------------------------------------------

const int ExportFFmpegWMAOptions::iWMASampleRates[] =
{ 8000, 11025, 16000, 22050, 44100, 0};

/// Bit Rates supported by WMA encoder. Setting bit rate to other values will not result in different file size.
const int ExportFFmpegWMAOptions::iWMABitRate[] =
{ 24000, 32000, 40000, 48000, 64000, 80000, 96000, 128000, 160000, 192000, 256000, 320000 };

ExportFFmpegWMAOptions::ExportFFmpegWMAOptions(wxWindow *parent, int WXUNUSED(format))
:  wxPanelWrapper(parent, wxID_ANY)
{
   for (unsigned int i=0; i < (sizeof(iWMABitRate)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i kbps"),iWMABitRate[i]/1000));
      mBitRateLabels.Add(iWMABitRate[i]);
   }

   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

ExportFFmpegWMAOptions::~ExportFFmpegWMAOptions()
{
   TransferDataFromWindow();
}

///
///
void ExportFFmpegWMAOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxCENTER);
      {
         S.StartMultiColumn(2, wxCENTER);
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/WMABitRate"),
               128000, mBitRateNames, mBitRateLabels);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}

///
///
bool ExportFFmpegWMAOptions::TransferDataToWindow()
{
   return true;
}

///
///
bool ExportFFmpegWMAOptions::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Flush();

   return true;
}

//----------------------------------------------------------------------------
// ExportFFmpegCustomOptions Class
//----------------------------------------------------------------------------

#define OpenID 9000

BEGIN_EVENT_TABLE(ExportFFmpegCustomOptions, wxPanelWrapper)
   EVT_BUTTON(OpenID, ExportFFmpegCustomOptions::OnOpen)
END_EVENT_TABLE()

ExportFFmpegCustomOptions::ExportFFmpegCustomOptions(wxWindow *parent, int WXUNUSED(format))
:  wxPanelWrapper(parent, wxID_ANY)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

ExportFFmpegCustomOptions::~ExportFFmpegCustomOptions()
{
   TransferDataFromWindow();
}

///
///
void ExportFFmpegCustomOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxCENTER);
   {
      S.StartHorizontalLay(wxCENTER, 0);
      {
         S.Id(OpenID).AddButton(_("Open custom FFmpeg format options"));
      }
      S.EndHorizontalLay();
   }
   S.EndHorizontalLay();
}

///
///
bool ExportFFmpegCustomOptions::TransferDataToWindow()
{
   return true;
}

///
///
bool ExportFFmpegCustomOptions::TransferDataFromWindow()
{
   return true;
}

///
///
void ExportFFmpegCustomOptions::OnOpen(wxCommandEvent & WXUNUSED(evt))
{
   // Show "Locate FFmpeg" dialog
   PickFFmpegLibs();
   if (!FFmpegLibsInst()->ValidLibsLoaded())
   {
      FFmpegLibsInst()->FindLibs(NULL);
      FFmpegLibsInst()->FreeLibs();
      if (!LoadFFmpeg(true))
      {
         return;
      }
   }
   DropFFmpegLibs();

   ExportFFmpegOptions od(wxGetTopLevelParent(this));
   od.ShowModal();
}

FFmpegPreset::FFmpegPreset()
{
   mControlState.SetCount(FELastID - FEFirstID);
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
   xmlfile.Parse(this,xmlFileName.GetFullPath());
}

FFmpegPresets::~FFmpegPresets()
{
   XMLFileWriter writer;
   // FIXME: TRAP_ERR Catch XMLFileWriterException
   wxFileName xmlFileName(FileNames::DataDir(), wxT("ffmpeg_presets.xml"));
   writer.Open(xmlFileName.GetFullPath(),wxT("wb"));
   WriteXMLHeader(writer);
   WriteXML(writer);
}

void FFmpegPresets::ImportPresets(wxString &filename)
{
   mPreset = NULL;
   mAbortImport = false;

   FFmpegPresetMap savePresets = mPresets;

   XMLFileReader xmlfile;
   bool success = xmlfile.Parse(this,filename);
   if (!success || mAbortImport) {
      mPresets = savePresets;
   }
}

void FFmpegPresets::ExportPresets(wxString &filename)
{
   XMLFileWriter writer;
   // FIXME: TRAP_ERR Catch XMLFileWriterException
   writer.Open(filename,wxT("wb"));
   WriteXMLHeader(writer);
   WriteXML(writer);
}

void FFmpegPresets::GetPresetList(wxArrayString &list)
{
   list.Clear();
   FFmpegPresetMap::iterator iter;
   for (iter = mPresets.begin(); iter != mPresets.end(); ++iter)
   {
      list.Add(iter->second.mPresetName);
   }

   list.Sort();
}

void FFmpegPresets::DeletePreset(wxString &name)
{
   FFmpegPresetMap::iterator iter = mPresets.find(name);
   if (iter != mPresets.end())
   {
      mPresets.erase(iter);
   }
}

FFmpegPreset *FFmpegPresets::FindPreset(wxString &name)
{
   FFmpegPresetMap::iterator iter = mPresets.find(name);
   if (iter != mPresets.end())
   {
      return &iter->second;
   }

   return NULL;
}

void FFmpegPresets::SavePreset(ExportFFmpegOptions *parent, wxString &name)
{
   wxString format;
   wxString codec;
   FFmpegPreset *preset = FindPreset(name);
   if (preset)
   {
      wxString query = wxString::Format(_("Overwrite preset '%s'?"),name.c_str());
      int action = wxMessageBox(query,_("Confirm Overwrite"),wxYES_NO | wxCENTRE);
      if (action == wxNO) return;
   }

   wxWindow *wnd;
   wxListBox *lb;

   wnd = dynamic_cast<wxWindow*>(parent)->FindWindowById(FEFormatID,parent);
   lb = dynamic_cast<wxListBox*>(wnd);
   if (lb->GetSelection() < 0)
   {
      wxMessageBox(_("Please select format before saving a profile"));
      return;
   }
   format = lb->GetStringSelection();

   wnd = dynamic_cast<wxWindow*>(parent)->FindWindowById(FECodecID,parent);
   lb = dynamic_cast<wxListBox*>(wnd);
   if (lb->GetSelection() < 0)
   {
      wxMessageBox(_("Please select codec before saving a profile"));
      return;
   }
   codec = lb->GetStringSelection();

   preset = &mPresets[name];
   preset->mPresetName = name;

   wxSpinCtrl *sc;
   wxTextCtrl *tc;
   wxCheckBox *cb;
   wxChoice *ch;

   for (int id = FEFirstID; id < FELastID; id++)
   {
      wxWindow *wnd = dynamic_cast<wxWindow*>(parent)->FindWindowById(id,parent);
      if (wnd != NULL)
      {
         switch(id)
         {
         case FEFormatID:
            preset->mControlState.Item(id - FEFirstID) = format;
            break;
         case FECodecID:
            preset->mControlState.Item(id - FEFirstID) = codec;
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
            preset->mControlState.Item(id - FEFirstID) = wxString::Format(wxT("%d"),sc->GetValue());
            break;
         // Text control
         case FELanguageID:
         case FETagID:
            tc = dynamic_cast<wxTextCtrl*>(wnd);
            preset->mControlState.Item(id - FEFirstID) = tc->GetValue();
            break;
         // Choice
         case FEProfileID:
         case FEPredOrderID:
            ch = dynamic_cast<wxChoice*>(wnd);
            preset->mControlState.Item(id - FEFirstID) = wxString::Format(wxT("%d"),ch->GetSelection());
            break;
         // Check box
         case FEUseLPCID:
         case FEBitReservoirID:
         case FEVariableBlockLenID:
            cb = dynamic_cast<wxCheckBox*>(wnd);
            preset->mControlState.Item(id - FEFirstID) = wxString::Format(wxT("%d"),cb->GetValue());
            break;
         }
      }
   }
}

void FFmpegPresets::LoadPreset(ExportFFmpegOptions *parent, wxString &name)
{
   FFmpegPreset *preset = FindPreset(name);
   if (!preset)
   {
      wxMessageBox(wxString::Format(_("Preset '%s' does not exist."),name.c_str()));
      return;
   }

   wxListBox *lb;
   wxSpinCtrl *sc;
   wxTextCtrl *tc;
   wxCheckBox *cb;
   wxChoice *ch;

   for (int id = FEFirstID; id < FELastID; id++)
   {
      wxWindow *wnd = parent->FindWindowById(id,parent);
      if (wnd != NULL)
      {
         wxString readstr;
         long readlong;
         bool readbool;
         switch(id)
         {
         // Listbox
         case FEFormatID:
         case FECodecID:
            lb = dynamic_cast<wxListBox*>(wnd);
            readstr = preset->mControlState.Item(id - FEFirstID);
            readlong = lb->FindString(readstr);
            if (readlong > -1) lb->Select(readlong);
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
            preset->mControlState.Item(id - FEFirstID).ToLong(&readlong);
            sc->SetValue(readlong);
            break;
         // Text control
         case FELanguageID:
         case FETagID:
            tc = dynamic_cast<wxTextCtrl*>(wnd);
            tc->SetValue(preset->mControlState.Item(id - FEFirstID));
            break;
         // Choice
         case FEProfileID:
         case FEPredOrderID:
            ch = dynamic_cast<wxChoice*>(wnd);
            preset->mControlState.Item(id - FEFirstID).ToLong(&readlong);
            if (readlong > -1) ch->Select(readlong);
            break;
         // Check box
         case FEUseLPCID:
         case FEBitReservoirID:
         case FEVariableBlockLenID:
            cb = dynamic_cast<wxCheckBox*>(wnd);
            preset->mControlState.Item(id - FEFirstID).ToLong(&readlong);
            if (readlong) readbool = true; else readbool = false;
            cb->SetValue(readbool);
            break;
         }
      }
   }
}

bool FFmpegPresets::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (mAbortImport)
   {
      return false;
   }

   if (!wxStrcmp(tag,wxT("ffmpeg_presets")))
   {
      return true;
   }

   if (!wxStrcmp(tag,wxT("preset")))
   {
      while (*attrs)
      {
         const wxChar *attr = *attrs++;
         wxString value = *attrs++;

         if (!value)
            break;

         if (!wxStrcmp(attr,wxT("name")))
         {
            mPreset = FindPreset(value);
            if (mPreset)
            {
               wxString query = wxString::Format(_("Replace preset '%s'?"), value.c_str());
               int action = wxMessageBox(query, _("Confirm Overwrite"), wxYES_NO | wxCANCEL | wxCENTRE);
               if (action == wxCANCEL)
               {
                  mAbortImport = true;
                  return false;
               }
               if (action == wxNO)
               {
                  mPreset = NULL;
                  return false;
               }
               *mPreset = FFmpegPreset();
            }
            else
            {
               mPreset = &mPresets[value];
            }
            mPreset->mPresetName = value;
         }
      }
      return true;
   }

   if (!wxStrcmp(tag,wxT("setctrlstate")) && mPreset)
   {
      long id = -1;
      while (*attrs)
      {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         if (!wxStrcmp(attr,wxT("id")))
         {
            for (long i = FEFirstID; i < FELastID; i++)
               if (!wxStrcmp(FFmpegExportCtrlIDNames[i - FEFirstID],value))
                  id = i;
         }
         else if (!wxStrcmp(attr,wxT("state")))
         {
            if (id > FEFirstID && id < FELastID)
               mPreset->mControlState.Item(id - FEFirstID) = wxString(value);
         }
      }
      return true;
   }

   return false;
}

XMLTagHandler *FFmpegPresets::HandleXMLChild(const wxChar *tag)
{
   if (mAbortImport)
   {
      return NULL;
   }

   if (!wxStrcmp(tag, wxT("preset")))
   {
      return this;
   }
   else if (!wxStrcmp(tag, wxT("setctrlstate")))
   {
      return this;
   }
   return NULL;
}

void FFmpegPresets::WriteXMLHeader(XMLWriter &xmlFile)
{
   xmlFile.Write(wxT("<?xml "));
   xmlFile.Write(wxT("version=\"1.0\" "));
   xmlFile.Write(wxT("standalone=\"no\" "));
   xmlFile.Write(wxT("?>\n"));

   wxString dtdName = wxT("-//audacityffmpegpreset-1.0.0//DTD//EN");
   wxString dtdURI =
      wxT("http://audacity.sourceforge.net/xml/audacityffmpegpreset-1.0.0.dtd");

   xmlFile.Write(wxT("<!DOCTYPE "));
   xmlFile.Write(wxT("project "));
   xmlFile.Write(wxT("PUBLIC "));
   xmlFile.Write(wxT("\"-//audacityffmpegpreset-1.0.0//DTD//EN\" "));
   xmlFile.Write(wxT("\"http://audacity.sourceforge.net/xml/audacityffmpegpreset-1.0.0.dtd\" "));
   xmlFile.Write(wxT(">\n"));
}

void FFmpegPresets::WriteXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("ffmpeg_presets"));
   xmlFile.WriteAttr(wxT("version"),wxT("1.0"));
   FFmpegPresetMap::iterator iter;
   for (iter = mPresets.begin(); iter != mPresets.end(); ++iter)
   {
      FFmpegPreset *preset = &iter->second;
      xmlFile.StartTag(wxT("preset"));
      xmlFile.WriteAttr(wxT("name"),preset->mPresetName);
      for (long i = FEFirstID + 1; i < FELastID; i++)
      {
         xmlFile.StartTag(wxT("setctrlstate"));
         xmlFile.WriteAttr(wxT("id"),wxString(FFmpegExportCtrlIDNames[i - FEFirstID]));
         xmlFile.WriteAttr(wxT("state"),preset->mControlState.Item(i - FEFirstID));
         xmlFile.EndTag(wxT("setctrlstate"));
      }
      xmlFile.EndTag(wxT("preset"));
   }
   xmlFile.EndTag(wxT("ffmpeg_presets"));
}

//----------------------------------------------------------------------------
// ExportFFmpegOptions Class
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ExportFFmpegOptions, wxDialogWrapper)
   EVT_BUTTON(wxID_OK,ExportFFmpegOptions::OnOK)
   EVT_LISTBOX(FEFormatID,ExportFFmpegOptions::OnFormatList)
   EVT_LISTBOX(FECodecID,ExportFFmpegOptions::OnCodecList)
   EVT_BUTTON(FEAllFormatsID,ExportFFmpegOptions::OnAllFormats)
   EVT_BUTTON(FEAllCodecsID,ExportFFmpegOptions::OnAllCodecs)
   EVT_BUTTON(FESavePresetID,ExportFFmpegOptions::OnSavePreset)
   EVT_BUTTON(FELoadPresetID,ExportFFmpegOptions::OnLoadPreset)
   EVT_BUTTON(FEDeletePresetID,ExportFFmpegOptions::OnDeletePreset)
   EVT_BUTTON(FEImportPresetsID,ExportFFmpegOptions::OnImportPresets)
   EVT_BUTTON(FEExportPresetsID,ExportFFmpegOptions::OnExportPresets)
END_EVENT_TABLE()

/// Format-codec compatibility list
/// Must end with NULL entry
CompatibilityEntry ExportFFmpegOptions::CompatibilityList[] =
{
   { wxT("adts"), AV_CODEC_ID_AAC },

   { wxT("aiff"), AV_CODEC_ID_PCM_S16BE },
   { wxT("aiff"), AV_CODEC_ID_PCM_S8 },
   { wxT("aiff"), AV_CODEC_ID_PCM_S24BE },
   { wxT("aiff"), AV_CODEC_ID_PCM_S32BE },
   { wxT("aiff"), AV_CODEC_ID_PCM_ALAW },
   { wxT("aiff"), AV_CODEC_ID_PCM_MULAW },
   { wxT("aiff"), AV_CODEC_ID_MACE3 },
   { wxT("aiff"), AV_CODEC_ID_MACE6 },
   { wxT("aiff"), AV_CODEC_ID_GSM },
   { wxT("aiff"), AV_CODEC_ID_ADPCM_G726 },
   { wxT("aiff"), AV_CODEC_ID_PCM_S16LE },
   { wxT("aiff"), AV_CODEC_ID_ADPCM_IMA_QT },
   { wxT("aiff"), AV_CODEC_ID_QDM2 },

   { wxT("amr"), AV_CODEC_ID_AMR_NB },
   { wxT("amr"), AV_CODEC_ID_AMR_WB },

   { wxT("asf"), AV_CODEC_ID_PCM_S16LE },
   { wxT("asf"), AV_CODEC_ID_PCM_U8 },
   { wxT("asf"), AV_CODEC_ID_PCM_S24LE },
   { wxT("asf"), AV_CODEC_ID_PCM_S32LE },
   { wxT("asf"), AV_CODEC_ID_ADPCM_MS },
   { wxT("asf"), AV_CODEC_ID_PCM_ALAW },
   { wxT("asf"), AV_CODEC_ID_PCM_MULAW },
   { wxT("asf"), AV_CODEC_ID_WMAVOICE },
   { wxT("asf"), AV_CODEC_ID_ADPCM_IMA_WAV },
   { wxT("asf"), AV_CODEC_ID_ADPCM_YAMAHA },
   { wxT("asf"), AV_CODEC_ID_TRUESPEECH },
   { wxT("asf"), AV_CODEC_ID_GSM_MS },
   { wxT("asf"), AV_CODEC_ID_ADPCM_G726 },
   { wxT("asf"), AV_CODEC_ID_MP2 },
   { wxT("asf"), AV_CODEC_ID_MP3 },
   { wxT("asf"), AV_CODEC_ID_VOXWARE },
   { wxT("asf"), AV_CODEC_ID_AAC },
   { wxT("asf"), AV_CODEC_ID_WMAV1 },
   { wxT("asf"), AV_CODEC_ID_WMAV2 },
   { wxT("asf"), AV_CODEC_ID_WMAPRO },
   { wxT("asf"), AV_CODEC_ID_ADPCM_CT },
   { wxT("asf"), AV_CODEC_ID_ATRAC3 },
   { wxT("asf"), AV_CODEC_ID_IMC },
   { wxT("asf"), AV_CODEC_ID_AC3 },
   { wxT("asf"), AV_CODEC_ID_DTS },
   { wxT("asf"), AV_CODEC_ID_FLAC },
   { wxT("asf"), AV_CODEC_ID_ADPCM_SWF },
   { wxT("asf"), AV_CODEC_ID_VORBIS },

   { wxT("au"), AV_CODEC_ID_PCM_MULAW },
   { wxT("au"), AV_CODEC_ID_PCM_S8 },
   { wxT("au"), AV_CODEC_ID_PCM_S16BE },
   { wxT("au"), AV_CODEC_ID_PCM_ALAW },

   { wxT("avi"), AV_CODEC_ID_PCM_S16LE },
   { wxT("avi"), AV_CODEC_ID_PCM_U8 },
   { wxT("avi"), AV_CODEC_ID_PCM_S24LE },
   { wxT("avi"), AV_CODEC_ID_PCM_S32LE },
   { wxT("avi"), AV_CODEC_ID_ADPCM_MS },
   { wxT("avi"), AV_CODEC_ID_PCM_ALAW },
   { wxT("avi"), AV_CODEC_ID_PCM_MULAW },
   { wxT("avi"), AV_CODEC_ID_WMAVOICE },
   { wxT("avi"), AV_CODEC_ID_ADPCM_IMA_WAV },
   { wxT("avi"), AV_CODEC_ID_ADPCM_YAMAHA },
   { wxT("avi"), AV_CODEC_ID_TRUESPEECH },
   { wxT("avi"), AV_CODEC_ID_GSM_MS },
   { wxT("avi"), AV_CODEC_ID_ADPCM_G726 },
   { wxT("avi"), AV_CODEC_ID_MP2 },
   { wxT("avi"), AV_CODEC_ID_MP3 },
   { wxT("avi"), AV_CODEC_ID_VOXWARE },
   { wxT("avi"), AV_CODEC_ID_AAC },
   { wxT("avi"), AV_CODEC_ID_WMAV1 },
   { wxT("avi"), AV_CODEC_ID_WMAV2 },
   { wxT("avi"), AV_CODEC_ID_WMAPRO },
   { wxT("avi"), AV_CODEC_ID_ADPCM_CT },
   { wxT("avi"), AV_CODEC_ID_ATRAC3 },
   { wxT("avi"), AV_CODEC_ID_IMC },
   { wxT("avi"), AV_CODEC_ID_AC3 },
   { wxT("avi"), AV_CODEC_ID_DTS },
   { wxT("avi"), AV_CODEC_ID_FLAC },
   { wxT("avi"), AV_CODEC_ID_ADPCM_SWF },
   { wxT("avi"), AV_CODEC_ID_VORBIS },

   { wxT("crc"), AV_CODEC_ID_NONE },

   { wxT("dv"), AV_CODEC_ID_PCM_S16LE },

   { wxT("ffm"), AV_CODEC_ID_NONE },

   { wxT("flv"), AV_CODEC_ID_MP3 },
   { wxT("flv"), AV_CODEC_ID_PCM_S8 },
   { wxT("flv"), AV_CODEC_ID_PCM_S16BE },
   { wxT("flv"), AV_CODEC_ID_PCM_S16LE },
   { wxT("flv"), AV_CODEC_ID_ADPCM_SWF },
   { wxT("flv"), AV_CODEC_ID_AAC },
   { wxT("flv"), AV_CODEC_ID_NELLYMOSER },

   { wxT("framecrc"), AV_CODEC_ID_NONE },

   { wxT("gxf"), AV_CODEC_ID_PCM_S16LE },

   { wxT("matroska"), AV_CODEC_ID_PCM_S16LE },
   { wxT("matroska"), AV_CODEC_ID_PCM_U8 },
   { wxT("matroska"), AV_CODEC_ID_PCM_S24LE },
   { wxT("matroska"), AV_CODEC_ID_PCM_S32LE },
   { wxT("matroska"), AV_CODEC_ID_ADPCM_MS },
   { wxT("matroska"), AV_CODEC_ID_PCM_ALAW },
   { wxT("matroska"), AV_CODEC_ID_PCM_MULAW },
   { wxT("matroska"), AV_CODEC_ID_WMAVOICE },
   { wxT("matroska"), AV_CODEC_ID_ADPCM_IMA_WAV },
   { wxT("matroska"), AV_CODEC_ID_ADPCM_YAMAHA },
   { wxT("matroska"), AV_CODEC_ID_TRUESPEECH },
   { wxT("matroska"), AV_CODEC_ID_GSM_MS },
   { wxT("matroska"), AV_CODEC_ID_ADPCM_G726 },
   { wxT("matroska"), AV_CODEC_ID_MP2 },
   { wxT("matroska"), AV_CODEC_ID_MP3 },
   { wxT("matroska"), AV_CODEC_ID_VOXWARE },
   { wxT("matroska"), AV_CODEC_ID_AAC },
   { wxT("matroska"), AV_CODEC_ID_WMAV1 },
   { wxT("matroska"), AV_CODEC_ID_WMAV2 },
   { wxT("matroska"), AV_CODEC_ID_WMAPRO },
   { wxT("matroska"), AV_CODEC_ID_ADPCM_CT },
   { wxT("matroska"), AV_CODEC_ID_ATRAC3 },
   { wxT("matroska"), AV_CODEC_ID_IMC },
   { wxT("matroska"), AV_CODEC_ID_AC3 },
   { wxT("matroska"), AV_CODEC_ID_DTS },
   { wxT("matroska"), AV_CODEC_ID_FLAC },
   { wxT("matroska"), AV_CODEC_ID_ADPCM_SWF },
   { wxT("matroska"), AV_CODEC_ID_VORBIS },

   { wxT("mmf"), AV_CODEC_ID_ADPCM_YAMAHA },

   { wxT("mov"), AV_CODEC_ID_PCM_S32BE }, //mov
   { wxT("mov"), AV_CODEC_ID_PCM_S32LE },
   { wxT("mov"), AV_CODEC_ID_PCM_S24BE },
   { wxT("mov"), AV_CODEC_ID_PCM_S24LE },
   { wxT("mov"), AV_CODEC_ID_PCM_S16BE },
   { wxT("mov"), AV_CODEC_ID_PCM_S16LE },
   { wxT("mov"), AV_CODEC_ID_PCM_S8 },
   { wxT("mov"), AV_CODEC_ID_PCM_U8 },
   { wxT("mov"), AV_CODEC_ID_PCM_MULAW },
   { wxT("mov"), AV_CODEC_ID_PCM_ALAW },
   { wxT("mov"), AV_CODEC_ID_ADPCM_IMA_QT },
   { wxT("mov"), AV_CODEC_ID_MACE3 },
   { wxT("mov"), AV_CODEC_ID_MACE6 },
   { wxT("mov"), AV_CODEC_ID_MP3 },
   { wxT("mov"), AV_CODEC_ID_AAC },
   { wxT("mov"), AV_CODEC_ID_AMR_NB },
   { wxT("mov"), AV_CODEC_ID_AMR_WB },
   { wxT("mov"), AV_CODEC_ID_GSM },
   { wxT("mov"), AV_CODEC_ID_ALAC },
   { wxT("mov"), AV_CODEC_ID_QCELP },
   { wxT("mov"), AV_CODEC_ID_QDM2 },
   { wxT("mov"), AV_CODEC_ID_DVAUDIO },
   { wxT("mov"), AV_CODEC_ID_WMAV2 },
   { wxT("mov"), AV_CODEC_ID_ALAC },

   { wxT("mp4"), AV_CODEC_ID_AAC },
   { wxT("mp4"), AV_CODEC_ID_QCELP },
   { wxT("mp4"), AV_CODEC_ID_MP3 },
   { wxT("mp4"), AV_CODEC_ID_VORBIS },

   { wxT("psp"), AV_CODEC_ID_AAC },
   { wxT("psp"), AV_CODEC_ID_QCELP },
   { wxT("psp"), AV_CODEC_ID_MP3 },
   { wxT("psp"), AV_CODEC_ID_VORBIS },

   { wxT("ipod"), AV_CODEC_ID_AAC },
   { wxT("ipod"), AV_CODEC_ID_QCELP },
   { wxT("ipod"), AV_CODEC_ID_MP3 },
   { wxT("ipod"), AV_CODEC_ID_VORBIS },

   { wxT("3gp"), AV_CODEC_ID_AAC },
   { wxT("3gp"), AV_CODEC_ID_AMR_NB },
   { wxT("3gp"), AV_CODEC_ID_AMR_WB },

   { wxT("3g2"), AV_CODEC_ID_AAC },
   { wxT("3g2"), AV_CODEC_ID_AMR_NB },
   { wxT("3g2"), AV_CODEC_ID_AMR_WB },

   { wxT("mp3"), AV_CODEC_ID_MP3 },

   { wxT("mpeg"), AV_CODEC_ID_AC3 },
   { wxT("mpeg"), AV_CODEC_ID_DTS },
   { wxT("mpeg"), AV_CODEC_ID_PCM_S16BE },
   { wxT("mpeg"), AV_CODEC_ID_MP2 },

   { wxT("vcd"), AV_CODEC_ID_AC3 },
   { wxT("vcd"), AV_CODEC_ID_DTS },
   { wxT("vcd"), AV_CODEC_ID_PCM_S16BE },
   { wxT("vcd"), AV_CODEC_ID_MP2 },

   { wxT("vob"), AV_CODEC_ID_AC3 },
   { wxT("vob"), AV_CODEC_ID_DTS },
   { wxT("vob"), AV_CODEC_ID_PCM_S16BE },
   { wxT("vob"), AV_CODEC_ID_MP2 },

   { wxT("svcd"), AV_CODEC_ID_AC3 },
   { wxT("svcd"), AV_CODEC_ID_DTS },
   { wxT("svcd"), AV_CODEC_ID_PCM_S16BE },
   { wxT("svcd"), AV_CODEC_ID_MP2 },

   { wxT("dvd"), AV_CODEC_ID_AC3 },
   { wxT("dvd"), AV_CODEC_ID_DTS },
   { wxT("dvd"), AV_CODEC_ID_PCM_S16BE },
   { wxT("dvd"), AV_CODEC_ID_MP2 },

   { wxT("nut"), AV_CODEC_ID_PCM_S16LE },
   { wxT("nut"), AV_CODEC_ID_PCM_U8 },
   { wxT("nut"), AV_CODEC_ID_PCM_S24LE },
   { wxT("nut"), AV_CODEC_ID_PCM_S32LE },
   { wxT("nut"), AV_CODEC_ID_ADPCM_MS },
   { wxT("nut"), AV_CODEC_ID_PCM_ALAW },
   { wxT("nut"), AV_CODEC_ID_PCM_MULAW },
   { wxT("nut"), AV_CODEC_ID_WMAVOICE },
   { wxT("nut"), AV_CODEC_ID_ADPCM_IMA_WAV },
   { wxT("nut"), AV_CODEC_ID_ADPCM_YAMAHA },
   { wxT("nut"), AV_CODEC_ID_TRUESPEECH },
   { wxT("nut"), AV_CODEC_ID_GSM_MS },
   { wxT("nut"), AV_CODEC_ID_ADPCM_G726 },
   { wxT("nut"), AV_CODEC_ID_MP2 },
   { wxT("nut"), AV_CODEC_ID_MP3 },
   { wxT("nut"), AV_CODEC_ID_VOXWARE },
   { wxT("nut"), AV_CODEC_ID_AAC },
   { wxT("nut"), AV_CODEC_ID_WMAV1 },
   { wxT("nut"), AV_CODEC_ID_WMAV2 },
   { wxT("nut"), AV_CODEC_ID_WMAPRO },
   { wxT("nut"), AV_CODEC_ID_ADPCM_CT },
   { wxT("nut"), AV_CODEC_ID_ATRAC3 },
   { wxT("nut"), AV_CODEC_ID_IMC },
   { wxT("nut"), AV_CODEC_ID_AC3 },
   { wxT("nut"), AV_CODEC_ID_DTS },
   { wxT("nut"), AV_CODEC_ID_FLAC },
   { wxT("nut"), AV_CODEC_ID_ADPCM_SWF },
   { wxT("nut"), AV_CODEC_ID_VORBIS },

   { wxT("ogg"), AV_CODEC_ID_VORBIS },
   { wxT("ogg"), AV_CODEC_ID_FLAC },

   { wxT("ac3"), AV_CODEC_ID_AC3 },

   { wxT("dts"), AV_CODEC_ID_DTS },

   { wxT("flac"), AV_CODEC_ID_FLAC },

   { wxT("RoQ"), AV_CODEC_ID_ROQ_DPCM },

   { wxT("rm"), AV_CODEC_ID_AC3 },

   { wxT("swf"), AV_CODEC_ID_MP3 },

   { wxT("avm2"), AV_CODEC_ID_MP3 },

   { wxT("voc"), AV_CODEC_ID_PCM_U8 },

   { wxT("wav"), AV_CODEC_ID_PCM_S16LE },
   { wxT("wav"), AV_CODEC_ID_PCM_U8 },
   { wxT("wav"), AV_CODEC_ID_PCM_S24LE },
   { wxT("wav"), AV_CODEC_ID_PCM_S32LE },
   { wxT("wav"), AV_CODEC_ID_ADPCM_MS },
   { wxT("wav"), AV_CODEC_ID_PCM_ALAW },
   { wxT("wav"), AV_CODEC_ID_PCM_MULAW },
   { wxT("wav"), AV_CODEC_ID_WMAVOICE },
   { wxT("wav"), AV_CODEC_ID_ADPCM_IMA_WAV },
   { wxT("wav"), AV_CODEC_ID_ADPCM_YAMAHA },
   { wxT("wav"), AV_CODEC_ID_TRUESPEECH },
   { wxT("wav"), AV_CODEC_ID_GSM_MS },
   { wxT("wav"), AV_CODEC_ID_ADPCM_G726 },
   { wxT("wav"), AV_CODEC_ID_MP2 },
   { wxT("wav"), AV_CODEC_ID_MP3 },
   { wxT("wav"), AV_CODEC_ID_VOXWARE },
   { wxT("wav"), AV_CODEC_ID_AAC },
   { wxT("wav"), AV_CODEC_ID_WMAV1 },
   { wxT("wav"), AV_CODEC_ID_WMAV2 },
   { wxT("wav"), AV_CODEC_ID_WMAPRO },
   { wxT("wav"), AV_CODEC_ID_ADPCM_CT },
   { wxT("wav"), AV_CODEC_ID_ATRAC3 },
   { wxT("wav"), AV_CODEC_ID_IMC },
   { wxT("wav"), AV_CODEC_ID_AC3 },
   { wxT("wav"), AV_CODEC_ID_DTS },
   { wxT("wav"), AV_CODEC_ID_FLAC },
   { wxT("wav"), AV_CODEC_ID_ADPCM_SWF },
   { wxT("wav"), AV_CODEC_ID_VORBIS },

   { NULL, AV_CODEC_ID_NONE }
};

/// AAC profiles
int ExportFFmpegOptions::iAACProfileValues[] = {
   FF_PROFILE_AAC_LOW,
   FF_PROFILE_AAC_MAIN,
   /*FF_PROFILE_AAC_SSR,*/
   FF_PROFILE_AAC_LTP
};

/// Names of AAC profiles to be displayed
const wxChar *ExportFFmpegOptions::iAACProfileNames[] = {
   _("LC"),
   _("Main"),
   /*_("SSR"),*/ //SSR is not supported
   _("LTP")
};

/// List of export types
ExposedFormat ExportFFmpegOptions::fmts[] =
{
   {FMT_M4A,   wxT("M4A"),    wxT("m4a"), wxT("ipod"), 48,  AV_VERSION_INT(-1,-1,-1), true,  _("M4A (AAC) Files (FFmpeg)"),         AV_CODEC_ID_AAC,    true},
   {FMT_AC3,   wxT("AC3"),    wxT("ac3"), wxT("ac3"),  7,   AV_VERSION_INT(0,0,0),    false, _("AC3 Files (FFmpeg)"),               AV_CODEC_ID_AC3,    true},
   {FMT_AMRNB, wxT("AMRNB"),  wxT("amr"), wxT("amr"),  1,   AV_VERSION_INT(0,0,0),    false, _("AMR (narrow band) Files (FFmpeg)"), AV_CODEC_ID_AMR_NB, true},
   {FMT_WMA2,  wxT("WMA"),    wxT("wma"), wxT("asf"),  2,   AV_VERSION_INT(52,53,0),  false, _("WMA (version 2) Files (FFmpeg)"),   AV_CODEC_ID_WMAV2,  true},
   {FMT_OTHER, wxT("FFMPEG"), wxT(""),    wxT(""),     255, AV_VERSION_INT(-1,-1,-1), true,  _("Custom FFmpeg Export"),             AV_CODEC_ID_NONE,   true}
};

/// Sample rates supported by AAC encoder (must end with zero-element)
const int ExportFFmpegOptions::iAACSampleRates[] = { 7350, 8000, 11025, 12000, 16000, 22050, 24000, 32000, 44100, 48000, 64000, 88200, 0 };

/// Some controls (parameters they represent) are only applicable to a number
/// of codecs and/or formats.
/// Syntax: first, enable a control for each applicable format-codec combination
/// then disable it for anything else
/// "any" - any format
/// AV_CODEC_ID_NONE - any codec
/// This list must end with {FALSE,FFmpegExportCtrlID(0),AV_CODEC_ID_NONE,NULL}
ApplicableFor ExportFFmpegOptions::apptable[] =
{
   {TRUE,FEQualityID,AV_CODEC_ID_AAC,"any"},
   {TRUE,FEQualityID,AV_CODEC_ID_MP3,"any"},
   {TRUE,FEQualityID,AV_CODEC_ID_VORBIS,"any"},
   {FALSE,FEQualityID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FECutoffID,AV_CODEC_ID_AC3,"any"},
   {TRUE,FECutoffID,AV_CODEC_ID_AAC,"any"},
   {TRUE,FECutoffID,AV_CODEC_ID_VORBIS,"any"},
   {FALSE,FECutoffID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEFrameSizeID,AV_CODEC_ID_FLAC,"any"},
   {FALSE,FEFrameSizeID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEProfileID,AV_CODEC_ID_AAC,"any"},
   {FALSE,FEProfileID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FECompLevelID,AV_CODEC_ID_FLAC,"any"},
   {FALSE,FECompLevelID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEUseLPCID,AV_CODEC_ID_FLAC,"any"},
   {FALSE,FEUseLPCID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FELPCCoeffsID,AV_CODEC_ID_FLAC,"any"},
   {FALSE,FELPCCoeffsID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEMinPredID,AV_CODEC_ID_FLAC,"any"},
   {FALSE,FEMinPredID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEMaxPredID,AV_CODEC_ID_FLAC,"any"},
   {FALSE,FEMaxPredID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEPredOrderID,AV_CODEC_ID_FLAC,"any"},
   {FALSE,FEPredOrderID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEMinPartOrderID,AV_CODEC_ID_FLAC,"any"},
   {FALSE,FEMinPartOrderID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEMaxPartOrderID,AV_CODEC_ID_FLAC,"any"},
   {FALSE,FEMaxPartOrderID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEMuxRateID,AV_CODEC_ID_NONE,"mpeg"},
   {TRUE,FEMuxRateID,AV_CODEC_ID_NONE,"vcd"},
   {TRUE,FEMuxRateID,AV_CODEC_ID_NONE,"vob"},
   {TRUE,FEMuxRateID,AV_CODEC_ID_NONE,"svcd"},
   {TRUE,FEMuxRateID,AV_CODEC_ID_NONE,"dvd"},
   {FALSE,FEMuxRateID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEPacketSizeID,AV_CODEC_ID_NONE,"mpeg"},
   {TRUE,FEPacketSizeID,AV_CODEC_ID_NONE,"vcd"},
   {TRUE,FEPacketSizeID,AV_CODEC_ID_NONE,"vob"},
   {TRUE,FEPacketSizeID,AV_CODEC_ID_NONE,"svcd"},
   {TRUE,FEPacketSizeID,AV_CODEC_ID_NONE,"dvd"},
   {FALSE,FEPacketSizeID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FELanguageID,AV_CODEC_ID_NONE,"matroska"},
   {TRUE,FELanguageID,AV_CODEC_ID_NONE,"mov"},
   {TRUE,FELanguageID,AV_CODEC_ID_NONE,"3gp"},
   {TRUE,FELanguageID,AV_CODEC_ID_NONE,"mp4"},
   {TRUE,FELanguageID,AV_CODEC_ID_NONE,"psp"},
   {TRUE,FELanguageID,AV_CODEC_ID_NONE,"3g2"},
   {TRUE,FELanguageID,AV_CODEC_ID_NONE,"ipod"},
   {TRUE,FELanguageID,AV_CODEC_ID_NONE,"mpegts"},
   {FALSE,FELanguageID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEBitReservoirID,AV_CODEC_ID_MP3,"any"},
   {TRUE,FEBitReservoirID,AV_CODEC_ID_WMAV1,"any"},
   {TRUE,FEBitReservoirID,AV_CODEC_ID_WMAV2,"any"},
   {FALSE,FEBitReservoirID,AV_CODEC_ID_NONE,"any"},

   {TRUE,FEVariableBlockLenID,AV_CODEC_ID_WMAV1,"any"},
   {TRUE,FEVariableBlockLenID,AV_CODEC_ID_WMAV2,"any"},
   {FALSE,FEVariableBlockLenID,AV_CODEC_ID_NONE,"any"},

   {FALSE,FFmpegExportCtrlID(0),AV_CODEC_ID_NONE,NULL}
};

/// Prediction order method - names. Labels are indices of this array.
const wxChar *ExportFFmpegOptions::PredictionOrderMethodNames[] = { _("Estimate"), _("2-level"), _("4-level"), _("8-level"), _("Full search"), _("Log search")};


ExportFFmpegOptions::~ExportFFmpegOptions()
{
   DropFFmpegLibs();
}

ExportFFmpegOptions::ExportFFmpegOptions(wxWindow *parent)
:  wxDialogWrapper(parent, wxID_ANY,
            wxString(_("Configure custom FFmpeg options")))
{
   SetName(GetTitle());
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PickFFmpegLibs();
   //FFmpegLibsInst()->LoadLibs(NULL,true); //Loaded at startup or from Prefs now

   mPresets = std::make_unique<FFmpegPresets>();
   mPresets->GetPresetList(mPresetNames);

   if (FFmpegLibsInst()->ValidLibsLoaded())
   {
      FetchFormatList();
      FetchCodecList();

      for (unsigned int i = 0; i < 6; i++)
      {
         mPredictionOrderMethodLabels.Add(i);
         mPredictionOrderMethodNames.Add(wxString::Format(wxT("%s"),PredictionOrderMethodNames[i]));
      }

      for (unsigned int i=0; i < (sizeof(iAACProfileValues)/sizeof(int)); i++)
      {
         mProfileNames.Add(wxString::Format(wxT("%s"),iAACProfileNames[i]));
         mProfileLabels.Add(iAACProfileValues[i]);
      }

      PopulateOrExchange(S);

      //Select the format that was selected last time this dialog was closed
      mFormatList->Select(mFormatList->FindString(gPrefs->Read(wxT("/FileFormats/FFmpegFormat"))));
      DoOnFormatList();

      //Select the codec that was selected last time this dialog was closed
      AVCodec *codec = avcodec_find_encoder_by_name(gPrefs->Read(wxT("/FileFormats/FFmpegCodec")).ToUTF8());
      if (codec != NULL) mCodecList->Select(mCodecList->FindString(wxString::FromUTF8(codec->name)));
      DoOnCodecList();
   }

}

///
///
void ExportFFmpegOptions::FetchFormatList()
{
   // Enumerate all output formats
   AVOutputFormat *ofmt = NULL;
   while ((ofmt = av_oformat_next(ofmt))!=NULL)
   {
      // Any audio-capable format has default audio codec.
      // If it doesn't, then it doesn't supports any audio codecs
      if (ofmt->audio_codec != AV_CODEC_ID_NONE)
      {
         mFormatNames.Add(wxString::FromUTF8(ofmt->name));
         mFormatLongNames.Add(wxString::Format(wxT("%s - %s"),mFormatNames.Last().c_str(),wxString::FromUTF8(ofmt->long_name).c_str()));
      }
   }
   // Show all formats
   mShownFormatNames = mFormatNames;
   mShownFormatLongNames =  mFormatLongNames;
}

///
///
void ExportFFmpegOptions::FetchCodecList()
{
   // Enumerate all codecs
   AVCodec *codec = NULL;
   while ((codec = av_codec_next(codec))!=NULL)
   {
      // We're only interested in audio and only in encoders
      if (codec->type == AVMEDIA_TYPE_AUDIO && av_codec_is_encoder(codec))
      {
         mCodecNames.Add(wxString::FromUTF8(codec->name));
         mCodecLongNames.Add(wxString::Format(wxT("%s - %s"),mCodecNames.Last().c_str(),wxString::FromUTF8(codec->long_name).c_str()));
      }
   }
   // Show all codecs
   mShownCodecNames = mCodecNames;
   mShownCodecLongNames = mCodecLongNames;
}

///
///
void ExportFFmpegOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay(1);
   S.StartMultiColumn(1, wxEXPAND);
   {
      S.SetStretchyRow(3);
      S.StartMultiColumn(7, wxEXPAND);
      {
         S.SetStretchyCol(1);
         mPresetCombo = S.Id(FEPresetID).AddCombo(_("Preset:"), gPrefs->Read(wxT("/FileFormats/FFmpegPreset"),wxEmptyString), &mPresetNames);
         mLoadPreset = S.Id(FELoadPresetID).AddButton(_("Load Preset"));
         mSavePreset = S.Id(FESavePresetID).AddButton(_("Save Preset"));
         mDeletePreset = S.Id(FEDeletePresetID).AddButton(_("Delete Preset"));
         mImportPresets = S.Id(FEImportPresetsID).AddButton(_("Import Presets"));
         mExportPresets = S.Id(FEExportPresetsID).AddButton(_("Export Presets"));
      }
      S.EndMultiColumn();
      S.StartMultiColumn(4, wxALIGN_LEFT);
      {
         S.SetStretchyCol(1);
         S.SetStretchyCol(3);
         S.Id(FEFormatLabelID).AddFixedText(_("Format:"));
         mFormatName = S.Id(FEFormatNameID).AddVariableText(wxT(""));
         S.Id(FECodecLabelID).AddFixedText(_("Codec:"));
         mCodecName = S.Id(FECodecNameID).AddVariableText(wxT(""));
      }
      S.EndMultiColumn();
      S.AddVariableText(_("Not all formats and codecs are compatible. Nor are all option combinations compatible with all codecs."), false);
      S.StartMultiColumn(2, wxEXPAND);
      {
         S.StartMultiColumn(2, wxEXPAND);
         {
            S.SetStretchyRow(1);
            S.Id(FEAllFormatsID).AddButton(_("Show All Formats"));
            S.Id(FEAllCodecsID).AddButton(_("Show All Codecs"));
            mFormatList = S.Id(FEFormatID).AddListBox(&mFormatNames);
            mFormatList->DeselectAll();
            mCodecList = S.Id(FECodecID).AddListBox(&mCodecNames);
            mCodecList->DeselectAll();
         }
         S.EndMultiColumn();
         S.StartVerticalLay();
         {
            //S.StartScroller( );
            S.SetBorder( 3 );
            S.StartStatic(_("General Options"), 0);
            {
               S.StartMultiColumn(8, wxEXPAND);
               {
                  mLanguageText = S.Id(FELanguageID).TieTextBox(_("Language:"), wxT("/FileFormats/FFmpegLanguage"), wxEmptyString, 9);
                  mLanguageText->SetToolTip(_("ISO 639 3-letter language code\nOptional\nempty - automatic"));

                  S.AddSpace( 20,0 );
                  S.AddVariableText(_("Bit Reservoir"));
                  S.Id(FEBitReservoirID).TieCheckBox(wxEmptyString, wxT("/FileFormats/FFmpegBitReservoir"), true);

                  S.AddSpace( 20,0 );
                  S.AddVariableText(_("VBL"));
                  S.Id(FEVariableBlockLenID).TieCheckBox(wxEmptyString, wxT("/FileFormats/FFmpegVariableBlockLen"), true);
               }
               S.EndMultiColumn();
               S.StartMultiColumn(4, wxALIGN_LEFT);
               {
                  mTag = S.Id(FETagID).TieTextBox(_("Tag:"), wxT("/FileFormats/FFmpegTag"), wxEmptyString, 4);
                  mTag->SetToolTip(_("Codec tag (FOURCC)\nOptional\nempty - automatic"));

                  mBitrateSpin = S.Id(FEBitrateID).TieSpinCtrl(_("Bit Rate:"), wxT("/FileFormats/FFmpegBitRate"), 0, 1000000, 0);
                  mBitrateSpin->SetToolTip(_("Bit Rate (bits/second) - influences the resulting file size and quality\nSome codecs may only accept specific values (128k, 192k, 256k etc)\n0 - automatic\nRecommended - 192000"));

                  mQualitySpin = S.Id(FEQualityID).TieSpinCtrl(_("Quality:"), wxT("/FileFormats/FFmpegQuality"), 0, 500, -1);
                  mQualitySpin->SetToolTip(_("Overall quality, used differently by different codecs\nRequired for vorbis\n0 - automatic\n-1 - off (use bitrate instead)"));

                  mSampleRateSpin = S.Id(FESampleRateID).TieSpinCtrl(_("Sample Rate:"), wxT("/FileFormats/FFmpegSampleRate"), 0, 200000, 0);
                  mSampleRateSpin->SetToolTip(_("Sample rate (Hz)\n0 - don't change sample rate"));

                  mCutoffSpin = S.Id(FECutoffID).TieSpinCtrl(_("Cutoff:"), wxT("/FileFormats/FFmpegCutOff"), 0, 10000000, 0);
                  mCutoffSpin->SetToolTip(_("Audio cutoff bandwidth (Hz)\nOptional\n0 - automatic"));

                  mProfileChoice = S.Id(FEProfileID).TieChoice(_("Profile:"), wxT("/FileFormats/FFmpegAACProfile"),
                     mProfileLabels[0], mProfileNames, mProfileLabels);
                  mProfileChoice->SetSizeHints( 100,-1);
                  mProfileChoice->SetToolTip(_("AAC Profile\nLow Complexity -default\nMost players won't play anything other than LC"));

               }
               S.EndMultiColumn();
            }
            S.EndStatic();
            S.StartStatic(_("FLAC options"),0);
            {
               S.StartMultiColumn(4, wxALIGN_LEFT);
               {
                  mCompressionLevelSpin = S.Id(FECompLevelID).TieSpinCtrl(_("Compression:"), wxT("/FileFormats/FFmpegCompLevel"), 0, 10, -1);
                  mCompressionLevelSpin->SetToolTip(_("Compression level\nRequired for FLAC\n-1 - automatic\nmin - 0 (fast encoding, large output file)\nmax - 10 (slow encoding, small output file)"));

                  mFrameSizeSpin =  S.Id(FEFrameSizeID).TieSpinCtrl(_("Frame:"), wxT("/FileFormats/FFmpegFrameSize"), 0, 65535, 0);
                  mFrameSizeSpin->SetToolTip(_("Frame size\nOptional\n0 - default\nmin - 16\nmax - 65535"));

                  mLPCCoeffsPrecisionSpin = S.Id(FELPCCoeffsID).TieSpinCtrl(_("LPC"), wxT("/FileFormats/FFmpegLPCCoefPrec"), 0, 15, 0);
                  mLPCCoeffsPrecisionSpin->SetToolTip(_("LPC coefficients precision\nOptional\n0 - default\nmin - 1\nmax - 15"));

                  mPredictionOrderMethodChoice = S.Id(FEPredOrderID).TieChoice(_("PdO Method:"), wxT("/FileFormats/FFmpegPredOrderMethod"),
                     mPredictionOrderMethodLabels[4], mPredictionOrderMethodNames, mPredictionOrderMethodLabels);
                  mPredictionOrderMethodChoice->SetSizeHints( 100,-1);
                  mPredictionOrderMethodChoice->SetToolTip(_("Prediction Order Method\nEstimate - fastest, lower compression\nLog search - slowest, best compression\nFull search - default"));

                  mMinPredictionOrderSpin = S.Id(FEMinPredID).TieSpinCtrl(_("Min. PdO"), wxT("/FileFormats/FFmpegMinPredOrder"), -1, 32, -1);
                  mMinPredictionOrderSpin->SetToolTip(_("Minimal prediction order\nOptional\n-1 - default\nmin - 0\nmax - 32 (with LPC) or 4 (without LPC)"));

                  mMaxPredictionOrderSpin = S.Id(FEMaxPredID).TieSpinCtrl(_("Max. PdO"), wxT("/FileFormats/FFmpegMaxPredOrder"), -1, 32, -1);
                  mMaxPredictionOrderSpin->SetToolTip(_("Maximal prediction order\nOptional\n-1 - default\nmin - 0\nmax - 32 (with LPC) or 4 (without LPC)"));

                  mMinPartitionOrderSpin = S.Id(FEMinPartOrderID).TieSpinCtrl(_("Min. PtO"), wxT("/FileFormats/FFmpegMinPartOrder"), -1, 8, -1);
                  mMinPartitionOrderSpin->SetToolTip(_("Minimal partition order\nOptional\n-1 - default\nmin - 0\nmax - 8"));

                  mMaxPartitionOrderSpin = S.Id(FEMaxPartOrderID).TieSpinCtrl(_("Max. PtO"), wxT("/FileFormats/FFmpegMaxPredOrder"), -1, 8, -1);
                  mMaxPartitionOrderSpin->SetToolTip(_("Maximal partition order\nOptional\n-1 - default\nmin - 0\nmax - 8"));

                  S.AddVariableText(_("Use LPC"));
                  S.Id(FEUseLPCID).TieCheckBox(wxEmptyString, wxT("/FileFormats/FFmpegUseLPC"), true);
               }
               S.EndMultiColumn();
            }
            S.EndStatic();
            S.StartStatic(_("MPEG container options"),0);
            {
               S.StartMultiColumn(4, wxALIGN_LEFT);
               {
                  /* i18n-hint: 'mux' is short for multiplexor, a device that selects between several inputs
                    'Mux Rate' is a parameter that has some bearing on compression ratio for MPEG
                    it has a hard to predict effect on the degree of compression */
                  mMuxRate = S.Id(FEMuxRateID).TieSpinCtrl(_("Mux Rate:"), wxT("/FileFormats/FFmpegMuxRate"), 0, 10000000, 0);
                  mMuxRate->SetToolTip(_("Maximum bit rate of the multiplexed stream\nOptional\n0 - default"));

                  /* i18n-hint: 'Packet Size' is a parameter that has some bearing on compression ratio for MPEG
                    compression.  It measures how big a chunk of audio is compressed in one piece. */
                  mPacketSize = S.Id(FEPacketSizeID).TieSpinCtrl(_("Packet Size:"), wxT("/FileFormats/FFmpegPacketSize"), 0, 10000000, 0);
                  mPacketSize->SetToolTip(_("Packet size\nOptional\n0 - default"));
               }
               S.EndMultiColumn();
            }
            S.EndStatic();
            //S.EndScroller();
            S.SetBorder( 5 );
            S.AddStandardButtons();
         }
         S.EndVerticalLay();
      }
      S.EndMultiColumn();
   }
   S.EndMultiColumn();
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

///
///
void ExportFFmpegOptions::FindSelectedFormat(wxString **name, wxString **longname)
{
   // Get current selection
   wxArrayInt selections;
   int n = mFormatList->GetSelections(selections);
   if (n <= 0) return;

   // Get selected format short name
   wxString selfmt = mFormatList->GetString(selections[0]);

   // Find it's index
   int nFormat = mFormatNames.Index(selfmt.c_str());
   if (nFormat == wxNOT_FOUND) return;

   // Return short name and description
   if (name != NULL) *name = &mFormatNames[nFormat];
   if (longname != NULL) *longname = &mFormatLongNames[nFormat];
   return;
}
///
///
void ExportFFmpegOptions::FindSelectedCodec(wxString **name, wxString **longname)
{
   // Get current selection
   wxArrayInt selections;
   int n = mCodecList->GetSelections(selections);
   if (n <= 0) return;

   // Get selected codec short name
   wxString selcdc = mCodecList->GetString(selections[0]);

   // Find it's index
   int nCodec = mCodecNames.Index(selcdc.c_str());
   if (nCodec == wxNOT_FOUND) return;

   // Return short name and description
   if (name != NULL) *name = &mCodecNames[nCodec];
   if (longname != NULL) *longname = &mCodecLongNames[nCodec];
}

///
///
int ExportFFmpegOptions::FetchCompatibleCodecList(const wxChar *fmt, AVCodecID id)
{
   // By default assume that id is not in the list
   int index = -1;
   // By default no codecs are compatible (yet)
   mShownCodecNames.Clear();
   mShownCodecLongNames.Clear();
   // Clear the listbox
   mCodecList->Clear();
   // Zero - format is not found at all
   int found = 0;
   wxString str(fmt);
   for (int i = 0; CompatibilityList[i].fmt != NULL; i++)
   {
      if (str.Cmp(CompatibilityList[i].fmt) == 0)
      {
         // Format is found in the list
         found = 1;
         if (CompatibilityList[i].codec == AV_CODEC_ID_NONE)
         {
            // Format is found in the list and it is compatible with AV_CODEC_ID_NONE (means that it is compatible to anything)
            found = 2;
            break;
         }
         // Find the codec, that is claimed to be compatible
         AVCodec *codec = avcodec_find_encoder(CompatibilityList[i].codec);
         // If it exists, is audio and has encoder
         if (codec != NULL && (codec->type == AVMEDIA_TYPE_AUDIO) && av_codec_is_encoder(codec))
         {
            // If it was selected - remember it's NEW index
            if ((id >= 0) && codec->id == id) index = mShownCodecNames.GetCount();
            mShownCodecNames.Add(wxString::FromUTF8(codec->name));
            mShownCodecLongNames.Add(wxString::Format(wxT("%s - %s"),mShownCodecNames.Last().c_str(),wxString::FromUTF8(codec->long_name).c_str()));
         }
      }
   }
   // All codecs are compatible with this format
   if (found == 2)
   {
      AVCodec *codec = NULL;
      while ((codec = av_codec_next(codec))!=NULL)
      {
         if (codec->type == AVMEDIA_TYPE_AUDIO && av_codec_is_encoder(codec))
         {
            if (mShownCodecNames.Index(wxString::FromUTF8(codec->name)) < 0)
            {
               if ((id >= 0) && codec->id == id) index = mShownCodecNames.GetCount();
               mShownCodecNames.Add(wxString::FromUTF8(codec->name));
               mShownCodecLongNames.Add(wxString::Format(wxT("%s - %s"),mShownCodecNames.Last().c_str(),wxString::FromUTF8(codec->long_name).c_str()));
            }
         }
      }
   }
   // Format is not found - find format in libavformat and add it's default audio codec
   // This allows us to provide limited support for NEW formats without modifying the compatibility list
   else if (found == 0)
   {
      wxCharBuffer buf = str.ToUTF8();
      AVOutputFormat *format = av_guess_format(buf,NULL,NULL);
      if (format != NULL)
      {
         AVCodec *codec = avcodec_find_encoder(format->audio_codec);
         if (codec != NULL && (codec->type == AVMEDIA_TYPE_AUDIO) && av_codec_is_encoder(codec))
         {
            if ((id >= 0) && codec->id == id) index = mShownCodecNames.GetCount();
            mShownCodecNames.Add(wxString::FromUTF8(codec->name));
            mShownCodecLongNames.Add(wxString::Format(wxT("%s - %s"),mShownCodecNames.Last().c_str(),wxString::FromUTF8(codec->long_name).c_str()));
         }
      }
   }
   // Show NEW codec list
   mCodecList->Append(mShownCodecNames);

   return index;
}

///
///
int ExportFFmpegOptions::FetchCompatibleFormatList(AVCodecID id, wxString *selfmt)
{
   int index = -1;
   mShownFormatNames.Clear();
   mShownFormatLongNames.Clear();
   mFormatList->Clear();
   AVOutputFormat *ofmt = NULL;
   ofmt = NULL;
   wxArrayString FromList;
   // Find all formats compatible to this codec in compatibility list
   for (int i = 0; CompatibilityList[i].fmt != NULL; i++)
   {
      if (CompatibilityList[i].codec == id || CompatibilityList[i].codec == AV_CODEC_ID_NONE)
      {
         if ((selfmt != NULL) && (selfmt->Cmp(CompatibilityList[i].fmt) == 0)) index = mShownFormatNames.GetCount();
         FromList.Add(CompatibilityList[i].fmt);
         mShownFormatNames.Add(CompatibilityList[i].fmt);
         AVOutputFormat *tofmt = av_guess_format(wxString(CompatibilityList[i].fmt).ToUTF8(),NULL,NULL);
         if (tofmt != NULL) mShownFormatLongNames.Add(wxString::Format(wxT("%s - %s"),CompatibilityList[i].fmt,wxString::FromUTF8(tofmt->long_name).c_str()));
      }
   }
   bool found = false;
   if (selfmt != NULL)
   {
      for (int i = 0; CompatibilityList[i].fmt != NULL; i++)
      {
         if (!selfmt->Cmp(CompatibilityList[i].fmt))
         {
            found = true;
            break;
         }
      }
   }
   // Format was in the compatibility list
   if (found)
   {
      // Find all formats which have this codec as default and which are not in the list yet and add them too
      while ((ofmt = av_oformat_next(ofmt))!=NULL)
      {
         if (ofmt->audio_codec == id)
         {
            wxString ofmtname = wxString::FromUTF8(ofmt->name);
            bool found = false;
            for (unsigned int i = 0; i < FromList.GetCount(); i++)
            {
               if (ofmtname.Cmp(FromList[i]) == 0)
               {
                  found = true;
                  break;
               }
            }
            if (!found)
            {
               if ((selfmt != NULL) && (selfmt->Cmp(wxString::FromUTF8(ofmt->name)) == 0)) index = mShownFormatNames.GetCount();
               mShownFormatNames.Add(wxString::FromUTF8(ofmt->name));
               mShownFormatLongNames.Add(wxString::Format(wxT("%s - %s"),mShownFormatNames.Last().c_str(),wxString::FromUTF8(ofmt->long_name).c_str()));
            }
         }
      }
   }
   mFormatList->Append(mShownFormatNames);
   return index;
}

///
///
void ExportFFmpegOptions::OnDeletePreset(wxCommandEvent& WXUNUSED(event))
{
   wxComboBox *preset = dynamic_cast<wxComboBox*>(FindWindowById(FEPresetID,this));
   wxString presetname = preset->GetValue();
   if (presetname.IsEmpty())
   {
      wxMessageBox(_("You can't delete a preset without name"));
      return;
   }

   wxString query = wxString::Format(_("Delete preset '%s'?"),presetname.c_str());
   int action = wxMessageBox(query,_("Confirm Deletion"),wxYES_NO | wxCENTRE);
   if (action == wxNO) return;

   mPresets->DeletePreset(presetname);
   long index = preset->FindString(presetname);
   preset->SetValue(wxEmptyString);
   preset->Delete(index);
   mPresetNames.Remove(presetname);
}

///
///
void ExportFFmpegOptions::OnSavePreset(wxCommandEvent& WXUNUSED(event))
{
   wxComboBox *preset = dynamic_cast<wxComboBox*>(FindWindowById(FEPresetID,this));
   wxString name = preset->GetValue();
   if (name.IsEmpty())
   {
      wxMessageBox(_("You can't save a preset without name"));
      return;
   }
   mPresets->SavePreset(this,name);
   int index = mPresetNames.Index(name.c_str(),false);
   if (index == -1)
   {
      mPresetNames.Add(name);
      mPresetCombo->Clear();
      mPresetCombo->Append(mPresetNames);
      mPresetCombo->Select(mPresetNames.Index(name,false));
   }
}

///
///
void ExportFFmpegOptions::OnLoadPreset(wxCommandEvent& WXUNUSED(event))
{
   wxComboBox *preset = dynamic_cast<wxComboBox*>(FindWindowById(FEPresetID,this));
   wxString presetname = preset->GetValue();

   mShownFormatNames = mFormatNames;
   mShownFormatLongNames = mFormatLongNames;
   mFormatList->Clear();
   mFormatList->Append(mFormatNames);

   mShownCodecNames = mCodecNames;
   mShownCodecLongNames = mCodecLongNames;
   mCodecList->Clear();
   mCodecList->Append(mCodecNames);

   mPresets->LoadPreset(this,presetname);

   DoOnFormatList();
   DoOnCodecList();
}

///
///
void ExportFFmpegOptions::OnImportPresets(wxCommandEvent& WXUNUSED(event))
{
   wxString path;
   FileDialog dlg(this,
                  _("Select xml file with presets to import"),
                  gPrefs->Read(wxT("/FileFormats/FFmpegPresetDir")),
                  wxEmptyString,
                  wxString(_("XML files (*.xml)|*.xml|All files|*")),
                  wxFD_OPEN);
   if (dlg.ShowModal() == wxID_CANCEL) return;
   path = dlg.GetPath();
   mPresets->ImportPresets(path);
   mPresets->GetPresetList(mPresetNames);
   mPresetCombo->Clear();
   mPresetCombo->Append(mPresetNames);
}

///
///
void ExportFFmpegOptions::OnExportPresets(wxCommandEvent& WXUNUSED(event))
{
   wxString path;
   FileDialog dlg(this,
                  _("Select xml file to export presets into"),
                  gPrefs->Read(wxT("/FileFormats/FFmpegPresetDir")),
                  wxEmptyString,
                  wxString(_("XML files (*.xml)|*.xml|All files|*")),
                  wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
   if (dlg.ShowModal() == wxID_CANCEL) return;
   path = dlg.GetPath();
   mPresets->ExportPresets(path);
}

///
///
void ExportFFmpegOptions::OnAllFormats(wxCommandEvent& WXUNUSED(event))
{
   mShownFormatNames = mFormatNames;
   mShownFormatLongNames = mFormatLongNames;
   mFormatList->Clear();
   mFormatList->Append(mFormatNames);
}

///
///
void ExportFFmpegOptions::OnAllCodecs(wxCommandEvent& WXUNUSED(event))
{
   mShownCodecNames = mCodecNames;
   mShownCodecLongNames = mCodecLongNames;
   mCodecList->Clear();
   mCodecList->Append(mCodecNames);
}

void ExportFFmpegOptions::EnableDisableControls(AVCodec *cdc, wxString *selfmt)
{
   int handled = -1;
   for (int i = 0; apptable[i].control != 0; i++)
   {
      if (apptable[i].control != handled)
      {
         bool codec = false;
         bool format = false;
         if (apptable[i].codec == AV_CODEC_ID_NONE) codec = true;
         else if (cdc != NULL && apptable[i].codec == cdc->id) codec = true;
         if (!wxString::FromUTF8(apptable[i].format).Cmp(wxT("any"))) format = true;
         else if (selfmt != NULL && selfmt->Cmp(wxString::FromUTF8(apptable[i].format)) == 0) format = true;
         if (codec && format)
         {
            handled = apptable[i].control;
            wxWindow *item = FindWindowById(apptable[i].control,this);
            if (item != NULL) item->Enable(apptable[i].enable);
         }
      }
   }
}

void ExportFFmpegOptions::DoOnFormatList()
{
   wxString *selfmt = NULL;
   wxString *selfmtlong = NULL;
   FindSelectedFormat(&selfmt, &selfmtlong);
   if (selfmt == NULL)
   {
      return;
   }

   wxString *selcdc = NULL;
   wxString *selcdclong = NULL;
   FindSelectedCodec(&selcdc, &selcdclong);

   AVOutputFormat *fmt = av_guess_format(selfmt->ToUTF8(),NULL,NULL);
   if (fmt == NULL)
   {
      //This shouldn't really happen
      mFormatName->SetLabel(wxString(_("Failed to guess format")));
      return;
   }
   mFormatName->SetLabel(wxString::Format(wxT("%s"),selfmtlong->c_str()));
   int selcdcid = -1;

   if (selcdc != NULL)
   {
      AVCodec *cdc = avcodec_find_encoder_by_name(selcdc->ToUTF8());
      if (cdc != NULL)
      {
         selcdcid = cdc->id;
      }
   }
   int newselcdc = FetchCompatibleCodecList(selfmt->c_str(), (AVCodecID)selcdcid);
   if (newselcdc >= 0) mCodecList->Select(newselcdc);

   AVCodec *cdc = NULL;
   if (selcdc != NULL)
      cdc = avcodec_find_encoder_by_name(selcdc->ToUTF8());
   EnableDisableControls(cdc, selfmt);
   Layout();
   Fit();
   return;
}

void ExportFFmpegOptions::DoOnCodecList()
{
   wxString *selcdc = NULL;
   wxString *selcdclong = NULL;
   FindSelectedCodec(&selcdc, &selcdclong);
   if (selcdc == NULL)
   {
      return;
   }

   wxString *selfmt = NULL;
   wxString *selfmtlong = NULL;
   FindSelectedFormat(&selfmt, &selfmtlong);

   AVCodec *cdc = avcodec_find_encoder_by_name(selcdc->ToUTF8());
   if (cdc == NULL)
   {
      //This shouldn't really happen
      mCodecName->SetLabel(wxString(_("Failed to find the codec")));
      return;
   }
   mCodecName->SetLabel(wxString::Format(wxT("[%d] %s"), (int) cdc->id,selcdclong->c_str()));

   if (selfmt != NULL)
   {
      AVOutputFormat *fmt = av_guess_format(selfmt->ToUTF8(),NULL,NULL);
      if (fmt == NULL)
      {
         selfmt = NULL;
         selfmtlong = NULL;
      }
   }

   int newselfmt = FetchCompatibleFormatList(cdc->id,selfmt);
   if (newselfmt >= 0) mFormatList->Select(newselfmt);

   EnableDisableControls(cdc, selfmt);
   Layout();
   Fit();
   return;
}

///
///
void ExportFFmpegOptions::OnFormatList(wxCommandEvent& WXUNUSED(event))
{
   DoOnFormatList();
}

///
///
void ExportFFmpegOptions::OnCodecList(wxCommandEvent& WXUNUSED(event))
{
   DoOnCodecList();
}

///
///
void ExportFFmpegOptions::OnOK(wxCommandEvent& WXUNUSED(event))
{
   int selcdc = mCodecList->GetSelection();
   int selfmt = mFormatList->GetSelection();
   if (selcdc > -1) gPrefs->Write(wxT("/FileFormats/FFmpegCodec"),mCodecList->GetString(selcdc));
   if (selfmt > -1) gPrefs->Write(wxT("/FileFormats/FFmpegFormat"),mFormatList->GetString(selfmt));
   gPrefs->Flush();

   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Flush();

   EndModal(wxID_OK);

   return;
}

#endif
