/**********************************************************************

   Audacity: A Digital Audio Editor

   ExportFFmpegDialogs.cpp

   Audacity(R) is copyright (c) 1999-2008 Audacity Team.
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

\class ExportFFmpegAMRWBOptions
\brief Options dialog for FFmpeg exporting of AMRWB format.

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
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../Track.h"
#include "../WaveTrack.h"

#include "Export.h"

#if defined(USE_FFMPEG)

extern FFmpegLibs *FFmpegLibsInst;

//----------------------------------------------------------------------------
// ExportFFmpegAC3Options Class
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ExportFFmpegAC3Options, wxDialog)
   EVT_BUTTON(wxID_OK,ExportFFmpegAC3Options::OnOK)
END_EVENT_TABLE()

// This initialises content for the static const member variables defined in
// ExportFFmpegDialogs.h (note no static keyword - important!)
const int ExportFFmpegAC3Options::iAC3BitRates[] = { 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 320000, 384000, 448000, 512000, 576000, 640000 };
const int ExportFFmpegAC3Options::iAC3SampleRates[] = { 32000, 44100, 48000, 0 };

ExportFFmpegAC3Options::ExportFFmpegAC3Options(wxWindow *parent)
:  wxDialog(parent, wxID_ANY,
            wxString(_("Specify AC3 Options")))
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iAC3BitRates)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iAC3BitRates[i]/1000));
      mBitRateLabels.Add(iAC3BitRates[i]);
   }

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegAC3Options::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("AC3 Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/AC3BitRate"), 
               160000, mBitRateNames, mBitRateLabels);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegAC3Options::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

//----------------------------------------------------------------------------
// ExportFFmpegAACOptions Class
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ExportFFmpegAACOptions, wxDialog)
   EVT_BUTTON(wxID_OK,ExportFFmpegAACOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegAACOptions::ExportFFmpegAACOptions(wxWindow *parent)
:  wxDialog(parent, wxID_ANY,
            wxString(_("Specify AAC Options")))
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegAACOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartStatic(_("AAC Export Setup"), 1);
   {
      S.StartTwoColumn();
      {
      S.TieSlider(_("Quality:"),wxT("/FileFormats/AACQuality"),100,500,10);
   }
      S.EndTwoColumn();
   }
   S.EndStatic();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegAACOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}


//----------------------------------------------------------------------------
// ExportFFmpegAMRNBOptions Class
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ExportFFmpegAMRNBOptions, wxDialog)
   EVT_BUTTON(wxID_OK,ExportFFmpegAMRNBOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegAMRNBOptions::ExportFFmpegAMRNBOptions(wxWindow *parent)
:  wxDialog(parent, wxID_ANY,
            wxString(_("Specify AMR-NB Options")))
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iAMRNBBitRate)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iAMRNBBitRate[i]/1000));
      mBitRateLabels.Add(iAMRNBBitRate[i]);
   }

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegAMRNBOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("AMR-NB Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/AMRNBBitRate"), 
               12200, mBitRateNames, mBitRateLabels);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegAMRNBOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

//----------------------------------------------------------------------------
// ExportFFmpegAMRWBOptions Class
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ExportFFmpegAMRWBOptions, wxDialog)
   EVT_BUTTON(wxID_OK,ExportFFmpegAMRWBOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegAMRWBOptions::ExportFFmpegAMRWBOptions(wxWindow *parent)
:  wxDialog(parent, wxID_ANY,
            wxString(_("Specify AMR-WB Options")))
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iAMRWBBitRate)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iAMRWBBitRate[i]/1000));
      mBitRateLabels.Add(iAMRWBBitRate[i]);
   }

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegAMRWBOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("AMR-WB Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/AMRWBBitRate"), 
               23850, mBitRateNames, mBitRateLabels);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegAMRWBOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

//----------------------------------------------------------------------------
// ExportFFmpegWMAOptions Class
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ExportFFmpegWMAOptions, wxDialog)
   EVT_BUTTON(wxID_OK,ExportFFmpegWMAOptions::OnOK)
END_EVENT_TABLE()

const int ExportFFmpegWMAOptions::iWMASampleRates[] = { 8000, 11025, 16000, 22050, 44100, 0};

ExportFFmpegWMAOptions::ExportFFmpegWMAOptions(wxWindow *parent)
:  wxDialog(parent, wxID_ANY,
            wxString(_("Specify WMA Options")))
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iWMABitRate)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iWMABitRate[i]/1000));
      mBitRateLabels.Add(iWMABitRate[i]);
   }

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegWMAOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("WMA Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/WMABitRate"), 
               180189, mBitRateNames, mBitRateLabels);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegWMAOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

FFmpegPreset::FFmpegPreset(wxString &name)
{
   mPresetName = new wxString(name);
   mControlState = new wxArrayString();
   mControlState->SetCount(FELastID - FEFirstID);
}

FFmpegPreset::~FFmpegPreset()
{
   delete mPresetName;
   delete mControlState;
}

WX_DEFINE_LIST(FFmpegPresetList);


FFmpegPresets::FFmpegPresets()
{
   mPresets = new FFmpegPresetList();
   mPresets->DeleteContents(true);
   XMLFileReader xmlfile;
   wxFileName xmlFileName(FileNames::DataDir(), wxT("ffmpeg_presets.xml"));
   xmlfile.Parse(this,xmlFileName.GetFullPath());
}

FFmpegPresets::~FFmpegPresets()
{
   XMLFileWriter writer;
   // FIXME: Catch XMLFileWriterException
   wxFileName xmlFileName(FileNames::DataDir(), wxT("ffmpeg_presets.xml"));
   writer.Open(xmlFileName.GetFullPath(),wxT("wb"));
   WriteXMLHeader(writer);
   WriteXML(writer);
   delete mPresets;
}

void FFmpegPresets::ImportPresets(wxString &filename)
{
   XMLFileReader xmlfile;
   xmlfile.Parse(this,filename);
}

void FFmpegPresets::ExportPresets(wxString &filename)
{
   XMLFileWriter writer;
   // FIXME: Catch XMLFileWriterException
   writer.Open(filename,wxT("wb"));
   WriteXMLHeader(writer);
   WriteXML(writer);
}

wxArrayString *FFmpegPresets::GetPresetList()
{
   wxArrayString *list = new wxArrayString();
   FFmpegPresetList::iterator iter;
   for (iter = mPresets->begin(); iter != mPresets->end(); ++iter)
   {
      FFmpegPreset *preset = *iter;
      list->Add(*preset->mPresetName);
   }   
   return list;
}

void FFmpegPresets::DeletePreset(wxString &name)
{
   FFmpegPresetList::iterator iter;
   for (iter = mPresets->begin(); iter != mPresets->end(); ++iter)
   {
      FFmpegPreset *preset = *iter;
      if (!preset->mPresetName->CmpNoCase(name))
      {
         mPresets->erase(iter);
         delete preset;
         break;
      }
   }
}

FFmpegPreset *FFmpegPresets::FindPreset(wxString &name)
{
   FFmpegPreset *preset = NULL;
   FFmpegPresetList::iterator iter;
   for (iter = mPresets->begin(); iter != mPresets->end(); ++iter)
   {
      FFmpegPreset *current = *iter;
      if (!current->mPresetName->CmpNoCase(name))
         preset = current;
   }
   return preset;
}

void FFmpegPresets::SavePreset(ExportFFmpegOptions *parent, wxString &name)
{
   FFmpegPreset *preset = FindPreset(name);
   if (preset)
   {
      wxString query = wxString::Format(_("Overwrite preset '%s'?"),name.c_str());
      int action = wxMessageBox(query,_("Confirm Overwrite"),wxYES_NO | wxCENTRE);
      if (action == wxNO) return;
   }
   else
   {
      wxWindow *wnd;
      wxListBox *lb;

      wnd = dynamic_cast<wxWindow*>(parent)->FindWindowById(FEFormatID,parent);
      lb = dynamic_cast<wxListBox*>(wnd);
      if (lb->GetSelection() < 0)
      {
         wxMessageBox(_("Please select format before saving a profile"));
         return;
      }

      wnd = dynamic_cast<wxWindow*>(parent)->FindWindowById(FECodecID,parent);
      lb = dynamic_cast<wxListBox*>(wnd);
      if (lb->GetSelection() < 0)
      {
         wxMessageBox(_("Please select codec before saving a profile"));
         return;
      }
      preset = new FFmpegPreset(name);
      mPresets->push_front(preset);
   }

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
            break;
         case FECodecID:
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
            preset->mControlState->Item(id - FEFirstID) = wxString::Format(wxT("%d"),sc->GetValue());
            break;
         // Text control
         case FELanguageID:
         case FETagID:
            tc = dynamic_cast<wxTextCtrl*>(wnd);
            preset->mControlState->Item(id - FEFirstID) = tc->GetValue();
            break;
         // Choice
         case FEProfileID:
         case FEPredOrderID:
            ch = dynamic_cast<wxChoice*>(wnd);
            preset->mControlState->Item(id - FEFirstID) = wxString::Format(wxT("%d"),ch->GetSelection());
            break;
         // Check box
         case FEUseLPCID:
         case FEBitReservoirID:
         case FEVariableBlockLenID:
            cb = dynamic_cast<wxCheckBox*>(wnd);
            preset->mControlState->Item(id - FEFirstID) = wxString::Format(wxT("%d"),cb->GetValue());
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
            readstr = preset->mControlState->Item(id - FEFirstID);
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
            preset->mControlState->Item(id - FEFirstID).ToLong(&readlong);
            sc->SetValue(readlong);
            break;
         // Text control
         case FELanguageID:
         case FETagID:
            tc = dynamic_cast<wxTextCtrl*>(wnd);
            tc->SetValue(preset->mControlState->Item(id - FEFirstID));
            break;
         // Choice
         case FEProfileID:
         case FEPredOrderID:
            ch = dynamic_cast<wxChoice*>(wnd);
            preset->mControlState->Item(id - FEFirstID).ToLong(&readlong);
            if (readlong > -1) ch->Select(readlong);
            break;
         // Check box
         case FEUseLPCID:
         case FEBitReservoirID:
         case FEVariableBlockLenID:
            cb = dynamic_cast<wxCheckBox*>(wnd);
            preset->mControlState->Item(id - FEFirstID).ToLong(&readlong);
            if (readlong) readbool = true; else readbool = false;
            cb->SetValue(readbool);
            break;
         }
      }
   }
}

bool FFmpegPresets::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag,wxT("ffmpeg_presets")))
   {
      return true;
   }
   else if (!wxStrcmp(tag,wxT("preset")))
   {
      while (*attrs)
      {
         const wxChar *attr = *attrs++;
         wxString value = *attrs++;

         if (!value)
            break;

         if (!wxStrcmp(attr,wxT("name")))
         {
            FFmpegPreset *newpreset = FindPreset(value);
            if (!newpreset)
               mPresets->push_front(new FFmpegPreset(value));
            else
            {
               mPresets->remove(newpreset);
               mPresets->push_front(newpreset);
            }
         }
      }
      return true;
   }
   else if (!wxStrcmp(tag,wxT("setctrlstate")))
   {
      FFmpegPreset *preset = mPresets->front();
      long id = -1;
      if (!preset) return false;
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
               preset->mControlState->Item(id - FEFirstID) = wxString(value);
         }
      }
      return true;
   }
   return false;
}

XMLTagHandler *FFmpegPresets::HandleXMLChild(const wxChar *tag)
{
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
   FFmpegPresetList::iterator iter;
   for (iter = mPresets->begin(); iter != mPresets->end(); ++iter)
   {
      FFmpegPreset *preset = *iter;
      xmlFile.StartTag(wxT("preset"));
      xmlFile.WriteAttr(wxT("name"),*preset->mPresetName);
      for (long i = FEFirstID + 1; i < FELastID; i++)
      {
         xmlFile.StartTag(wxT("setctrlstate"));
         xmlFile.WriteAttr(wxT("id"),wxString(FFmpegExportCtrlIDNames[i - FEFirstID]));
         xmlFile.WriteAttr(wxT("state"),preset->mControlState->Item(i - FEFirstID));
         xmlFile.EndTag(wxT("setctrlstate"));
      }
      xmlFile.EndTag(wxT("preset"));
   }
   xmlFile.EndTag(wxT("ffmpeg_presets"));
}

//----------------------------------------------------------------------------
// ExportFFmpegOptions Class
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ExportFFmpegOptions, wxDialog)
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


ExportFFmpegOptions::~ExportFFmpegOptions()
{
   delete mPresets;
   delete mPresetNames;
   DropFFmpegLibs();
}

ExportFFmpegOptions::ExportFFmpegOptions(wxWindow *parent)
:  wxDialog(parent, wxID_ANY,
            wxString(_("Specify Other Options")))
{
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PickFFmpegLibs();
   //FFmpegLibsInst->LoadLibs(NULL,true); //Loaded at startup or from Prefs now

   mPresets = new FFmpegPresets();
   mPresetNames = mPresets->GetPresetList();

   if (FFmpegLibsInst->ValidLibsLoaded())
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
      AVCodec *codec = FFmpegLibsInst->avcodec_find_encoder((CodecID)gPrefs->Read(wxT("/FileFormats/FFmpegCodec"),(long)CODEC_ID_NONE));
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
   while ((ofmt = FFmpegLibsInst->av_oformat_next(ofmt)))
   {
      // Any audio-capable format has default audio codec.
      // If it doesn't, then it doesn't supports any audio codecs
      if (ofmt->audio_codec != CODEC_ID_NONE)
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
   while ((codec = FFmpegLibsInst->av_codec_next(codec)))
   {
      // We're only interested in audio and only in encoders
      if (codec->type == CODEC_TYPE_AUDIO && codec->encode)
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
        mPresetCombo = S.Id(FEPresetID).AddCombo(_("Preset:"), gPrefs->Read(wxT("/FileFormats/FFmpegPreset"),wxEmptyString), mPresetNames);
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
     S.AddVariableText(_("Not all formats and codecs are compatible. Nor are all option combinations, (such as bitrates and samplerates), compatible with all codecs."), false);
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
//         S.StartScroller( );
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
                 mMuxRate = S.Id(FEMuxRateID).TieSpinCtrl(_("Mux Rate:"), wxT("/FileFormats/FFmpegMuxRate"), 0, 10000000, 0);
                 mMuxRate->SetToolTip(_("Maximum bit rate of the multiplexed stream\nOptional\n0 - default"));

                 mPacketSize = S.Id(FEPacketSizeID).TieSpinCtrl(_("Packet Size:"), wxT("/FileFormats/FFmpegPacketSize"), 0, 10000000, 0);
                 mPacketSize->SetToolTip(_("Packet size\nOptional\n0 - default"));
              }
              S.EndMultiColumn();
           }
           S.EndStatic();
//         S.EndScroller();
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
int ExportFFmpegOptions::FetchCompatibleCodecList(const wxChar *fmt, CodecID id)
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
         if (CompatibilityList[i].codec == CODEC_ID_NONE)
         {
            // Format is found in the list and it is compatible with CODEC_ID_NONE (means that it is compatible to anything)
            found = 2;
            break;
         }
         // Find the codec, that is claimed to be compatible
         AVCodec *codec = FFmpegLibsInst->avcodec_find_encoder(CompatibilityList[i].codec);
         // If it exists, is audio and has encoder
         if (codec != NULL && (codec->type == CODEC_TYPE_AUDIO) && codec->encode)
         {
            // If it was selected - remember it's new index
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
      while ((codec = FFmpegLibsInst->av_codec_next(codec)))
      {
         if (codec->type == CODEC_TYPE_AUDIO && codec->encode)
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
   // This allows us to provide limited support for new formats without modifying the compatibility list
   else if (found == 0)
   {
      wxCharBuffer buf = str.ToUTF8();
      AVOutputFormat *format = FFmpegLibsInst->guess_format(buf,NULL,NULL);
      if (format != NULL)
      {
         AVCodec *codec = FFmpegLibsInst->avcodec_find_encoder(format->audio_codec);
         if (codec != NULL && (codec->type == CODEC_TYPE_AUDIO) && codec->encode)
         {
            if ((id >= 0) && codec->id == id) index = mShownCodecNames.GetCount();
            mShownCodecNames.Add(wxString::FromUTF8(codec->name));
            mShownCodecLongNames.Add(wxString::Format(wxT("%s - %s"),mShownCodecNames.Last().c_str(),wxString::FromUTF8(codec->long_name).c_str()));
         }
      }
   }
   // Show new codec list
   mCodecList->Append(mShownCodecNames);

   return index;
}

///
///
int ExportFFmpegOptions::FetchCompatibleFormatList(CodecID id, wxString *selfmt)
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
      if (CompatibilityList[i].codec == id || CompatibilityList[i].codec == CODEC_ID_NONE)
      {
         if ((selfmt != NULL) && (selfmt->Cmp(CompatibilityList[i].fmt) == 0)) index = mShownFormatNames.GetCount();
         FromList.Add(CompatibilityList[i].fmt);
         mShownFormatNames.Add(CompatibilityList[i].fmt);
         AVOutputFormat *tofmt = FFmpegLibsInst->guess_format(wxString(CompatibilityList[i].fmt).ToUTF8(),NULL,NULL);
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
      while ((ofmt = FFmpegLibsInst->av_oformat_next(ofmt)))
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
void ExportFFmpegOptions::OnDeletePreset(wxCommandEvent& event)
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
   mPresetNames->Remove(presetname);
}

///
///
void ExportFFmpegOptions::OnSavePreset(wxCommandEvent& event)
{
   wxComboBox *preset = dynamic_cast<wxComboBox*>(FindWindowById(FEPresetID,this));
   wxString name = preset->GetValue();
   if (name.IsEmpty())
   {
      wxMessageBox(_("You can't save a preset without name"));
      return;
   }
   mPresets->SavePreset(this,name);
   int index = mPresetNames->Index(name.c_str(),false);
   if (index == -1)
   {
      mPresetNames->Add(name);
      mPresetCombo->Clear();
      mPresetCombo->Append(*mPresetNames);
      mPresetCombo->Select(mPresetNames->Index(name,false));
   }
}

///
///
void ExportFFmpegOptions::OnLoadPreset(wxCommandEvent& event)
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
}

///
///
void ExportFFmpegOptions::OnImportPresets(wxCommandEvent& event)
{
   wxString path;
   FileDialog dlg(this,
                  _("Select xml file with presets to import"),
                  gPrefs->Read(wxT("/FileFormats/FFmpegPresetDir")),
                  wxEmptyString,
                  wxString(_("XML files (*.xml)|*.xml|All files (*.*)|*.*")),
                  wxFD_OPEN);
   if (dlg.ShowModal() == wxID_CANCEL) return;
   path = dlg.GetPath();
   mPresets->ImportPresets(path);
   delete mPresetNames;
   mPresetNames = mPresets->GetPresetList();
   mPresetCombo->Clear();
   mPresetCombo->Append(*mPresetNames);
}

///
///
void ExportFFmpegOptions::OnExportPresets(wxCommandEvent& event)
{
   wxString path;
   FileDialog dlg(this,
                  _("Select xml file to export presets into"),
                  gPrefs->Read(wxT("/FileFormats/FFmpegPresetDir")),
                  wxEmptyString,
                  wxString(_("XML files (*.xml)|*.xml|All files (*.*)|*.*")),
                  wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
   if (dlg.ShowModal() == wxID_CANCEL) return;
   path = dlg.GetPath();
   mPresets->ExportPresets(path);
}

///
///
void ExportFFmpegOptions::OnAllFormats(wxCommandEvent& event)
{
   mShownFormatNames = mFormatNames;
   mShownFormatLongNames = mFormatLongNames;
   mFormatList->Clear();
   mFormatList->Append(mFormatNames);
}

///
///
void ExportFFmpegOptions::OnAllCodecs(wxCommandEvent& event)
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
         if (apptable[i].codec == CODEC_ID_NONE) codec = true;
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

   AVOutputFormat *fmt = FFmpegLibsInst->guess_format(selfmt->ToUTF8(),NULL,NULL);
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
      AVCodec *cdc = FFmpegLibsInst->avcodec_find_encoder_by_name(selcdc->ToUTF8());
      if (cdc != NULL)
      {
         selcdcid = cdc->id;
      }
   }
   int newselcdc = FetchCompatibleCodecList(selfmt->c_str(), (CodecID)selcdcid);
   if (newselcdc >= 0) mCodecList->Select(newselcdc);

   AVCodec *cdc = NULL;
   if (selcdc != NULL)
      cdc = FFmpegLibsInst->avcodec_find_encoder_by_name(selcdc->ToUTF8());
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

   AVCodec *cdc = FFmpegLibsInst->avcodec_find_encoder_by_name(selcdc->ToUTF8());
   if (cdc == NULL)
   {
      //This shouldn't really happen
      mCodecName->SetLabel(wxString(_("Failed to find the codec")));
      return;
   }
   mCodecName->SetLabel(wxString::Format(wxT("[%d] %s"),cdc->id,selcdclong->c_str()));

   if (selfmt != NULL)
   {
      AVOutputFormat *fmt = FFmpegLibsInst->guess_format(selfmt->ToUTF8(),NULL,NULL);
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
void ExportFFmpegOptions::OnFormatList(wxCommandEvent& event)
{
   DoOnFormatList();
}

///
///
void ExportFFmpegOptions::OnCodecList(wxCommandEvent& event)
{
   DoOnCodecList();
}

///
///
void ExportFFmpegOptions::OnOK(wxCommandEvent& event)
{
   int selcdc = mCodecList->GetSelection();
   int selfmt = mFormatList->GetSelection();
   if (selcdc > -1) gPrefs->Write(wxT("/FileFormats/FFmpegCodec"),(long)FFmpegLibsInst->avcodec_find_encoder_by_name(mCodecList->GetString(selcdc).ToUTF8())->id);
   if (selfmt > -1) gPrefs->Write(wxT("/FileFormats/FFmpegFormat"),mFormatList->GetString(selfmt));
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

#endif
