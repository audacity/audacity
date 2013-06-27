/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

   Reverb.cpp
   Rob Sykes, Vaughan Johnson

******************************************************************//**

\class EffectReverb
\brief A reverberation effect

*//****************************************************************//**

\class ReverbDialogue
\brief A configuration class used by effect, EffectReverb.

*//*******************************************************************/

#include "Reverb.h"
#include "Reverb_libSoX.h"
#include "../Prefs.h"

//
// EffectReverb
//

struct Reverb_priv_t {
   size_t ichannels, ochannels;
   struct {
      reverb_t reverb;
      float * dry, * wet[2];
   } chan[2];
};

void EffectReverb::Create(double rate, bool isStereo)
{
   mP = (Reverb_priv_t *)calloc(sizeof *mP, 1);
   size_t i;

#define BLOCK 0x4000u
   mP->ichannels = mP->ochannels = 1 + isStereo;
   for (i = 0; i < mP->ichannels; ++i)
      reverb_create(
         &mP->chan[i].reverb, rate, mParams.mWetGain, mParams.mRoomSize,
         mParams.mReverberance, mParams.mHfDamping, mParams.mDelay, mParams.mStereoWidth*isStereo,
         mParams.mToneLow, mParams.mToneHigh, BLOCK, mP->chan[i].wet);
}

bool EffectReverb::ProcessOneBlock(sampleCount len0, float * const * chans0)
{
   float * chans[2];
   chans[0] = chans0[0], chans[1] = chans0[1];
   size_t c, i, w, len = len0;
   float const dryMult(mParams.mWetOnly? 0 : dB_to_linear(mParams.mDryGain));
   while (len) {
      size_t len1 = min(len, (size_t)BLOCK);
      for (c = 0; c < mP->ichannels; ++c) {
         mP->chan[c].dry = (float *)fifo_write(&mP->chan[c].reverb.input_fifo, len1, chans[c]);
         reverb_process(&mP->chan[c].reverb, len1);
      }
      if (mP->ichannels == 2)
         for (i = 0; i < len1; ++i)
            for (w = 0; w < 2; ++w)
               chans[w][i] = dryMult * mP->chan[w].dry[i] +
                  .5 * (mP->chan[0].wet[w][i] + mP->chan[1].wet[w][i]);
      else for (i = 0; i < len1; ++i)
         for (w = 0; w < mP->ochannels; ++w)
            chans[0][i] = dryMult * mP->chan[0].dry[i] + mP->chan[0].wet[w][i];
      len -= len1;
      for (c = 0; c < mP->ichannels; chans[c++] += len1);
   }
   return true;
}

void EffectReverb::Delete()
{
   for (size_t i = 0; i < mP->ichannels; reverb_delete(&mP->chan[i++].reverb));
   free(mP);
}

// Most of what follows should really be provided by Audacity framework classes:
//vvv Not sure what you mean by that, Rob. A la EffectSimpleMono, e.g.?

bool EffectReverb::ProcessOneTrack(size_t n, WaveTrack * track, WaveTrack * track2, wxString const & msg)
{
   sampleCount begin = track->TimeToLongSamples(mCurT0), pos = begin;
   sampleCount end = track->TimeToLongSamples(mCurT1);
   float * buffers[2];
   buffers[0] = new float[track->GetMaxBlockSize()];
   buffers[1] = track2? new float[track->GetMaxBlockSize()] : 0;
   bool cancelled = false;

   Create(track->GetRate(), track2 != 0);
   while (!cancelled && pos < end) {
      sampleCount block = track->GetBestBlockSize(pos);
      block = min(block, end - pos);
      track->Get((samplePtr) buffers[0], floatSample, pos, block);
      if (track2)
         track2->Get((samplePtr) buffers[1], floatSample, pos, block);
      ProcessOneBlock(block, buffers);
      track->Set((samplePtr) buffers[0], floatSample, pos, block);
      if (track2)
         track2->Set((samplePtr) buffers[1], floatSample, pos, block);
      pos += block;
      cancelled = TrackProgress(n, (1. * pos - begin) / (1. * end - begin) * .5 + .5, msg);
   }
   Delete();
   delete[] buffers[0];
   delete[] buffers[1];
   return !cancelled;
}

bool EffectReverb::Process()
{
   CopyInputTracks(); // Set up mOutputTracks.
   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks);
   WaveTrack * track = (WaveTrack *)iter.First();
   bool success = true;
   for (int n = 0; success && track; track = (WaveTrack *)iter.Next(), ++n) {
      mCurT0 = max(mT0, track->GetStartTime());
      mCurT1 = min(mT1, track->GetEndTime());
      if (mCurT1 > mCurT0) {
         wxString msg(_("Processing: ") + track->GetName());
         if (track->GetLinked() && mParams.mStereoWidth) // do stereo?
            success = ProcessOneTrack(++n, track, (WaveTrack *)iter.Next(), msg);
         else success = ProcessOneTrack(n, track, 0, msg);
      }
   }
   ReplaceProcessedTracks(success); 
   return success;
}

wxString EffectReverb::SettingsPath(int settingsNumber) const
{
   wxString x(wxString::FromAscii("/Effects/Reverb/"));
   if (settingsNumber >= 0)
      x += wxString::Format(wxString::FromAscii("%i/"), settingsNumber);
   return x;
}

wxString EffectReverb::SettingsName(int settingsNumber) const
{
   wxString x(SettingsPath(settingsNumber));
   gPrefs->Read(x + wxT("name"), &x, wxString::Format(wxString::FromAscii("Settings%i"), settingsNumber));
   return x;
}

void EffectReverb::LoadSettings(int settingsNumber, EffectReverb::Params & params)
{
   wxString sSettingsPath(SettingsPath(settingsNumber));
   gPrefs->Read(sSettingsPath + wxT("RoomSize"), &params.mRoomSize, 75);
   gPrefs->Read(sSettingsPath + wxT("Delay"), &params.mDelay, 10);
   gPrefs->Read(sSettingsPath + wxT("Reverberance"), &params.mReverberance, 50);
   gPrefs->Read(sSettingsPath + wxT("HfDamping"), &params.mHfDamping, 50);
   gPrefs->Read(sSettingsPath + wxT("ToneLow"), &params.mToneLow, 100);
   gPrefs->Read(sSettingsPath + wxT("ToneHigh"), &params.mToneHigh, 100);
   gPrefs->Read(sSettingsPath + wxT("WetGain"), &params.mWetGain, -1);
   gPrefs->Read(sSettingsPath + wxT("DryGain"), &params.mDryGain, -1);
   gPrefs->Read(sSettingsPath + wxT("StereoWidth"), &params.mStereoWidth, 100);
   gPrefs->Read(sSettingsPath + wxT("WetOnly"), &params.mWetOnly, 0);
}

void EffectReverb::SaveSettings(int settingsNumber, EffectReverb::Params const * params, wxString const * name) const
{
   wxString sSettingsPath(SettingsPath(settingsNumber));
   if (name)
      gPrefs->Write(sSettingsPath + wxT("name"), *name);
   if (params) 
   {
      gPrefs->Write(sSettingsPath + wxT("RoomSize"), params->mRoomSize);
      gPrefs->Write(sSettingsPath + wxT("Delay"), params->mDelay);
      gPrefs->Write(sSettingsPath + wxT("Reverberance"), params->mReverberance);
      gPrefs->Write(sSettingsPath + wxT("HfDamping"), params->mHfDamping);
      gPrefs->Write(sSettingsPath + wxT("ToneLow"), params->mToneLow);
      gPrefs->Write(sSettingsPath + wxT("ToneHigh"), params->mToneHigh);
      gPrefs->Write(sSettingsPath + wxT("WetGain"), params->mWetGain);
      gPrefs->Write(sSettingsPath + wxT("DryGain"), params->mDryGain);
      gPrefs->Write(sSettingsPath + wxT("StereoWidth"), params->mStereoWidth);
      gPrefs->Write(sSettingsPath + wxT("WetOnly"), params->mWetOnly);
   }
   gPrefs->Flush();
}

EffectReverb::EffectReverb()
{
   LoadSettings(-1, mParams);
}

wxString EffectReverb::GetEffectDescription() 
{
   wxString strResult = 
      wxString::Format(_("Applied effect: %s"), GetEffectName().c_str());
   strResult += wxString::Format(_(", Room Size = %.0f"), mParams.mRoomSize);
   strResult += wxString::Format(_(", Delay = %.0fms"), mParams.mDelay);
   strResult += wxString::Format(_(", Reverberance = %.0f%%"), mParams.mReverberance);
   strResult += wxString::Format(_(", Damping = %.0f%%"), mParams.mHfDamping);
   strResult += wxString::Format(_(", Tone Low = %.0f%%"), mParams.mToneLow);
   strResult += wxString::Format(_(", Tone High = %.0f%%"), mParams.mToneHigh);
   strResult += wxString::Format(_(", Wet Gain = %.0fdB"), mParams.mWetGain);
   strResult += wxString::Format(_(", Dry Gain Size = %.0fdB"), mParams.mDryGain);
   strResult += wxString::Format(_(", Stereo Width = %.0f%%"), mParams.mStereoWidth);
   strResult += wxString::Format(_(", Wet Only = %s"), mParams.mWetOnly ? _("true") : _("false"));
   return strResult;
} 

bool EffectReverb::PromptUser()
{
   ReverbDialogue d(this, mParent);
   d.CentreOnParent();
   d.ShowModal();
   if (d.GetReturnCode() == wxID_CANCEL)
      return false;
   SaveSettings(-1, &mParams);
   return true;
}

bool EffectReverb::TransferParameters(Shuttle & shuttle)
{
   shuttle.TransferDouble(wxT("RoomSize"), mParams.mRoomSize, 75);
   shuttle.TransferDouble(wxT("Delay"), mParams.mDelay, 10);
   shuttle.TransferDouble(wxT("Reverberance"), mParams.mReverberance, 50);
   shuttle.TransferDouble(wxT("HfDamping"), mParams.mHfDamping, 50);
   shuttle.TransferDouble(wxT("ToneLow"), mParams.mToneLow, 100);
   shuttle.TransferDouble(wxT("ToneHigh"), mParams.mToneHigh, 100);
   shuttle.TransferDouble(wxT("WetGain"), mParams.mWetGain, -1);
   shuttle.TransferDouble(wxT("DryGain"), mParams.mDryGain, -1);
   shuttle.TransferDouble(wxT("StereoWidth"), mParams.mStereoWidth, 100);

   shuttle.TransferBool(wxT("WetOnly"), mParams.mWetOnly, 0);

   return true;
}

//----------------------------------------------------------------------------
// ReverbDialogue
//----------------------------------------------------------------------------

#include <wx/button.h>
#include <wx/choicdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/spinctrl.h>
#include <wx/textdlg.h>

enum {ID_START = 10000,
   ID_PRESETS,
   ID_LOAD_SETTINGS,
   ID_SAVE_SETTINGS,
   ID_RENAME_SETTINGS,

   ID_RoomSize_TEXT, ID_RoomSize_WIDGET,
   ID_Delay_TEXT, ID_Delay_WIDGET,
   ID_Reverberance_TEXT, ID_Reverberance_WIDGET,
   ID_HfDamping_TEXT, ID_HfDamping_WIDGET,
   ID_ToneLow_TEXT, ID_ToneLow_WIDGET,
   ID_ToneHigh_TEXT, ID_ToneHigh_WIDGET,
   ID_WetGain_TEXT, ID_WetGain_WIDGET,
   ID_DryGain_TEXT, ID_DryGain_WIDGET,
   ID_StereoWidth_TEXT, ID_StereoWidth_WIDGET,

   ID_WetOnly_WIDGET,

   ID_END};

BEGIN_EVENT_TABLE(ReverbDialogue, EffectDialog)
   EVT_SLIDER(ID_RoomSize_WIDGET, ReverbDialogue::OnRoomSizeWidget)
   EVT_TEXT(ID_RoomSize_TEXT, ReverbDialogue::OnRoomSizeText)

   EVT_SLIDER(ID_Delay_WIDGET, ReverbDialogue::OnDelayWidget)
   EVT_TEXT(ID_Delay_TEXT, ReverbDialogue::OnDelayText)
   
   EVT_SLIDER(ID_Reverberance_WIDGET, ReverbDialogue::OnReverberanceWidget)
   EVT_TEXT(ID_Reverberance_TEXT, ReverbDialogue::OnReverberanceText)
   
   EVT_SLIDER(ID_HfDamping_WIDGET, ReverbDialogue::OnHfDampingWidget)
   EVT_TEXT(ID_HfDamping_TEXT, ReverbDialogue::OnHfDampingText)
   
   EVT_SLIDER(ID_ToneLow_WIDGET, ReverbDialogue::OnToneLowWidget)
   EVT_TEXT(ID_ToneLow_TEXT, ReverbDialogue::OnToneLowText)
   
   EVT_SLIDER(ID_ToneHigh_WIDGET, ReverbDialogue::OnToneHighWidget)
   EVT_TEXT(ID_ToneHigh_TEXT, ReverbDialogue::OnToneHighText)
   
   EVT_SLIDER(ID_WetGain_WIDGET, ReverbDialogue::OnWetGainWidget)
   EVT_TEXT(ID_WetGain_TEXT, ReverbDialogue::OnWetGainText)
   
   EVT_SLIDER(ID_DryGain_WIDGET, ReverbDialogue::OnDryGainWidget)
   EVT_TEXT(ID_DryGain_TEXT, ReverbDialogue::OnDryGainText)
   
   EVT_SLIDER(ID_StereoWidth_WIDGET, ReverbDialogue::OnStereoWidthWidget)
   EVT_TEXT(ID_StereoWidth_TEXT, ReverbDialogue::OnStereoWidthText)

   EVT_BUTTON(ePreviewID, ReverbDialogue::OnPreview)
   EVT_BUTTON(ePreviewDryID, ReverbDialogue::OnPreview)
   EVT_BUTTON(ID_PRESETS, ReverbDialogue::LoadPreset)
   EVT_BUTTON(ID_SAVE_SETTINGS, ReverbDialogue::SaveSettings)
   EVT_BUTTON(ID_LOAD_SETTINGS, ReverbDialogue::LoadSettings)
   EVT_BUTTON(ID_RENAME_SETTINGS, ReverbDialogue::RenameSettings)
END_EVENT_TABLE()

ReverbDialogue::ReverbDialogue(EffectReverb * effect, wxWindow * parent):
   EffectDialog(parent, _("Reverb"), PROCESS_EFFECT, wxDEFAULT_DIALOG_STYLE, ePreviewDryButton),
   mEffect(*effect), mParams(effect->mParams)
{
   Init();
}

void ReverbDialogue::PopulateOrExchange(ShuttleGui & s)
{
   s.AddSpace(0, 5);

   s.StartMultiColumn(3, wxEXPAND); 
   {
      s.SetStretchyCol(2);

      mRoomSizeText = s.Id(ID_RoomSize_TEXT).AddSpinCtrl(_("Room Size (%):"), 0, 100, 0); 
      s.SetStyle(wxSL_HORIZONTAL);
      mRoomSizeWidget = s.Id(ID_RoomSize_WIDGET).AddSlider(wxT(""), 0, 100, 0);

      // Rob's original code referred to this param as "Delay". 
      // Then, May 11, 2013, in a thread on [Audacity-quality], subject "Reverb effect", 
      // Steve suggested and Gale seconded renaming it "Pre-delay". 
      // I've changed it only here, in the GUI, and left the rest of the code as *Delay*.
      mDelayText = s.Id(ID_Delay_TEXT).AddSpinCtrl(_("Pre-delay (ms):"), 0, 200, 0);
      s.SetStyle(wxSL_HORIZONTAL);
      mDelayWidget = s.Id(ID_Delay_WIDGET).AddSlider(wxT(""), 0, 200, 0);

      mReverberanceText = s.Id(ID_Reverberance_TEXT).AddSpinCtrl(_("Reverberance (%):"), 0, 100, 0);
      s.SetStyle(wxSL_HORIZONTAL);
      mReverberanceWidget = s.Id(ID_Reverberance_WIDGET).AddSlider(wxT(""), 0, 100, 0);

      mHfDampingText = s.Id(ID_HfDamping_TEXT).AddSpinCtrl(_("Damping (%):"), 0, 100, 0);
      s.SetStyle(wxSL_HORIZONTAL);
      mHfDampingWidget = s.Id(ID_HfDamping_WIDGET).AddSlider(wxT(""), 0, 100, 0);

      mToneLowText = s.Id(ID_ToneLow_TEXT).AddSpinCtrl(_("Tone Low (%):"), 0, 100, 0);
      s.SetStyle(wxSL_HORIZONTAL);
      mToneLowWidget = s.Id(ID_ToneLow_WIDGET).AddSlider(wxT(""), 0, 100, 0);

      mToneHighText = s.Id(ID_ToneHigh_TEXT).AddSpinCtrl(_("Tone High (%):"), 0, 100, 0);
      s.SetStyle(wxSL_HORIZONTAL);
      mToneHighWidget = s.Id(ID_ToneHigh_WIDGET).AddSlider(wxT(""), 0, 100, 0);

      mWetGainText = s.Id(ID_WetGain_TEXT).AddSpinCtrl(_("Wet Gain (dB):"), 0, 10, -20);
      s.SetStyle(wxSL_HORIZONTAL);
      mWetGainWidget = s.Id(ID_WetGain_WIDGET).AddSlider(wxT(""), 0, 10, -20);

      mDryGainText = s.Id(ID_DryGain_TEXT).AddSpinCtrl(_("Dry Gain (dB):"), 0, 10, -20);
      s.SetStyle(wxSL_HORIZONTAL);
      mDryGainWidget = s.Id(ID_DryGain_WIDGET).AddSlider(wxT(""), 0, 10, -20);

      mStereoWidthText = s.Id(ID_StereoWidth_TEXT).AddSpinCtrl(_("Stereo Width (%):"), 0, 100, 0);
      s.SetStyle(wxSL_HORIZONTAL);
      mStereoWidthWidget = s.Id(ID_StereoWidth_WIDGET).AddSlider(wxT(""), 0, 100, 0);
   } 
   s.EndMultiColumn();

   s.StartHorizontalLay(wxCENTER, false); 
   {
      mWetOnlyWidget = s.Id(ID_WetOnly_WIDGET).AddCheckBox(_("Wet Only"), wxT("false"));
   } 
   s.EndHorizontalLay();

   s.StartHorizontalLay(wxCENTER); {
      s.StartStatic(_("Presets:")); {
      s.Id(ID_PRESETS), s.AddButton(_("Load"));
      } s.EndStatic();
      s.StartStatic(_("User settings:")); {
         s.StartHorizontalLay(wxCENTER); {
            s.Id(ID_LOAD_SETTINGS), s.AddButton(_("Load"));
            s.Id(ID_SAVE_SETTINGS), s.AddButton(_("Save"));
            s.Id(ID_RENAME_SETTINGS), s.AddButton(_("Rename"));
         } s.EndHorizontalLay();
      } s.EndStatic();
   } s.EndHorizontalLay();
   return;
}

bool ReverbDialogue::TransferDataToWindow()
{
   mRoomSizeWidget->SetValue(int(mParams.mRoomSize));
   mRoomSizeText->SetValue(wxString::Format(wxT("%d"), int(mParams.mRoomSize)));

   mDelayWidget->SetValue(int(mParams.mDelay));
   mDelayText->SetValue(wxString::Format(wxT("%d"), int(mParams.mDelay)));

   mReverberanceWidget->SetValue(int(mParams.mReverberance));
   mReverberanceText->SetValue(wxString::Format(wxT("%d"), int(mParams.mReverberance)));

   mHfDampingWidget->SetValue(int(mParams.mHfDamping));
   mHfDampingText->SetValue(wxString::Format(wxT("%d"), int(mParams.mHfDamping)));

   mToneLowWidget->SetValue(int(mParams.mToneLow));
   mToneLowText->SetValue(wxString::Format(wxT("%d"), int(mParams.mToneLow)));

   mToneHighWidget->SetValue(int(mParams.mToneHigh));
   mToneHighText->SetValue(wxString::Format(wxT("%d"), int(mParams.mToneHigh)));

   mWetGainWidget->SetValue(int(mParams.mWetGain));
   mWetGainText->SetValue(wxString::Format(wxT("%d"), int(mParams.mWetGain)));

   mDryGainWidget->SetValue(int(mParams.mDryGain));
   mDryGainText->SetValue(wxString::Format(wxT("%d"), int(mParams.mDryGain)));

   mStereoWidthWidget->SetValue(int(mParams.mStereoWidth));
   mStereoWidthText->SetValue(wxString::Format(wxT("%d"), int(mParams.mStereoWidth)));

   mWetOnlyWidget->SetValue(int(mParams.mWetOnly));

   return true;
}

bool ReverbDialogue::TransferDataFromWindow()
{
   mParams.mRoomSize = mRoomSizeWidget->GetValue();
   mParams.mDelay = mDelayWidget->GetValue();
   mParams.mReverberance = mReverberanceWidget->GetValue();
   mParams.mHfDamping = mHfDampingWidget->GetValue();
   mParams.mToneLow = mToneLowWidget->GetValue();
   mParams.mToneHigh = mToneHighWidget->GetValue();
   mParams.mWetGain = mWetGainWidget->GetValue();
   mParams.mDryGain = mDryGainWidget->GetValue();
   mParams.mStereoWidth = mStereoWidthWidget->GetValue();
   mParams.mWetOnly = mWetOnlyWidget->GetValue();
   return true;
}

void ReverbDialogue::OnRoomSizeText(wxCommandEvent & WXUNUSED(event)) 
{ int val = mRoomSizeText->GetValue(); mRoomSizeWidget->SetValue(TrapLong(val, 0, 100)); }
void ReverbDialogue::OnRoomSizeWidget(wxCommandEvent & WXUNUSED(event))
{ mRoomSizeText->SetValue(wxString::Format(wxT("%d"), mRoomSizeWidget->GetValue())); }

void ReverbDialogue::OnDelayText(wxCommandEvent & WXUNUSED(event)) 
{ int val = mDelayText->GetValue(); mDelayWidget->SetValue(TrapLong(val, 0, 200)); }
void ReverbDialogue::OnDelayWidget(wxCommandEvent & WXUNUSED(event))
{ mDelayText->SetValue(wxString::Format(wxT("%d"), mDelayWidget->GetValue())); }

void ReverbDialogue::OnReverberanceText(wxCommandEvent & WXUNUSED(event)) 
{ int val = mReverberanceText->GetValue(); mReverberanceWidget->SetValue(TrapLong(val, 0, 100)); }
void ReverbDialogue::OnReverberanceWidget(wxCommandEvent & WXUNUSED(event))
{ mReverberanceText->SetValue(wxString::Format(wxT("%d"), mReverberanceWidget->GetValue())); }

void ReverbDialogue::OnHfDampingText(wxCommandEvent & WXUNUSED(event)) 
{ int val = mHfDampingText->GetValue(); mHfDampingWidget->SetValue(TrapLong(val, 0, 100)); }
void ReverbDialogue::OnHfDampingWidget(wxCommandEvent & WXUNUSED(event))
{ mHfDampingText->SetValue(wxString::Format(wxT("%d"), mHfDampingWidget->GetValue())); }

void ReverbDialogue::OnToneLowText(wxCommandEvent & WXUNUSED(event)) 
{ int val = mToneLowText->GetValue(); mToneLowWidget->SetValue(TrapLong(val, 0, 100)); }
void ReverbDialogue::OnToneLowWidget(wxCommandEvent & WXUNUSED(event))
{ mToneLowText->SetValue(wxString::Format(wxT("%d"), mToneLowWidget->GetValue())); }

void ReverbDialogue::OnToneHighText(wxCommandEvent & WXUNUSED(event)) 
{ int val = mToneHighText->GetValue(); mToneHighWidget->SetValue(TrapLong(val, 0, 100)); }
void ReverbDialogue::OnToneHighWidget(wxCommandEvent & WXUNUSED(event))
{ mToneHighText->SetValue(wxString::Format(wxT("%d"), mToneHighWidget->GetValue())); }

void ReverbDialogue::OnWetGainText(wxCommandEvent & WXUNUSED(event)) 
{ int val = mWetGainText->GetValue(); mWetGainWidget->SetValue(TrapLong(val, -20, 10)); }
void ReverbDialogue::OnWetGainWidget(wxCommandEvent & WXUNUSED(event))
{ mWetGainText->SetValue(wxString::Format(wxT("%d"), mWetGainWidget->GetValue())); }

void ReverbDialogue::OnDryGainText(wxCommandEvent & WXUNUSED(event)) 
{ int val = mDryGainText->GetValue(); mDryGainWidget->SetValue(TrapLong(val, -20, 10)); }
void ReverbDialogue::OnDryGainWidget(wxCommandEvent & WXUNUSED(event))
{ mDryGainText->SetValue(wxString::Format(wxT("%d"), mDryGainWidget->GetValue())); }

void ReverbDialogue::OnStereoWidthText(wxCommandEvent & WXUNUSED(event)) 
{ int val = mStereoWidthText->GetValue(); mStereoWidthWidget->SetValue(TrapLong(val, 0, 100)); }
void ReverbDialogue::OnStereoWidthWidget(wxCommandEvent & WXUNUSED(event))
{ mStereoWidthText->SetValue(wxString::Format(wxT("%d"), mStereoWidthWidget->GetValue())); }


static int wxGetChoiceFromUser(wxWindow * parent, wxString const & message,
      wxString const & caption, wxArrayString const & choices,
      char * * clientData = 0, long style = wxCHOICEDLG_STYLE,
      wxPoint const & pos = wxDefaultPosition) // Home-grown function
{
   wxSingleChoiceDialog d(parent, message, caption, choices, clientData, style, pos);
   d.ShowModal();
   return d.GetReturnCode() == wxID_CANCEL? -1 : d.GetSelection();
}

void ReverbDialogue::LoadPreset(wxCommandEvent & WXUNUSED(event))
{
   EffectReverb::Params & p = mParams;
   wxString caption(_("Reverb settings"));
   wxString message(_("Load preset:"));
   wxArrayString choices;
   choices.Add(_("Vocal I"));
   choices.Add(_("Vocal II"));
   choices.Add(_("Bathroom"));
   choices.Add(_("Small Room Bright"));
   choices.Add(_("Small Room Dark"));
   choices.Add(_("Medium Room"));
   choices.Add(_("Large Room"));
   choices.Add(_("Church Hall"));
   choices.Add(_("Cathedral"));
   int i(wxGetChoiceFromUser(this, message, caption, choices));
   switch (i) {
      case 0: p.mRoomSize=70; p.mHfDamping=99; p.mDelay=20; p.mReverberance=40; p.mToneLow=100; p.mToneHigh=50 ; p.mWetGain=-12; p.mDryGain=0  ; p.mStereoWidth=70 ; break; 
      case 1: p.mRoomSize=50; p.mHfDamping=99; p.mDelay=0 ; p.mReverberance=50; p.mToneLow=50 ; p.mToneHigh=100; p.mWetGain=-1 ; p.mDryGain=-1 ; p.mStereoWidth=70 ; break; 
      case 2: p.mRoomSize=16; p.mHfDamping=0 ; p.mDelay=8 ; p.mReverberance=80; p.mToneLow=0  ; p.mToneHigh=100; p.mWetGain=-6 ; p.mDryGain=0  ; p.mStereoWidth=100; break; 
      case 3: p.mRoomSize=30; p.mHfDamping=50; p.mDelay=10; p.mReverberance=50; p.mToneLow=50 ; p.mToneHigh=100; p.mWetGain=-1 ; p.mDryGain=-1 ; p.mStereoWidth=100; break; 
      case 4: p.mRoomSize=30; p.mHfDamping=50; p.mDelay=10; p.mReverberance=50; p.mToneLow=100; p.mToneHigh=0  ; p.mWetGain=-1 ; p.mDryGain=-1 ; p.mStereoWidth=100; break; 
      case 5: p.mRoomSize=75; p.mHfDamping=50; p.mDelay=10; p.mReverberance=40; p.mToneLow=100; p.mToneHigh=70 ; p.mWetGain=-1 ; p.mDryGain=-1 ; p.mStereoWidth=70 ; break; 
      case 6: p.mRoomSize=85; p.mHfDamping=50; p.mDelay=10; p.mReverberance=40; p.mToneLow=100; p.mToneHigh=80 ; p.mWetGain=0  ; p.mDryGain=-6 ; p.mStereoWidth=90 ; break; 
      case 7: p.mRoomSize=90; p.mHfDamping=50; p.mDelay=32; p.mReverberance=60; p.mToneLow=100; p.mToneHigh=50 ; p.mWetGain=0  ; p.mDryGain=-12; p.mStereoWidth=100; break; 
      case 8: p.mRoomSize=90; p.mHfDamping=50; p.mDelay=16; p.mReverberance=90; p.mToneLow=100; p.mToneHigh=0  ; p.mWetGain=0  ; p.mDryGain=-20; p.mStereoWidth=100; break;
      default: return;
   }
   p.mWetOnly=0;
   TransferDataToWindow();
   SetTitle(choices[i]);
}

int ReverbDialogue::ChooseSettings(wxString const & message)
{
   wxString caption(_("Reverb settings"));
   wxArrayString choices;
   for (int i = 0; i < 10; choices.Add(mEffect.SettingsName(i++)));
   return wxGetChoiceFromUser(this, message, caption, choices);
}

void ReverbDialogue::SaveSettings(wxCommandEvent & WXUNUSED(event))
{
   int i(ChooseSettings(_("Save current settings as:")));
   if (i >= 0) {
      EffectReverb::Params savedParams(mParams);
      TransferDataFromWindow();
      mEffect.SaveSettings(i, &mParams);
      mParams = savedParams;
      SetTitle(mEffect.SettingsName(i));
   }
}

void ReverbDialogue::SetTitle(wxString const & name)  
{
   wxString title(_("Reverb"));
   if (name != wxT(""))
      title += wxT(": ") + name;
   wxTopLevelWindow::SetTitle(title);
}

void ReverbDialogue::LoadSettings(wxCommandEvent & WXUNUSED(event))
{
   int i(ChooseSettings(_("Load settings:")));
   if (i >= 0) {
      mEffect.LoadSettings(i, mParams);
      TransferDataToWindow();
      SetTitle(mEffect.SettingsName(i));
   }
}

void ReverbDialogue::RenameSettings(wxCommandEvent & WXUNUSED(event))
{
   int i(ChooseSettings(_("Rename settings:")));
   if (i >= 0) {
      wxString oldName = mEffect.SettingsName(i);
      wxString newName = wxGetTextFromUser(_("Change name to:"), oldName, oldName, this);
      if (newName != wxT(""))
         mEffect.SaveSettings(i, 0, &newName);
   }
}

void ReverbDialogue::OnPreview(wxCommandEvent & event)
{
   if (event.GetId() == ePreviewID) {
      EffectReverb::Params savedParams(mParams);
      TransferDataFromWindow();
      mEffect.Preview();
      mParams = savedParams;
   }
   else mEffect.Preview(true);
}

