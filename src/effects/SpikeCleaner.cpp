/**********************************************************************

  Audacity: A Digital Audio Editor

  SpikeCleaner.cpp

  Lynn Allan

******************************************************************//**

\class EffectSpikeCleaner
\brief An EffectSimpleMono for removing spikes.  

This is a simple version, Audacity could do with
a more sophisticated approach to this.


*//****************************************************************//**

\class SpikeCleanerDialog
\brief Dialog for EffectSpikeCleaner

*//*******************************************************************/




// JKC: This effect is not yet good enough to be included in Audacity.
// To save translators from translating it the transaltable strings
// are marked by _TRANSLATABLE( which is not recognised by gettext and so
// won't appear in the .pot file.
// gettext isn't smart enough to ignore commented out code.
#ifdef NOT_READY_FOR_PRIME_TIME

#include "../Audacity.h"

#include <wx/wx.h>
#include <math.h>

#include "../Prefs.h"
#include "SpikeCleaner.h"

EffectSpikeCleaner::EffectSpikeCleaner()
{
   Init();
}

static int gValidateSpikeFramesRemoved;  // odd ... crashes on exit if member variable
bool EffectSpikeCleaner::Init()
{
   mSpikeMaxDurationMs = gPrefs->Read(wxT("/CsPresets/SpikeMaxDurationMs"), SKIP_EFFECT_MILLISECOND);
   mSpikeDbChoiceIndex = gPrefs->Read(wxT("/CsPresets/SpikeDbChoiceIndex"), (Enums::NumDbChoices - 1));
   if ((mSpikeDbChoiceIndex < 0) || (mSpikeDbChoiceIndex >= Enums::NumDbChoices)) {  // corrupted Prefs?
      mSpikeDbChoiceIndex = Enums::NumDbChoices - 1;  // Off-Skip
      gPrefs->Write(wxT("/CsPresets/SpikeDbChoiceIndex"), mSpikeDbChoiceIndex);
      mSpikeMaxDurationMs = SKIP_EFFECT_MILLISECOND;
      gPrefs->Write(wxT("/CsPresets/SpikeMaxDurationMs"), (int)mSpikeMaxDurationMs);
   }
   gValidateSpikeFramesRemoved = 0;

   return true;
}

bool EffectSpikeCleaner::CheckWhetherSkipEffect()
{
   bool rc = ((mSpikeDbChoiceIndex >= (Enums::NumDbChoices - 1))
   ||  (mSpikeMaxDurationMs >= SKIP_EFFECT_MILLISECOND));
   return rc;
}

void EffectSpikeCleaner::End()
{
   gPrefs->Write(wxT("/Validate/SpikeFramesRemoved"), gValidateSpikeFramesRemoved);
}

bool EffectSpikeCleaner::PromptUser()
{
   SpikeCleanerDialog dlog(mParent, -1, _TRANSLATABLE("Spike Cleaner"));
   dlog.mSpikeMaxDurationMs = mSpikeMaxDurationMs;
   dlog.mSpikeDbChoiceIndex = mSpikeDbChoiceIndex;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode()) {
      return false;
   }
   mSpikeMaxDurationMs = dlog.mSpikeMaxDurationMs;
   gPrefs->Write(wxT("/CsPresets/SpikeMaxDurationMs"), mSpikeMaxDurationMs);

   mSpikeDbChoiceIndex = dlog.mSpikeDbChoiceIndex;
   gPrefs->Write(wxT("/CsPresets/SpikeDbChoiceIndex"), mSpikeDbChoiceIndex);

   return true;
}

bool EffectSpikeCleaner::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferEnum(wxT("Db"),mSpikeDbChoiceIndex,Enums::NumDbChoices,Enums::GetDbChoices());
   shuttle.TransferInt(wxT("Duration"),mSpikeMaxDurationMs,10);
   return true;
}

bool EffectSpikeCleaner::ProcessSimpleMono(float *buffer, sampleCount len)
{
   float  curFrame;
   float  fabsCurFrame;
   int    state = 1;  // 1=WithinSilence  2=WithinNoise
   int    innerSilentFrameCount = 0;
   int    firstNoiseFrame = 0;
   int    noiseLen = 0;

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   double    curRate = track->GetRate();
   int       spikeMaxDurationSamples = int((curRate / 1000.0) * double(mSpikeMaxDurationMs));
   int       maxInnerSilentFrameCount = spikeMaxDurationSamples / 10;
   double    spikeDbSilenceThreshold = Enums::Db2Signal[mSpikeDbChoiceIndex];

   for (int i = 0; i < len; ++i) {
      curFrame = buffer[i];
      fabsCurFrame = (float)fabs(curFrame);

      if (fabsCurFrame < spikeDbSilenceThreshold) {
         if (state == 2) {
            innerSilentFrameCount++;
            if (innerSilentFrameCount > maxInnerSilentFrameCount) {
               state = 1;
               noiseLen = i - firstNoiseFrame;
               if (noiseLen < spikeMaxDurationSamples) {
                 for (int j = firstNoiseFrame; j <= i; ++j) {
                    gValidateSpikeFramesRemoved++;
                    buffer[j] = 0.0;
                 }
               }
            }
         }
      }
      else {
        innerSilentFrameCount = 0;
        if (state == 1) {
           state = 2;
           firstNoiseFrame = i;
        }
      }
   }
   return true;
}

//----------------------------------------------------------------------------
// SpikeCleanerDialog
//----------------------------------------------------------------------------

#define ID_SPIKE_CLEANER_TEXT 7000
#define ID_DB_SILENCE_THRESHOLD_CHOICE 7001

BEGIN_EVENT_TABLE(SpikeCleanerDialog,wxDialog)
   EVT_BUTTON( wxID_OK, SpikeCleanerDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, SpikeCleanerDialog::OnCancel )
END_EVENT_TABLE()

SpikeCleanerDialog::SpikeCleanerDialog(wxWindow *parent, wxWindowID id,
                                   const wxString &title ) :
   wxDialog( parent, id, title)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxStaticText *statText = new wxStaticText(this, -1,
                           _TRANSLATABLE("SpikeCleaner by Lynn Allan"));
   mainSizer->Add(statText, 0, wxALIGN_CENTRE | wxALL, 5);

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   statText = new wxStaticText(this, -1, _TRANSLATABLE("Max Spike Duration (milliseconds):"));
   hSizer->Add(statText, 0, wxALIGN_CENTRE | wxALL, 5);

   wxString spikeMaxDurationMsStr;
   spikeMaxDurationMsStr.Printf(wxT("%d"), mSpikeMaxDurationMs);
   mSpikeMaxDurationMsText = new wxTextCtrl(this, ID_SPIKE_CLEANER_TEXT,
                     spikeMaxDurationMsStr, wxDefaultPosition,
                     wxSize(60, -1), 0,
                     wxTextValidator(wxFILTER_NUMERIC));
   hSizer->Add(mSpikeMaxDurationMsText, 0, wxALL, 5);
   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);
   hSizer = new wxBoxSizer(wxHORIZONTAL);

   statText = new wxStaticText(this, -1, _TRANSLATABLE("Theshold for silence: "));
   hSizer->Add(statText, 0, wxALIGN_CENTRE | wxALL, 5);

   mSpikeDbSilenceThresholdChoice = new wxChoice(this, ID_DB_SILENCE_THRESHOLD_CHOICE,
      wxDefaultPosition, wxSize(64, -1), Enums::NumDbChoices,
      Enums::GetDbChoices());
   hSizer->Add(mSpikeDbSilenceThresholdChoice, 0, wxALIGN_CENTER | wxALL, 4);
   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);
   hSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton *cancel = new wxButton(this, wxID_CANCEL, _TRANSLATABLE("&Cancel"));
   hSizer->Add(cancel, 0, wxALIGN_CENTRE|wxALL, 5);

   wxButton *ok = new wxButton(this, wxID_OK, _TRANSLATABLE("&OK"));
   ok->SetDefault();
   hSizer->Add(ok, 0, wxALIGN_CENTRE|wxALL, 5);

   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

bool SpikeCleanerDialog::TransferDataToWindow()
{
   mSpikeMaxDurationMsText->SetValue(wxString::Format(wxT("%d"), mSpikeMaxDurationMs));
   mSpikeDbSilenceThresholdChoice->SetSelection(mSpikeDbChoiceIndex);

   return true;
}

bool SpikeCleanerDialog::TransferDataFromWindow()
{
   long ms;
   mSpikeMaxDurationMsText->GetValue().ToLong(&ms);
   mSpikeMaxDurationMs = ms;

   mSpikeDbChoiceIndex = mSpikeDbSilenceThresholdChoice->GetSelection();

   return true;
}

void SpikeCleanerDialog::OnOk(wxCommandEvent &event)
{
   TransferDataFromWindow();

   EndModal(true);
}

void SpikeCleanerDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(false);
}

#endif