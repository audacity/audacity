/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.cpp

  Vaughan Johnson, Dominic Mazzoni

*******************************************************************//**

\class EffectChangeSpeed
\brief An Effect that affects both pitch & speed.

*//*******************************************************************/

#include "../Audacity.h"
#include "ChangeSpeed.h"

#include <math.h>

#include <wx/intl.h>

#include "../LabelTrack.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Resample.h"
#include "../ShuttleGui.h"
#include "../widgets/valnum.h"

#include "TimeWarper.h"
#include "../WaveTrack.h"

enum
{
   ID_PercentChange = 10000,
   ID_Multiplier,
   ID_FromVinyl,
   ID_ToVinyl,
   ID_ToLength
};

// the standard vinyl rpm choices
// If the percent change is not one of these ratios, the choice control gets "n/a".
enum kVinyl
{
   kVinyl_33AndAThird = 0,
   kVinyl_45,
   kVinyl_78,
   kVinyl_NA,
   kNumVinyl
};

static const wxChar *kVinylStrings[kNumVinyl] =
{
   wxT("33 1/3"),
   wxT("45"),
   wxT("78"),
   /* i18n-hint: n/a is an English abbreviation meaning "not applicable". */
   XO("n/a"),
};

// Soundtouch is not reasonable below -99% or above 3000%.

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name          Type     Key               Def   Min      Max      Scale
Param( Percentage,   double,  XO("Percentage"), 0.0,  -99.0,   4900.0,  1  );

// We warp the slider to go up to 400%, but user can enter higher values
static const double kSliderMax = 100.0;         // warped above zero to actually go up to 400%
static const double kSliderWarp = 1.30105;      // warp power takes max from 100 to 400.

//
// EffectChangeSpeed
//

BEGIN_EVENT_TABLE(EffectChangeSpeed, wxEvtHandler)
    EVT_TEXT(ID_PercentChange, EffectChangeSpeed::OnText_PercentChange)
    EVT_TEXT(ID_Multiplier, EffectChangeSpeed::OnText_Multiplier)
    EVT_SLIDER(ID_PercentChange, EffectChangeSpeed::OnSlider_PercentChange)
    EVT_CHOICE(ID_FromVinyl, EffectChangeSpeed::OnChoice_Vinyl)
    EVT_CHOICE(ID_ToVinyl, EffectChangeSpeed::OnChoice_Vinyl)
    EVT_TEXT(ID_ToLength, EffectChangeSpeed::OnTimeCtrl_ToLength)
    EVT_COMMAND(ID_ToLength, EVT_TIMETEXTCTRL_UPDATED, EffectChangeSpeed::OnTimeCtrlUpdate)
END_EVENT_TABLE()

EffectChangeSpeed::EffectChangeSpeed()
{
   // effect parameters
   m_PercentChange = DEF_Percentage;

   mFromVinyl = kVinyl_33AndAThird;
   mToVinyl = kVinyl_33AndAThird;
   mFromLength = 0.0;
   mToLength = 0.0;
   mFormat = _("hh:mm:ss + milliseconds");
   mbLoopDetect = false;

   SetLinearEffectFlag(true);
}

EffectChangeSpeed::~EffectChangeSpeed()
{
}

// IdentInterface implementation

wxString EffectChangeSpeed::GetSymbol()
{
   return CHANGESPEED_PLUGIN_SYMBOL;
}

wxString EffectChangeSpeed::GetDescription()
{
   return XO("Change the speed of a track, also changing its pitch");
}

// EffectIdentInterface implementation

EffectType EffectChangeSpeed::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

bool EffectChangeSpeed::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Percentage, m_PercentChange);

   return true;
}

bool EffectChangeSpeed::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyDouble(Percentage);

   m_PercentChange = Percentage;

   return true;
}

bool EffectChangeSpeed::LoadFactoryDefaults()
{
   mFromVinyl = kVinyl_33AndAThird;
   mFormat = _("hh:mm:ss + milliseconds");

   return Effect::LoadFactoryDefaults();
}

// Effect implementation

bool EffectChangeSpeed::CheckWhetherSkipEffect()
{
   return (m_PercentChange == 0.0);
}

double EffectChangeSpeed::CalcPreviewInputLength(double previewLength)
{
   return previewLength * (100.0 + m_PercentChange) / 100.0;
}

bool EffectChangeSpeed::Startup()
{
   wxString base = wxT("/Effects/ChangeSpeed/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      // Retrieve last used control values
      gPrefs->Read(base + wxT("PercentChange"), &m_PercentChange, 0);

      // default format "4" is the same as the Selection toolbar: "hh:mm:ss + milliseconds";
      gPrefs->Read(base + wxT("TimeFormat"), &mFormat, _("hh:mm:ss + milliseconds"));

      gPrefs->Read(base + wxT("VinylChoice"), &mFromVinyl, 0);
      if (mFromVinyl == kVinyl_NA)
      {
         mFromVinyl = kVinyl_33AndAThird;
      }

      SetPrivateConfig(GetCurrentSettingsGroup(), wxT("TimeFormat"), mFormat);
      SetPrivateConfig(GetCurrentSettingsGroup(), wxT("VinylChoice"), mFromVinyl);

      SaveUserPreset(GetCurrentSettingsGroup());

      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
      gPrefs->Flush();
   }

   return true;
}

bool EffectChangeSpeed::Init()
{
   // The selection might have changed since the last time EffectChangeSpeed
   // was invoked, so recalculate the Length parameters.
   mFromLength = mT1 - mT0;
   return true;
}

bool EffectChangeSpeed::Process()
{
   // Similar to EffectSoundTouch::Process()

   // Iterate over each track.
   // Track::All is needed because this effect needs to introduce
   // silence in the sync-lock group tracks to keep sync
   CopyInputTracks(Track::All); // Set up mOutputTracks.
   bool bGoodResult = true;

   TrackListIterator iter(mOutputTracks.get());
   Track* t;
   mCurTrackNum = 0;
   mMaxNewLength = 0.0;

   mFactor = 100.0 / (100.0 + m_PercentChange);

   t = iter.First();
   while (t != NULL)
   {
      if (t->GetKind() == Track::Label) {
         if (t->GetSelected() || t->IsSyncLockSelected())
         {
            if (!ProcessLabelTrack(static_cast<LabelTrack*>(t))) {
               bGoodResult = false;
               break;
            }
         }
      }
      else if (t->GetKind() == Track::Wave && t->GetSelected())
      {
         WaveTrack *pOutWaveTrack = (WaveTrack*)t;
         //Get start and end times from track
         mCurT0 = pOutWaveTrack->GetStartTime();
         mCurT1 = pOutWaveTrack->GetEndTime();

         //Set the current bounds to whichever left marker is
         //greater and whichever right marker is less:
         mCurT0 = wxMax(mT0, mCurT0);
         mCurT1 = wxMin(mT1, mCurT1);

         // Process only if the right marker is to the right of the left marker
         if (mCurT1 > mCurT0) {
            //Transform the marker timepoints to samples
            auto start = pOutWaveTrack->TimeToLongSamples(mCurT0);
            auto end = pOutWaveTrack->TimeToLongSamples(mCurT1);

            //ProcessOne() (implemented below) processes a single track
            if (!ProcessOne(pOutWaveTrack, start, end))
            {
               bGoodResult = false;
               break;
            }
         }
         mCurTrackNum++;
      }
      else if (t->IsSyncLockSelected())
      {
         t->SyncLockAdjust(mT1, mT0 + (mT1 - mT0) * mFactor);
      }

      //Iterate to the next track
      t=iter.Next();
   }

   if (bGoodResult)
      ReplaceProcessedTracks(bGoodResult);

   // Update selection.
   mT1 = mT0 + (((mT1 - mT0) * 100.0) / (100.0 + m_PercentChange));

   return bGoodResult;
}

void EffectChangeSpeed::PopulateOrExchange(ShuttleGui & S)
{
   GetPrivateConfig(GetCurrentSettingsGroup(), wxT("TimeFormat"), mFormat, mFormat);
   GetPrivateConfig(GetCurrentSettingsGroup(), wxT("VinylChoice"), mFromVinyl, mFromVinyl);

   S.SetBorder(5);

   S.StartVerticalLay(0);
   {
      S.AddSpace(0, 5);
      S.AddTitle(_("Change Speed, affecting both Tempo and Pitch"));
      S.AddSpace(0, 10);

      // Speed multiplier and percent change controls.
      S.StartMultiColumn(4, wxCENTER);
      {
         FloatingPointValidator<double> vldMultiplier(3, &mMultiplier, NUM_VAL_THREE_TRAILING_ZEROES);
         vldMultiplier.SetRange(MIN_Percentage / 100.0, MAX_Percentage / 100.0);
         mpTextCtrl_Multiplier =
            S.Id(ID_Multiplier).AddTextBox(_("Speed Multiplier:"), wxT(""), 12);
         mpTextCtrl_Multiplier->SetValidator(vldMultiplier);

         FloatingPointValidator<double> vldPercentage(3, &m_PercentChange, NUM_VAL_THREE_TRAILING_ZEROES);
         vldPercentage.SetRange(MIN_Percentage, MAX_Percentage);
         mpTextCtrl_PercentChange =
            S.Id(ID_PercentChange).AddTextBox(_("Percent Change:"), wxT(""), 12);
         mpTextCtrl_PercentChange->SetValidator(vldPercentage);
      }
      S.EndMultiColumn();

      // Percent change slider.
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetStyle(wxSL_HORIZONTAL);
         mpSlider_PercentChange =
            S.Id(ID_PercentChange).AddSlider(wxT(""), 0, (int)kSliderMax, (int)MIN_Percentage);
         mpSlider_PercentChange->SetName(_("Percent Change"));
      }
      S.EndHorizontalLay();

      // Vinyl rpm controls.
      S.StartMultiColumn(5, wxCENTER);
      {
         /* i18n-hint: "rpm" is an English abbreviation meaning "revolutions per minute". */
         S.AddUnits(_("Standard Vinyl rpm:"));

         wxASSERT(kNumVinyl == WXSIZEOF(kVinylStrings));

         wxArrayString vinylChoices;
         for (int i = 0; i < kNumVinyl; i++)
         {
            if (i == kVinyl_NA)
            {
               vinylChoices.Add(wxGetTranslation(kVinylStrings[i]));
            }
            else
            {
               vinylChoices.Add(kVinylStrings[i]);
            }
         }

         mpChoice_FromVinyl =
            S.Id(ID_FromVinyl).AddChoice(_("from"), wxT(""), &vinylChoices);
         mpChoice_FromVinyl->SetName(_("From rpm"));
         mpChoice_FromVinyl->SetSizeHints(100, -1);

         mpChoice_ToVinyl =
            S.Id(ID_ToVinyl).AddChoice(_("to"), wxT(""), &vinylChoices);
         mpChoice_ToVinyl->SetName(_("To rpm"));
         mpChoice_ToVinyl->SetSizeHints(100, -1);
      }
      S.EndMultiColumn();

      // From/To time controls.
      S.StartStatic(_("Selection Length"), 0);
      {
         S.StartMultiColumn(2, wxALIGN_LEFT);
         {
            S.AddPrompt(_("Current Length:"));

            mpFromLengthCtrl = safenew
                  NumericTextCtrl(NumericConverter::TIME,
                                 S.GetParent(),
                                 wxID_ANY,
                                 mFormat,
                                 mFromLength,
                                 mProjectRate);

            mpFromLengthCtrl->SetName(_("from"));
            mpFromLengthCtrl->SetToolTip(_("Current length of selection."));
            mpFromLengthCtrl->SetReadOnly(true);
            mpFromLengthCtrl->EnableMenu(false);
            S.AddWindow(mpFromLengthCtrl, wxALIGN_LEFT);

            S.AddPrompt(_("New Length:"));

            mpToLengthCtrl = safenew
                  NumericTextCtrl(NumericConverter::TIME,
                                 S.GetParent(),
                                 ID_ToLength,
                                 mFormat,
                                 mToLength,
                                 mProjectRate);

            mpToLengthCtrl->SetName(_("to"));
            mpToLengthCtrl->EnableMenu();
            S.AddWindow(mpToLengthCtrl, wxALIGN_LEFT);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndVerticalLay();
}

bool EffectChangeSpeed::TransferDataToWindow()
{
   mbLoopDetect = true;

   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   if (mFromVinyl == kVinyl_NA)
   {
      mFromVinyl = kVinyl_33AndAThird;
   }

   Update_Text_PercentChange();
   Update_Text_Multiplier();
   Update_Slider_PercentChange();
   Update_TimeCtrl_ToLength();

   // Set from/to Vinyl controls - mFromVinyl must be set first.
   mpChoice_FromVinyl->SetSelection(mFromVinyl);
   // Then update to get correct mToVinyl.
   Update_Vinyl();
   // Then update ToVinyl control.
   mpChoice_ToVinyl->SetSelection(mToVinyl);

   // Set From Length control.
   // Set the format first so we can get sample accuracy.
   mpFromLengthCtrl->SetFormatName(mFormat);
   mpFromLengthCtrl->SetValue(mFromLength);

   mbLoopDetect = false;

   return true;
}

bool EffectChangeSpeed::TransferDataFromWindow()
{
   // mUIParent->TransferDataFromWindow() loses some precision, so save and restore it.
   double exactPercent = m_PercentChange;
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }
   m_PercentChange = exactPercent;

   SetPrivateConfig(GetCurrentSettingsGroup(), wxT("TimeFormat"), mFormat);
   SetPrivateConfig(GetCurrentSettingsGroup(), wxT("VinylChoice"), mFromVinyl);

   return true;
}

// EffectChangeSpeed implementation

// Labels are time-scaled linearly inside the affected region, and labels after
// the region are shifted along according to how the region size changed.
bool EffectChangeSpeed::ProcessLabelTrack(LabelTrack *lt)
{
   SetTimeWarper(std::make_unique<RegionTimeWarper>(mT0, mT1,
                     std::make_unique<LinearTimeWarper>(mT0, mT0,
                         mT1, mT0 + (mT1-mT0)*mFactor)));
   lt->WarpLabels(*GetTimeWarper());
   return true;
}

// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
// and calls libsamplerate code on these blocks.
bool EffectChangeSpeed::ProcessOne(WaveTrack * track,
                           sampleCount start, sampleCount end)
{
   if (track == NULL)
      return false;

   // initialization, per examples of Mixer::Mixer and
   // EffectSoundTouch::ProcessOne

   auto outputTrack = mFactory->NewWaveTrack(track->GetSampleFormat(),
                                                    track->GetRate());

   //Get the length of the selection (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();

   // Initiate processing buffers, most likely shorter than
   // the length of the selection being processed.
   auto inBufferSize = track->GetMaxBlockSize();

   float * inBuffer = new float[inBufferSize];

   // mFactor is at most 100-fold so this shouldn't overflow size_t
   auto outBufferSize = size_t( mFactor * inBufferSize + 10 );
   float * outBuffer = new float[outBufferSize];

   // Set up the resampling stuff for this track.
   Resample resample(true, mFactor, mFactor); // constant rate resampling

   //Go through the track one buffer at a time. samplePos counts which
   //sample the current buffer starts at.
   bool bResult = true;
   auto samplePos = start;
   while (samplePos < end) {
      //Get a blockSize of samples (smaller than the size of the buffer)
      auto blockSize = limitSampleBufferSize(
         track->GetBestBlockSize(samplePos),
         end - samplePos
      );

      //Get the samples from the track and put them in the buffer
      track->Get((samplePtr) inBuffer, floatSample, samplePos, blockSize);

      const auto results = resample.Process(mFactor,
                                    inBuffer,
                                    blockSize,
                                    ((samplePos + blockSize) >= end),
                                    outBuffer,
                                    outBufferSize);
      const auto outgen = results.second;

      if (outgen > 0)
         outputTrack->Append((samplePtr)outBuffer, floatSample,
                             outgen);

      // Increment samplePos
      samplePos += results.first;

      // Update the Progress meter
      if (TrackProgress(mCurTrackNum, (samplePos - start).as_double() / len)) {
         bResult = false;
         break;
      }
   }

   // Flush the output WaveTrack (since it's buffered, too)
   outputTrack->Flush();

   // Clean up the buffers
   delete [] inBuffer;
   delete [] outBuffer;

   // Take the output track and insert it in place of the original
   // sample data
   double newLength = outputTrack->GetEndTime();
   if (bResult)
   {
      SetTimeWarper(std::make_unique<LinearTimeWarper>(mCurT0, mCurT0, mCurT1, mCurT0 + newLength));
      bResult = track->ClearAndPaste(mCurT0, mCurT1, outputTrack.get(), true, false, GetTimeWarper());
   }

   if (newLength > mMaxNewLength)
      mMaxNewLength = newLength;

   return bResult;
}

// handler implementations for EffectChangeSpeed

void EffectChangeSpeed::OnText_PercentChange(wxCommandEvent & WXUNUSED(evt))
{
   if (mbLoopDetect)
      return;

   mpTextCtrl_PercentChange->GetValidator()->TransferFromWindow();
   UpdateUI();

   mbLoopDetect = true;
   Update_Text_Multiplier();
   Update_Slider_PercentChange();
   Update_Vinyl();
   Update_TimeCtrl_ToLength();
   mbLoopDetect = false;
}

void EffectChangeSpeed::OnText_Multiplier(wxCommandEvent & WXUNUSED(evt))
{
   if (mbLoopDetect)
      return;

   mpTextCtrl_Multiplier->GetValidator()->TransferFromWindow();
   m_PercentChange = 100 * (mMultiplier - 1);
   UpdateUI();

   mbLoopDetect = true;
   Update_Text_PercentChange();
   Update_Slider_PercentChange();
   Update_Vinyl();
   Update_TimeCtrl_ToLength();
   mbLoopDetect = false;
}

void EffectChangeSpeed::OnSlider_PercentChange(wxCommandEvent & WXUNUSED(evt))
{
   if (mbLoopDetect)
      return;

   m_PercentChange = (double)(mpSlider_PercentChange->GetValue());
   // Warp positive values to actually go up faster & further than negatives.
   if (m_PercentChange > 0.0)
      m_PercentChange = pow(m_PercentChange, kSliderWarp);
   UpdateUI();

   mbLoopDetect = true;
   Update_Text_PercentChange();
   Update_Text_Multiplier();
   Update_Vinyl();
   Update_TimeCtrl_ToLength();
   mbLoopDetect = false;
}

void EffectChangeSpeed::OnChoice_Vinyl(wxCommandEvent & WXUNUSED(evt))
{
   // Treat mpChoice_FromVinyl and mpChoice_ToVinyl as one control since we need
   // both to calculate Percent Change.
   mFromVinyl = mpChoice_FromVinyl->GetSelection();
   mToVinyl = mpChoice_ToVinyl->GetSelection();
   // Use this as the 'preferred' choice.
   if (mFromVinyl != kVinyl_NA) {
      SetPrivateConfig(GetCurrentSettingsGroup(), wxT("VinylChoice"), mFromVinyl);
   }

   // If mFromVinyl & mToVinyl are set, then there's a NEW percent change.
   if ((mFromVinyl != kVinyl_NA) && (mToVinyl != kVinyl_NA))
   {
      double fromRPM;
      double toRPM;
      switch (mFromVinyl) {
      default:
      case kVinyl_33AndAThird:   fromRPM = 33.0 + (1.0 / 3.0); break;
      case kVinyl_45:            fromRPM = 45.0; break;
      case kVinyl_78:            fromRPM = 78; break;
      }
      switch (mToVinyl) {
      default:
      case kVinyl_33AndAThird:   toRPM = 33.0 + (1.0 / 3.0); break;
      case kVinyl_45:            toRPM = 45.0; break;
      case kVinyl_78:            toRPM = 78; break;
      }
      m_PercentChange = ((toRPM * 100.0) / fromRPM) - 100.0;
      UpdateUI();

      mbLoopDetect = true;
      Update_Text_PercentChange();
      Update_Text_Multiplier();
      Update_Slider_PercentChange();
      Update_TimeCtrl_ToLength();
   }
   mbLoopDetect = false;
}

void EffectChangeSpeed::OnTimeCtrl_ToLength(wxCommandEvent & WXUNUSED(evt))
{
   if (mbLoopDetect)
      return;

   mToLength = mpToLengthCtrl->GetValue();
   m_PercentChange = ((mFromLength * 100.0) / mToLength) - 100.0;
   UpdateUI();

   mbLoopDetect = true;

   Update_Text_PercentChange();
   Update_Text_Multiplier();
   Update_Slider_PercentChange();
   Update_Vinyl();

   mbLoopDetect = false;
}

void EffectChangeSpeed::OnTimeCtrlUpdate(wxCommandEvent & evt)
{
   mFormat = evt.GetString();

   mpFromLengthCtrl->SetFormatName(mFormat);
   // Update From/To Length controls (precision has changed).
   mpToLengthCtrl->SetValue(mToLength);
   mpFromLengthCtrl->SetValue(mFromLength);
}

// helper functions

void EffectChangeSpeed::Update_Text_PercentChange()
// Update Text Percent control from percent change.
{
   mpTextCtrl_PercentChange->GetValidator()->TransferToWindow();
}

void EffectChangeSpeed::Update_Text_Multiplier()
// Update Multiplier control from percent change.
{
   mMultiplier =  1 + (m_PercentChange) / 100.0;
   mpTextCtrl_Multiplier->GetValidator()->TransferToWindow();
}

void EffectChangeSpeed::Update_Slider_PercentChange()
// Update Slider Percent control from percent change.
{
   double unwarped = m_PercentChange;
   if (unwarped > 0.0)
      // Un-warp values above zero to actually go up to kSliderMax.
      unwarped = pow(m_PercentChange, (1.0 / kSliderWarp));

   // Add 0.5 to unwarped so trunc -> round.
   mpSlider_PercentChange->SetValue((int)(unwarped + 0.5));
}

void EffectChangeSpeed::Update_Vinyl()
// Update Vinyl controls from percent change.
{
   // Match Vinyl rpm when within 0.01% of a standard ratio.
   // Ratios calculated as: ((toRPM / fromRPM) - 1) * 100 * 100
   int ratio = wxRound(m_PercentChange * 100);

   switch (ratio)
   {
      case 0: // toRPM is the same as fromRPM
         if (mFromVinyl != kVinyl_NA) {
            mpChoice_ToVinyl->SetSelection(mpChoice_FromVinyl->GetSelection());
         } else {
            // Use the last saved option.
            GetPrivateConfig(GetCurrentSettingsGroup(), wxT("VinylChoice"), mFromVinyl, 0);
            mpChoice_FromVinyl->SetSelection(mFromVinyl);
            mpChoice_ToVinyl->SetSelection(mFromVinyl);
         }
         break;
      case 3500:
         mpChoice_FromVinyl->SetSelection(kVinyl_33AndAThird);
         mpChoice_ToVinyl->SetSelection(kVinyl_45);
         break;
      case 13400:
         mpChoice_FromVinyl->SetSelection(kVinyl_33AndAThird);
         mpChoice_ToVinyl->SetSelection(kVinyl_78);
         break;
      case -2593:
         mpChoice_FromVinyl->SetSelection(kVinyl_45);
         mpChoice_ToVinyl->SetSelection(kVinyl_33AndAThird);
         break;
      case 7333:
         mpChoice_FromVinyl->SetSelection(kVinyl_45);
         mpChoice_ToVinyl->SetSelection(kVinyl_78);
         break;
      case -5727:
         mpChoice_FromVinyl->SetSelection(kVinyl_78);
         mpChoice_ToVinyl->SetSelection(kVinyl_33AndAThird);
         break;
      case -4231:
         mpChoice_FromVinyl->SetSelection(kVinyl_78);
         mpChoice_ToVinyl->SetSelection(kVinyl_45);
         break;
      default:
         mpChoice_ToVinyl->SetSelection(kVinyl_NA);
   }
   // and update variables.
   mFromVinyl = mpChoice_FromVinyl->GetSelection();
   mToVinyl = mpChoice_ToVinyl->GetSelection();
}

void EffectChangeSpeed::Update_TimeCtrl_ToLength()
// Update ToLength control from percent change.
{
   mToLength = (mFromLength * 100.0) / (100.0 + m_PercentChange);

   // Set the format first so we can get sample accuracy.
   mpToLengthCtrl->SetFormatName(mFormat);
   // Negative times do not make sense.
   // 359999 = 99h:59m:59s which is a little less disturbing than overflow characters
   // though it may still look a bit strange with some formats.
   mToLength = TrapDouble(mToLength, 0.0, 359999.0);
   mpToLengthCtrl->SetValue(mToLength);
}

void EffectChangeSpeed::UpdateUI()
// Disable OK and Preview if not in sensible range.
{
   EnableApply(m_PercentChange >= MIN_Percentage && m_PercentChange <= MAX_Percentage);
}
