/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

  ChangePitch.cpp
  Vaughan Johnson, Dominic Mazzoni, Steve Daulton

******************************************************************//**

\file ChangePitch.cpp
\brief Change Pitch effect provides raising or lowering
the pitch without changing the tempo.

*//*******************************************************************/

#include "../Audacity.h" // for USE_SOUNDTOUCH

#if USE_SOUNDTOUCH
#include "ChangePitch.h"

#if USE_SBSMS
#include "sbsms.h"
#include <wx/valgen.h>
#endif

#include <float.h>
#include <math.h>

#include <wx/intl.h>
#include <wx/valtext.h>

#include "../PitchName.h"
#include "../ShuttleGui.h"
#include "../Spectrum.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"
#include "TimeWarper.h"

enum {
   ID_PercentChange = 10000,
   ID_FromPitch,
   ID_FromOctave,
   ID_ToPitch,
   ID_ToOctave,
   ID_SemitonesChange,
   ID_FromFrequency,
   ID_ToFrequency
};

// Soundtouch is not reasonable below -99% or above 3000%.

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name          Type     Key               Def   Min      Max      Scale
Param( Percentage,   double,  XO("Percentage"), 0.0,  -99.0,   3000.0,  1  );
Param( UseSBSMS,     bool,    XO("SBSMS"),     false, false,   true,    1  );

// We warp the slider to go up to 400%, but user can enter up to 3000%
static const double kSliderMax = 100.0;          // warped above zero to actually go up to 400%
static const double kSliderWarp = 1.30105;       // warp power takes max from 100 to 400.

// EffectChangePitch

BEGIN_EVENT_TABLE(EffectChangePitch, wxEvtHandler)
   EVT_CHOICE(ID_FromPitch, EffectChangePitch::OnChoice_FromPitch)
   EVT_TEXT(ID_FromOctave, EffectChangePitch::OnSpin_FromOctave)
   EVT_CHOICE(ID_ToPitch, EffectChangePitch::OnChoice_ToPitch)
   EVT_TEXT(ID_ToOctave, EffectChangePitch::OnSpin_ToOctave)

   EVT_TEXT(ID_SemitonesChange, EffectChangePitch::OnText_SemitonesChange)

   EVT_TEXT(ID_FromFrequency, EffectChangePitch::OnText_FromFrequency)
   EVT_TEXT(ID_ToFrequency, EffectChangePitch::OnText_ToFrequency)

   EVT_TEXT(ID_PercentChange, EffectChangePitch::OnText_PercentChange)
   EVT_SLIDER(ID_PercentChange, EffectChangePitch::OnSlider_PercentChange)
END_EVENT_TABLE()

EffectChangePitch::EffectChangePitch()
{
   m_dPercentChange = DEF_Percentage;
   m_dSemitonesChange = 0.0;
   m_dStartFrequency = 0.0; // 0.0 => uninitialized
   m_bLoopDetect = false;

#if USE_SBSMS
   mUseSBSMS = DEF_UseSBSMS;
#else
   mUseSBSMS = false;
#endif

   // NULL out these control members because there are some cases where the
   // event table handlers get called during this method, and those handlers that
   // can cause trouble check for NULL.
   m_pChoice_FromPitch = NULL;
   m_pSpin_FromOctave = NULL;
   m_pChoice_ToPitch = NULL;
   m_pSpin_ToOctave = NULL;

   m_pTextCtrl_SemitonesChange = NULL;

   m_pTextCtrl_FromFrequency = NULL;
   m_pTextCtrl_ToFrequency = NULL;

   m_pTextCtrl_PercentChange = NULL;
   m_pSlider_PercentChange = NULL;

   SetLinearEffectFlag(true);
}

EffectChangePitch::~EffectChangePitch()
{
}

// IdentInterface implementation

wxString EffectChangePitch::GetSymbol()
{
   return CHANGEPITCH_PLUGIN_SYMBOL;
}

wxString EffectChangePitch::GetDescription()
{
   return XO("Change the pitch of a track without changing its tempo");
}

// EffectIdentInterface implementation

EffectType EffectChangePitch::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

bool EffectChangePitch::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Percentage, m_dPercentChange);
   parms.Write(KEY_UseSBSMS, mUseSBSMS);

   return true;
}

bool EffectChangePitch::SetAutomationParameters(EffectAutomationParameters & parms)
{
   // Vaughan, 2013-06: Long lost to history, I don't see why m_dPercentChange was chosen to be shuttled.
   // Only m_dSemitonesChange is used in Process().
   ReadAndVerifyDouble(Percentage);

   m_dPercentChange = Percentage;
   Calc_SemitonesChange_fromPercentChange();

#if USE_SBSMS
   ReadAndVerifyBool(UseSBSMS);
   mUseSBSMS = UseSBSMS;
#else
   mUseSBSMS = false;
#endif

   return true;
}

bool EffectChangePitch::LoadFactoryDefaults()
{
   DeduceFrequencies();

   return Effect::LoadFactoryDefaults();
}

// Effect implementation

bool EffectChangePitch::Init()
{
   mSoundTouch.reset();
   return true;
}

bool EffectChangePitch::Process()
{
#if USE_SBSMS
   if (mUseSBSMS)
   {
      double pitchRatio = 1.0 + m_dPercentChange / 100.0;
      SelectedRegion region(mT0, mT1);
      EffectSBSMS proxy;
      proxy.mProxyEffectName = XO("High Quality Pitch Change");
      proxy.setParameters(1.0, pitchRatio);

      return proxy.DoEffect(mUIParent, mProjectRate, mTracks, mFactory, &region, false);
   }
   else
#endif
   {
      mSoundTouch = std::make_unique<SoundTouch>();
      SetTimeWarper(std::make_unique<IdentityTimeWarper>());
      mSoundTouch->setPitchSemiTones((float)(m_dSemitonesChange));
#ifdef USE_MIDI
      // Pitch shifting note tracks is currently only supported by SoundTouchEffect
      // and non-real-time-preview effects require an audio track selection.
      //
      // Note: m_dSemitonesChange is private to ChangePitch because it only
      // needs to pass it along to mSoundTouch (above). I added mSemitones
      // to SoundTouchEffect (the super class) to convey this value
      // to process Note tracks. This approach minimizes changes to existing
      // code, but it would be cleaner to change all m_dSemitonesChange to
      // mSemitones, make mSemitones exist with or without USE_MIDI, and
      // eliminate the next line:
      mSemitones = m_dSemitonesChange;
#endif
      return EffectSoundTouch::Process();
   }
}

bool EffectChangePitch::CheckWhetherSkipEffect()
{
   return (m_dPercentChange == 0.0);
}

void EffectChangePitch::PopulateOrExchange(ShuttleGui & S)
{
   DeduceFrequencies(); // Set frequency-related control values based on sample.

   wxArrayString pitch;
   pitch.Add(wxT("C"));
   pitch.Add(wxT("C#/Db"));
   pitch.Add(wxT("D"));
   pitch.Add(wxT("D#/Eb"));
   pitch.Add(wxT("E"));
   pitch.Add(wxT("F"));
   pitch.Add(wxT("F#/Gb"));
   pitch.Add(wxT("G"));
   pitch.Add(wxT("G#/Ab"));
   pitch.Add(wxT("A"));
   pitch.Add(wxT("A#/Bb"));
   pitch.Add(wxT("B"));

   S.SetBorder(5);

   S.StartVerticalLay(0);
   {
      S.StartVerticalLay();
      {
         S.AddTitle(_("Change Pitch without Changing Tempo"));
         S.AddTitle(
            wxString::Format(_("Estimated Start Pitch: %s%d (%.3f Hz)"),
                              pitch[m_nFromPitch].c_str(), m_nFromOctave, m_FromFrequency));
      }
      S.EndVerticalLay();

      /* i18n-hint: (noun) Musical pitch.*/
      S.StartStatic(_("Pitch"));
      {
         S.StartMultiColumn(6, wxALIGN_CENTER); // 6 controls, because each AddChoice adds a wxStaticText and a wxChoice.
         {
            m_pChoice_FromPitch = S.Id(ID_FromPitch).AddChoice(_("from"), wxT(""), &pitch);
            m_pChoice_FromPitch->SetName(_("from"));
            m_pChoice_FromPitch->SetSizeHints(80, -1);

            m_pSpin_FromOctave = S.Id(ID_FromOctave).AddSpinCtrl(wxT(""), m_nFromOctave, INT_MAX, INT_MIN);
            m_pSpin_FromOctave->SetName(_("from Octave"));
            m_pSpin_FromOctave->SetSizeHints(50, -1);

            m_pChoice_ToPitch = S.Id(ID_ToPitch).AddChoice(_("to"), wxT(""), &pitch);
            m_pChoice_ToPitch->SetName(_("to"));
            m_pChoice_ToPitch->SetSizeHints(80, -1);

            m_pSpin_ToOctave =
               S.Id(ID_ToOctave).AddSpinCtrl(wxT(""), m_nToOctave, INT_MAX, INT_MIN);
            m_pSpin_ToOctave->SetName(_("to Octave"));
            m_pSpin_ToOctave->SetSizeHints(50, -1);
         }
         S.EndMultiColumn();

         S.StartHorizontalLay(wxALIGN_CENTER);
         {
            FloatingPointValidator<double> vldSemitones(2, &m_dSemitonesChange, NUM_VAL_TWO_TRAILING_ZEROES);
            m_pTextCtrl_SemitonesChange =
               S.Id(ID_SemitonesChange).AddTextBox(_("Semitones (half-steps):"), wxT(""), 12);
            m_pTextCtrl_SemitonesChange->SetName(_("Semitones (half-steps)"));
            m_pTextCtrl_SemitonesChange->SetValidator(vldSemitones);
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.StartStatic(_("Frequency"));
      {
         S.StartMultiColumn(5, wxALIGN_CENTER); // 5, because AddTextBox adds a wxStaticText and a wxTextCtrl.
         {
            FloatingPointValidator<double> vldFromFrequency(3, &m_FromFrequency, NUM_VAL_THREE_TRAILING_ZEROES);
            vldFromFrequency.SetMin(0.0);
            m_pTextCtrl_FromFrequency = S.Id(ID_FromFrequency).AddTextBox(_("from"), wxT(""), 12);
            m_pTextCtrl_FromFrequency->SetName(_("from (Hz)"));
            m_pTextCtrl_FromFrequency->SetValidator(vldFromFrequency);

            FloatingPointValidator<double> vldToFrequency(3, &m_ToFrequency, NUM_VAL_THREE_TRAILING_ZEROES);
            vldToFrequency.SetMin(0.0);
            m_pTextCtrl_ToFrequency = S.Id(ID_ToFrequency).AddTextBox(_("to"), wxT(""), 12);
            m_pTextCtrl_ToFrequency->SetName(_("to (Hz)"));
            m_pTextCtrl_ToFrequency->SetValidator(vldToFrequency);

            S.AddUnits(_("Hz"));
         }
         S.EndMultiColumn();

         S.StartHorizontalLay(wxALIGN_CENTER);
         {
            FloatingPointValidator<double> vldPercentage(3, &m_dPercentChange, NUM_VAL_THREE_TRAILING_ZEROES);
            vldPercentage.SetRange(MIN_Percentage, MAX_Percentage);
            m_pTextCtrl_PercentChange = S.Id(ID_PercentChange).AddTextBox(_("Percent Change:"), wxT(""), 12);
            m_pTextCtrl_PercentChange->SetValidator(vldPercentage);
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay(wxEXPAND);
         {
            S.SetStyle(wxSL_HORIZONTAL);
            m_pSlider_PercentChange = S.Id(ID_PercentChange)
               .AddSlider(wxT(""), 0, (int)kSliderMax, (int)MIN_Percentage);
            m_pSlider_PercentChange->SetName(_("Percent Change"));
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

#if USE_SBSMS
      S.StartMultiColumn(2);
      {
         mUseSBSMSCheckBox = S.AddCheckBox(_("Use high quality stretching (slow)"),
                                             mUseSBSMS? wxT("true") : wxT("false"));
         mUseSBSMSCheckBox->SetValidator(wxGenericValidator(&mUseSBSMS));
      }
      S.EndMultiColumn();
#endif

   }
   S.EndVerticalLay();

   return;
}

bool EffectChangePitch::TransferDataToWindow()
{
   m_bLoopDetect = true;

   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   Calc_SemitonesChange_fromPercentChange();
   Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.
   Calc_ToFrequency();
   Calc_ToOctave(); // Call after Calc_ToFrequency().

   Update_Choice_FromPitch();
   Update_Choice_ToPitch();
   Update_Spin_FromOctave();
   Update_Spin_ToOctave();
   Update_Text_SemitonesChange();
   Update_Text_FromFrequency();
   Update_Text_ToFrequency();
   Update_Text_PercentChange();
   Update_Slider_PercentChange();

   m_bLoopDetect = false;

   return true;
}

bool EffectChangePitch::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   // from/to pitch controls
   m_nFromPitch = m_pChoice_FromPitch->GetSelection();
   m_nFromOctave = m_pSpin_FromOctave->GetValue();

   m_nToPitch = m_pChoice_ToPitch->GetSelection();

   // No need to update Slider_PercentChange here because TextCtrl_PercentChange
   // always tracks it & is more precise (decimal points).

   return true;
}

// EffectChangePitch implementation

// Deduce m_FromFrequency from the samples at the beginning of
// the selection. Then set some other params accordingly.
void EffectChangePitch::DeduceFrequencies()
{
   // As a neat trick, attempt to get the frequency of the note at the
   // beginning of the selection.
   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   if (track) {
      double rate = track->GetRate();

      // Auto-size window -- high sample rates require larger windowSize.
      // Aim for around 2048 samples at 44.1 kHz (good down to about 100 Hz).
      // To detect single notes, analysis period should be about 0.2 seconds.
      // windowSize must be a power of 2.
      const size_t windowSize =
         // windowSize < 256 too inaccurate
         std::max(256, wxRound(pow(2.0, floor((log(rate / 20.0)/log(2.0)) + 0.5))));

      // we want about 0.2 seconds to catch the first note.
      // number of windows rounded to nearest integer >= 1.
      const unsigned numWindows =
         std::max(1, wxRound((double)(rate / (5.0f * windowSize))));

      double trackStart = track->GetStartTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      auto start = track->TimeToLongSamples(t0);

      auto analyzeSize = windowSize * numWindows;
      float * buffer;
      buffer = new float[analyzeSize];

      float * freq;
      freq = new float[windowSize / 2];

      float * freqa;
      freqa = new float[windowSize / 2];

      for(size_t j = 0; j < windowSize / 2; j++)
         freqa[j] = 0;

      track->Get((samplePtr) buffer, floatSample, start, analyzeSize);
      for(unsigned i = 0; i < numWindows; i++) {
         ComputeSpectrum(buffer + i * windowSize, windowSize,
                         windowSize, rate, freq, true);
         for(size_t j = 0; j < windowSize / 2; j++)
            freqa[j] += freq[j];
      }
      size_t argmax = 0;
      for(size_t j = 1; j < windowSize / 2; j++)
         if (freqa[j] > freqa[argmax])
            argmax = j;

      delete [] freq;
      delete [] freqa;
      delete [] buffer;

      auto lag = (windowSize / 2 - 1) - argmax;
      m_dStartFrequency = rate / lag;
   }

   double dFromMIDInote = FreqToMIDInote(m_dStartFrequency);
   double dToMIDInote = dFromMIDInote + m_dSemitonesChange;
   m_nFromPitch = PitchIndex(dFromMIDInote);
   m_nFromOctave = PitchOctave(dFromMIDInote);
   m_nToPitch = PitchIndex(dToMIDInote);
   m_nToOctave = PitchOctave(dToMIDInote);

   m_FromFrequency = m_dStartFrequency;
   Calc_PercentChange();
   Calc_ToFrequency();
}

// calculations

void EffectChangePitch::Calc_ToPitch()
{
   int nSemitonesChange =
      (int)(m_dSemitonesChange + ((m_dSemitonesChange < 0.0) ? -0.5 : 0.5));
   m_nToPitch = (m_nFromPitch + nSemitonesChange) % 12;
   if (m_nToPitch < 0)
      m_nToPitch += 12;
}

void EffectChangePitch::Calc_ToOctave()
{
   m_nToOctave = PitchOctave(FreqToMIDInote(m_ToFrequency));
}

void EffectChangePitch::Calc_SemitonesChange_fromPitches()
{
   m_dSemitonesChange =
      PitchToMIDInote(m_nToPitch, m_nToOctave) - PitchToMIDInote(m_nFromPitch, m_nFromOctave);
}

void EffectChangePitch::Calc_SemitonesChange_fromPercentChange()
{
   // Use m_dPercentChange rather than m_FromFrequency & m_ToFrequency, because
   // they start out uninitialized, but m_dPercentChange is always valid.
   m_dSemitonesChange = (12.0 * log((100.0 + m_dPercentChange) / 100.0)) / log(2.0);
}

void EffectChangePitch::Calc_ToFrequency()
{
   m_ToFrequency = (m_FromFrequency * (100.0 + m_dPercentChange)) / 100.0;
}

void EffectChangePitch::Calc_PercentChange()
{
   m_dPercentChange = 100.0 * (pow(2.0, (m_dSemitonesChange / 12.0)) - 1.0);
}


// handlers
void EffectChangePitch::OnChoice_FromPitch(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   m_nFromPitch = m_pChoice_FromPitch->GetSelection();
   m_FromFrequency = PitchToFreq(m_nFromPitch, m_nFromOctave);

   Calc_ToPitch();
   Calc_ToFrequency();
   Calc_ToOctave(); // Call after Calc_ToFrequency().

   m_bLoopDetect = true;
   {
      Update_Choice_ToPitch();
      Update_Spin_ToOctave();
      Update_Text_FromFrequency();
      Update_Text_ToFrequency();
   }
   m_bLoopDetect = false;
}

void EffectChangePitch::OnSpin_FromOctave(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   m_nFromOctave = m_pSpin_FromOctave->GetValue();
   //vvv If I change this code to not keep semitones and percent constant,
   // will need validation code as in OnSpin_ToOctave.
   m_FromFrequency = PitchToFreq(m_nFromPitch, m_nFromOctave);

   Calc_ToFrequency();
   Calc_ToOctave(); // Call after Calc_ToFrequency().

   m_bLoopDetect = true;
   {
      Update_Spin_ToOctave();
      Update_Text_FromFrequency();
      Update_Text_ToFrequency();
   }
   m_bLoopDetect = false;
}

void EffectChangePitch::OnChoice_ToPitch(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   m_nToPitch = m_pChoice_ToPitch->GetSelection();

   Calc_SemitonesChange_fromPitches();
   Calc_PercentChange(); // Call *after* m_dSemitonesChange is updated.
   Calc_ToFrequency(); // Call *after* m_dPercentChange is updated.

   m_bLoopDetect = true;
   {
      Update_Text_SemitonesChange();
      Update_Text_ToFrequency();
      Update_Text_PercentChange();
      Update_Slider_PercentChange();
   }
   m_bLoopDetect = false;
}

void EffectChangePitch::OnSpin_ToOctave(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   int nNewValue = m_pSpin_ToOctave->GetValue();
   // Validation: Rather than set a range for octave numbers, enforce a range that
   // keeps m_dPercentChange above -99%, per Soundtouch constraints.
   if ((nNewValue + 3) < m_nFromOctave)
   {
      ::wxBell();
      m_pSpin_ToOctave->SetValue(m_nFromOctave - 3);
      return;
   }
   m_nToOctave = nNewValue;

   m_ToFrequency = PitchToFreq(m_nToPitch, m_nToOctave);

   Calc_SemitonesChange_fromPitches();
   Calc_PercentChange(); // Call *after* m_dSemitonesChange is updated.

   m_bLoopDetect = true;
   {
      Update_Text_SemitonesChange();
      Update_Text_ToFrequency();
      Update_Text_PercentChange();
      Update_Slider_PercentChange();
   }
   m_bLoopDetect = false;
}

void EffectChangePitch::OnText_SemitonesChange(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   if (!m_pTextCtrl_SemitonesChange->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   Calc_PercentChange();
   Calc_ToFrequency(); // Call *after* m_dPercentChange is updated.
   Calc_ToPitch();
   Calc_ToOctave(); // Call after Calc_ToFrequency().

   m_bLoopDetect = true;
   {
      Update_Choice_ToPitch();
      Update_Spin_ToOctave();
      Update_Text_ToFrequency();
      Update_Text_PercentChange();
      Update_Slider_PercentChange();
   }
   m_bLoopDetect = false;

   // If m_dSemitonesChange is a big enough negative, we can go to or below 0 freq.
   // If m_dSemitonesChange is a big enough positive, we can go to 1.#INF (Windows) or inf (Linux).
   // But practically, these are best limits for Soundtouch.
   bool bIsGoodValue = (m_dSemitonesChange > -80.0) && (m_dSemitonesChange <= 60.0);
   EnableApply(bIsGoodValue);
}

void EffectChangePitch::OnText_FromFrequency(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   // Empty string causes unpredictable results with ToDouble() and later calculations.
   // Non-positive frequency makes no sense, but user might still be editing,
   // so it's not an error, but we do not want to update the values/controls.
   if (!m_pTextCtrl_FromFrequency->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   double newFromMIDInote = FreqToMIDInote(m_FromFrequency);
   m_nFromPitch = PitchIndex(newFromMIDInote);
   m_nFromOctave = PitchOctave(newFromMIDInote);
   Calc_ToPitch();
   Calc_ToFrequency();
   Calc_ToOctave(); // Call after Calc_ToFrequency().

   m_bLoopDetect = true;
   {
      Update_Choice_FromPitch();
      Update_Spin_FromOctave();
      Update_Choice_ToPitch();
      Update_Spin_ToOctave();
      Update_Text_ToFrequency();
   }
   m_bLoopDetect = false;

   // Success. Make sure OK and Preview are enabled, in case we disabled above during editing.
   EnableApply(true);
}

void EffectChangePitch::OnText_ToFrequency(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   // Empty string causes unpredictable results with ToDouble() and later calculations.
   // Non-positive frequency makes no sense, but user might still be editing,
   // so it's not an error, but we do not want to update the values/controls.
   if (!m_pTextCtrl_ToFrequency->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   m_dPercentChange = ((m_ToFrequency * 100.0) / m_FromFrequency) - 100.0;

   Calc_ToOctave(); // Call after Calc_ToFrequency().
   Calc_SemitonesChange_fromPercentChange();
   Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.

   m_bLoopDetect = true;
   {
      Update_Choice_ToPitch();
      Update_Spin_ToOctave();
      Update_Text_SemitonesChange();
      Update_Text_PercentChange();
      Update_Slider_PercentChange();
   }
   m_bLoopDetect = false;

   // Success. Make sure OK and Preview are disabled if percent change is out of bounds.
   // Can happen while editing.
   // If the value is good, might also need to re-enable because of above clause.
   bool bIsGoodValue = (m_dPercentChange > MIN_Percentage) && (m_dPercentChange <= MAX_Percentage);
   EnableApply(bIsGoodValue);
}

void EffectChangePitch::OnText_PercentChange(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   if (!m_pTextCtrl_PercentChange->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   Calc_SemitonesChange_fromPercentChange();
   Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.
   Calc_ToFrequency();
   Calc_ToOctave(); // Call after Calc_ToFrequency().

   m_bLoopDetect = true;
   {
      Update_Choice_ToPitch();
      Update_Spin_ToOctave();
      Update_Text_SemitonesChange();
      Update_Text_ToFrequency();
      Update_Slider_PercentChange();
   }
   m_bLoopDetect = false;

   // Success. Make sure OK and Preview are enabled, in case we disabled above during editing.
   EnableApply(true);
}

void EffectChangePitch::OnSlider_PercentChange(wxCommandEvent & WXUNUSED(evt))
{
   if (m_bLoopDetect)
      return;

   m_dPercentChange = (double)(m_pSlider_PercentChange->GetValue());
   // Warp positive values to actually go up faster & further than negatives.
   if (m_dPercentChange > 0.0)
      m_dPercentChange = pow(m_dPercentChange, kSliderWarp);

   Calc_SemitonesChange_fromPercentChange();
   Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.
   Calc_ToFrequency();
   Calc_ToOctave(); // Call after Calc_ToFrequency().

   m_bLoopDetect = true;
   {
      Update_Choice_ToPitch();
      Update_Spin_ToOctave();
      Update_Text_SemitonesChange();
      Update_Text_ToFrequency();
      Update_Text_PercentChange();
   }
   m_bLoopDetect = false;
}

// helper fns for controls

void EffectChangePitch::Update_Choice_FromPitch()
{
   m_pChoice_FromPitch->SetSelection(m_nFromPitch);
}

void EffectChangePitch::Update_Spin_FromOctave()
{
   m_pSpin_FromOctave->SetValue(m_nFromOctave);
}

void EffectChangePitch::Update_Choice_ToPitch()
{
   m_pChoice_ToPitch->SetSelection(m_nToPitch);
}

void EffectChangePitch::Update_Spin_ToOctave()
{
   m_pSpin_ToOctave->SetValue(m_nToOctave);
}

void EffectChangePitch::Update_Text_SemitonesChange()
{
   m_pTextCtrl_SemitonesChange->GetValidator()->TransferToWindow();
}

void EffectChangePitch::Update_Text_FromFrequency()
{
   m_pTextCtrl_FromFrequency->GetValidator()->TransferToWindow();
}

void EffectChangePitch::Update_Text_ToFrequency()
{
   m_pTextCtrl_ToFrequency->GetValidator()->TransferToWindow();
}

void EffectChangePitch::Update_Text_PercentChange()
{
   m_pTextCtrl_PercentChange->GetValidator()->TransferToWindow();
}

void EffectChangePitch::Update_Slider_PercentChange()
{
   double unwarped = m_dPercentChange;
   if (unwarped > 0.0)
      // Un-warp values above zero to actually go up to kSliderMax.
      unwarped = pow(m_dPercentChange, (1.0 / kSliderWarp));

   // Add 0.5 to unwarped so trunc -> round.
   m_pSlider_PercentChange->SetValue((int)(unwarped + 0.5));
}

#endif // USE_SOUNDTOUCH

