/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

   Reverb.h
   Rob Sykes, Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERB__
#define __AUDACITY_EFFECT_REVERB__

#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>

#include "Effect.h"

class wxSpinCtrl;
class WaveTrack;

struct Reverb_priv_t;

class EffectReverb : public Effect
{
public:
   EffectReverb();
   virtual ~EffectReverb() {};

   // Implemented from the base class 'Effect':
   virtual wxString GetEffectName() {return _("Reverb...");}
   virtual wxString GetEffectAction() {return _("Applying Reverb");}
   virtual wxString GetEffectIdentifier() {return wxT("Reverb");}
   virtual wxString GetEffectDescription(); // Useful only after PromptUser values have been set.
   virtual bool TransferParameters(Shuttle & shuttle);

 protected:
   bool PromptUser();
   bool Process();

   // Processing:
   void Create(double rate, bool isStereo);
   bool ProcessOneBlock(sampleCount len, float * const * chans);
   bool ProcessOneTrack(size_t n, WaveTrack * track, WaveTrack * track2, wxString const & msg);
   void Delete();
   double mCurT0, mCurT1;
   Reverb_priv_t * mP;

   // Settings:
   wxString SettingsPath(int settingsNumber) const;
   wxString SettingsName(int settingsNumber) const;

   struct Params {
      double mRoomSize;
      double mDelay;
      double mReverberance;
      double mHfDamping;
      double mToneLow;
      double mToneHigh;
      double mWetGain;
      double mDryGain;
      double mStereoWidth;
      bool mWetOnly;
   };
   void LoadSettings(int settingsNumber, Params & params);
   void SaveSettings(int settingsNumber, Params const * params, wxString const * name = 0) const;

   Params mParams;

   friend class ReverbDialogue;
};

//----------------------------------------------------------------------------
// ReverbDialogue
//----------------------------------------------------------------------------
class ReverbDialogue : public EffectDialog
{
public:
   ReverbDialogue(EffectReverb * effect, wxWindow * parent);
   virtual ~ReverbDialogue() {};

private:
   void SetTitle(wxString const & name = wxT(""));
   void PopulateOrExchange(ShuttleGui &);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   void LoadPreset(wxCommandEvent & WXUNUSED(event));
   int  ChooseSettings(wxString const & message);
   void LoadSettings(wxCommandEvent & WXUNUSED(event));
   void RenameSettings(wxCommandEvent & WXUNUSED(event));
   void SaveSettings(wxCommandEvent & WXUNUSED(event));
   void OnPreview(wxCommandEvent & event);

   // event handlers and member vars
   void OnRoomSizeWidget(wxCommandEvent & event);
   void OnRoomSizeText(wxCommandEvent & event);
   wxSlider * mRoomSizeWidget;
   wxSpinCtrl * mRoomSizeText;

   void OnDelayWidget(wxCommandEvent & event);
   void OnDelayText(wxCommandEvent & event);
   wxSlider * mDelayWidget;
   wxSpinCtrl * mDelayText;

   void OnReverberanceWidget(wxCommandEvent & event);
   void OnReverberanceText(wxCommandEvent & event);
   wxSlider *  mReverberanceWidget;
   wxSpinCtrl * mReverberanceText;

   void OnHfDampingWidget(wxCommandEvent & event);
   void OnHfDampingText(wxCommandEvent & event);
   wxSlider * mHfDampingWidget;
   wxSpinCtrl * mHfDampingText;

   void OnToneLowWidget(wxCommandEvent & event);
   void OnToneLowText(wxCommandEvent & event);
   wxSlider * mToneLowWidget;
   wxSpinCtrl * mToneLowText;

   void OnToneHighWidget(wxCommandEvent & event);
   void OnToneHighText(wxCommandEvent & event);
   wxSlider * mToneHighWidget;
   wxSpinCtrl * mToneHighText;

   void OnWetGainWidget(wxCommandEvent & event);
   void OnWetGainText(wxCommandEvent & event);
   wxSlider * mWetGainWidget;
   wxSpinCtrl * mWetGainText;

   void OnDryGainWidget(wxCommandEvent & event);
   void OnDryGainText(wxCommandEvent & event);
   wxSlider * mDryGainWidget;
   wxSpinCtrl * mDryGainText;

   void OnStereoWidthWidget(wxCommandEvent & event);
   void OnStereoWidthText(wxCommandEvent & event);
   wxSlider * mStereoWidthWidget;
   wxSpinCtrl * mStereoWidthText;

   wxCheckBox * mWetOnlyWidget;


   EffectReverb & mEffect;
   EffectReverb::Params & mParams;

   DECLARE_EVENT_TABLE()
};

#endif
