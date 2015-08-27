/**********************************************************************

Audacity: A Digital Audio Editor

WaveformPrefs.h

Paul Licameli

**********************************************************************/


#ifndef __AUDACITY_WAVEFORM_PREFS__
#define __AUDACITY_WAVEFORM_PREFS__

#include "PrefsPanel.h"
#include "WaveformSettings.h"

class ShuttleGui;
class WaveTrack;
class wxCheckBox;
class wxChoice;

class WaveformPrefs :public PrefsPanel
{
public:
   WaveformPrefs(wxWindow * parent, WaveTrack *wt);
   virtual ~WaveformPrefs();
   virtual bool Apply();
   virtual bool ShowsApplyButton();
   virtual bool Validate();

private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   void OnControl(wxCommandEvent&);
   void OnScale(wxCommandEvent&);
   void OnDefaults(wxCommandEvent&);
   DECLARE_EVENT_TABLE()

   void EnableDisableRange();

   WaveTrack *const mWt;
   bool mDefaulted;

   wxCheckBox *mDefaultsCheckbox;
   wxChoice *mScaleChoice;
   wxChoice *mRangeChoice;

   wxArrayString mScaleChoices;
   wxArrayString mRangeCodes;
   wxArrayString mRangeChoices;

   WaveformSettings mTempSettings;

   bool mPopulating;
};

class WaveformPrefsFactory : public PrefsPanelFactory
{
public:
   explicit WaveformPrefsFactory(WaveTrack *wt = 0);
   virtual PrefsPanel *Create(wxWindow *parent);

private:
   WaveTrack *const mWt;
};
#endif
