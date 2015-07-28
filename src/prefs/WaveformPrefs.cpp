/**********************************************************************

Audacity: A Digital Audio Editor

WaveformPrefs.cpp

Paul Licameli

*******************************************************************//**

\class WaveformPrefs
\brief A PrefsPanel for spectrum settings.

*//*******************************************************************/

#include "../Audacity.h"
#include "WaveformPrefs.h"

#include <wx/checkbox.h>

#include "../Project.h"
#include "../TrackPanel.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"

WaveformPrefs::WaveformPrefs(wxWindow * parent, WaveTrack *wt)
: PrefsPanel(parent, _("Waveforms"))
, mWt(wt)
, mPopulating(false)
{
   if (mWt) {
      WaveformSettings &settings = wt->GetWaveformSettings();
      mDefaulted = (&WaveformSettings::defaults() == &settings);
      mTempSettings = settings;
   }
   else  {
      mTempSettings = WaveformSettings::defaults();
      mDefaulted = false;
   }

   Populate();
}

WaveformPrefs::~WaveformPrefs()
{
}

enum {
   ID_DEFAULTS = 10001,
   ID_APPLY,

   ID_SCALE,
};

void WaveformPrefs::Populate()
{
   mScaleChoices = WaveformSettings::GetScaleNames();

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void WaveformPrefs::PopulateOrExchange(ShuttleGui & S)
{
   mPopulating = true;

   S.SetBorder(2);

   // S.StartStatic(_("Track Settings"));
   {
      mDefaultsCheckbox = 0;
      if (mWt) {
         mDefaultsCheckbox = S.Id(ID_DEFAULTS).TieCheckBox(_("Defaults"), mDefaulted);
      }

      S.StartStatic(_("Display"));
      {
         S.StartTwoColumn();
         {
            S.Id(ID_SCALE).TieChoice(_("S&cale") + wxString(wxT(":")),
               *(int*)&mTempSettings.scaleType,
               &mScaleChoices);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   // S.EndStatic();

   /*
   S.StartStatic(_("Global settings"));
   {
   }
   S.EndStatic();
   */

   S.StartMultiColumn(2, wxALIGN_RIGHT);
   {
      S.Id(ID_APPLY).AddButton(_("Appl&y"));
   }
   S.EndMultiColumn();

   mPopulating = false;
}

bool WaveformPrefs::Validate()
{
   // Do checking for whole numbers

   // ToDo: use wxIntegerValidator<unsigned> when available

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   // Delegate range checking to WaveformSettings class
   const bool result = mTempSettings.Validate(false);
   return result;
}

bool WaveformPrefs::Apply()
{
   const bool isOpenPage = this->IsShown();

   WaveTrack *const partner =
      mWt ? static_cast<WaveTrack*>(mWt->GetLink()) : 0;

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   WaveformSettings::Globals::Get().SavePrefs();

   if (mWt) {
      if (mDefaulted) {
         mWt->SetWaveformSettings(NULL);
         if (partner)
            partner->SetWaveformSettings(NULL);
      }
      else {
         WaveformSettings *pSettings =
            &mWt->GetIndependentWaveformSettings();
         *pSettings = mTempSettings;
         if (partner) {
            pSettings = &partner->GetIndependentWaveformSettings();
            *pSettings = mTempSettings;
         }
      }
   }

   if (!mWt || mDefaulted) {
      WaveformSettings *const pSettings =
         &WaveformSettings::defaults();
      *pSettings = mTempSettings;
      pSettings->SavePrefs();
   }

   if (mWt && isOpenPage) {
      mWt->SetDisplay(WaveTrack::Waveform);
      if (partner)
         partner->SetDisplay(WaveTrack::Waveform);
   }

   return true;
}

void WaveformPrefs::OnControl(wxCommandEvent&)
{
   // Common routine for most controls
   // If any per-track setting is changed, break the association with defaults
   // Skip this, and View Settings... will be able to change defaults instead
   // when the checkbox is on, as in the original design.

   if (mDefaultsCheckbox && !mPopulating) {
      mDefaulted = false;
      mDefaultsCheckbox->SetValue(false);
   }
}

void WaveformPrefs::OnDefaults(wxCommandEvent &)
{
   if (mDefaultsCheckbox->IsChecked()) {
      mTempSettings = WaveformSettings::defaults();
      mDefaulted = true;
      ShuttleGui S(this, eIsSettingToDialog);
      PopulateOrExchange(S);
   }
}

void WaveformPrefs::OnApply(wxCommandEvent &)
{
   if (Validate()) {
      Apply();
      ::GetActiveProject()->GetTrackPanel()->Refresh(false);
   }
}

BEGIN_EVENT_TABLE(WaveformPrefs, PrefsPanel)

EVT_CHOICE(ID_SCALE, WaveformPrefs::OnControl)

EVT_CHECKBOX(ID_DEFAULTS, WaveformPrefs::OnDefaults)
EVT_BUTTON(ID_APPLY, WaveformPrefs::OnApply)
END_EVENT_TABLE()

WaveformPrefsFactory::WaveformPrefsFactory(WaveTrack *wt)
: mWt(wt)
{
}

PrefsPanel *WaveformPrefsFactory::Create(wxWindow *parent)
{
   return new WaveformPrefs(parent, mWt);
}
