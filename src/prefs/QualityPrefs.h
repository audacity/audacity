/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_QUALITY_PREFS__
#define __AUDACITY_QUALITY_PREFS__

#include <wx/defs.h>

#include <wx/arrstr.h>
#include <wx/choice.h>
#include <wx/dynarray.h>
#include <wx/textctrl.h>

#include "PrefsPanel.h"

class ShuttleGui;

class QualityPrefs final : public PrefsPanel
{
 public:
   QualityPrefs(wxWindow * parent);
   virtual ~QualityPrefs();

   bool Apply() override;

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void GetNamesAndLabels();
   void OnSampleRateChoice(wxCommandEvent & e);

   wxArrayString mDitherNames;
   wxArrayInt    mDitherLabels;
   wxArrayString mSampleRateNames;
   wxArrayInt    mSampleRateLabels;
   wxArrayString mSampleFormatNames;
   wxArrayInt    mSampleFormatLabels;
   wxArrayString mConverterNames;
   wxArrayInt    mConverterLabels;

   wxChoice *mSampleRates;
   wxTextCtrl *mOtherSampleRate;
   int mOtherSampleRateValue;

   DECLARE_EVENT_TABLE()
};

class QualityPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
