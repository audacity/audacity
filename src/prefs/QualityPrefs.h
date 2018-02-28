/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_QUALITY_PREFS__
#define __AUDACITY_QUALITY_PREFS__

#include <vector>
#include <wx/defs.h>

#include <wx/arrstr.h>
#include <wx/choice.h>
#include <wx/textctrl.h>

#include "PrefsPanel.h"

class ShuttleGui;

class QualityPrefs final : public PrefsPanel
{
 public:
   QualityPrefs(wxWindow * parent, wxWindowID winid);
   virtual ~QualityPrefs();

   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
   void GetNamesAndLabels();
   void OnSampleRateChoice(wxCommandEvent & e);

   wxArrayString mDitherNames;
   std::vector<int> mDitherLabels;
   wxArrayString mSampleRateNames;
   std::vector<int> mSampleRateLabels;
   wxArrayString mSampleFormatNames;
   std::vector<int> mSampleFormatLabels;
   wxArrayString mConverterNames;
   std::vector<int> mConverterLabels;

   wxChoice *mSampleRates;
   wxTextCtrl *mOtherSampleRate;
   int mOtherSampleRateValue;

   DECLARE_EVENT_TABLE()
};

class QualityPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
