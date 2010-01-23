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

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class QualityPrefs:public PrefsPanel 
{
 public:
   QualityPrefs(wxWindow * parent);
   virtual ~QualityPrefs();

   virtual bool Apply();

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

   DECLARE_EVENT_TABLE();
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: ccb794d2-45d5-4f7b-ba0c-6a4d2438ac93
