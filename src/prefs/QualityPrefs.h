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

#include "PrefsPanel.h"

class wxChoice;
class wxTextCtrl;
class ShuttleGui;
enum sampleFormat : unsigned;
enum DitherType : unsigned;

class wxArrayStringEx;

class QualityPrefs final : public PrefsPanel
{
 public:
   QualityPrefs(wxWindow * parent, wxWindowID winid);
   virtual ~QualityPrefs();

   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

   static sampleFormat SampleFormatChoice();

   static DitherType FastDitherChoice();
   static DitherType BestDitherChoice();

 private:
   void Populate();
   void GetNamesAndLabels();
   void OnSampleRateChoice(wxCommandEvent & e);

   wxArrayStringEx mSampleRateNames;
   std::vector<int> mSampleRateLabels;

   wxChoice *mSampleRates;
   wxTextCtrl *mOtherSampleRate;
   int mOtherSampleRateValue;

   DECLARE_EVENT_TABLE()
};

/// A PrefsPanelFactory that creates one QualityPrefs panel.
class QualityPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
