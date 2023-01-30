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
enum class sampleFormat : unsigned;
enum DitherType : unsigned;

#define QUALITY_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Quality") }

class AUDACITY_DLL_API QualityPrefs final : public PrefsPanel
{
 public:
   QualityPrefs(wxWindow * parent, wxWindowID winid);
   virtual ~QualityPrefs();
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   bool Commit() override;
   ManualPageID HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
   void GetNamesAndLabels();
   void OnSampleRateChoice(wxCommandEvent & e);

   TranslatableStrings mSampleRateNames;
   std::vector<int> mSampleRateLabels;

   wxChoice *mSampleRates;
   wxTextCtrl *mOtherSampleRate;
   int mOtherSampleRateValue;

   DECLARE_EVENT_TABLE()
};

#endif
