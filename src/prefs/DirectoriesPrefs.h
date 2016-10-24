/**********************************************************************

  Audacity: A Digital Audio Editor

  DirectoriesPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_DIRECTORIES_PREFS__
#define __AUDACITY_DIRECTORIES_PREFS__

#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "PrefsPanel.h"

class ShuttleGui;

class DirectoriesPrefs final : public PrefsPanel
{
 public:
   DirectoriesPrefs(wxWindow * parent);
   ~DirectoriesPrefs();
   bool Apply() override;
   bool Validate() override;

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void UpdateFreeSpace(wxCommandEvent & e);
   void OnChooseTempDir(wxCommandEvent & e);

   wxStaticText *mFreeSpace;
   wxTextCtrl *mTempDir;

   DECLARE_EVENT_TABLE()
};

class DirectoriesPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
