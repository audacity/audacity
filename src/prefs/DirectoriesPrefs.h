/**********************************************************************

  Audacity: A Digital Audio Editor

  DirectoriesPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_DIRECTORIES_PREFS__
#define __AUDACITY_DIRECTORIES_PREFS__

#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class DirectoriesPrefs:public PrefsPanel
{
 public:
   DirectoriesPrefs(wxWindow * parent);
   ~DirectoriesPrefs();
   virtual bool Apply();
   virtual bool Validate();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void UpdateFreeSpace(wxCommandEvent & e);
   void OnChooseTempDir(wxCommandEvent & e);

   wxStaticText *mFreeSpace;
   wxTextCtrl *mTempDir;

   DECLARE_EVENT_TABLE();
};

#endif
