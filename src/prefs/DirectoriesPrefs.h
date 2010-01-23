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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: f9f9d986-cc0f-4cd9-9589-7743d9f2c6d8

