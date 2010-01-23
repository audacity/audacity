/**********************************************************************

  Audacity: A Digital Audio Editor

  MousePrefs.h

**********************************************************************/

#ifndef __AUDACITY_MOUSE_PREFS__
#define __AUDACITY_MOUSE_PREFS__

#include <wx/defs.h>

#include <wx/listctrl.h>
#include <wx/string.h>
#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class MousePrefs:public PrefsPanel
{
 public:
   MousePrefs(wxWindow * parent);
   ~MousePrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void CreateList();
   void AddItem(wxString const & buttons, 
                wxString const & tool, 
                wxString const & action,
                wxString const & comment = wxEmptyString);

   wxListCtrl * mList;
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
// arch-tag: d7366e17-0464-4863-8b68-de4edea64974

