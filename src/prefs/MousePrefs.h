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
