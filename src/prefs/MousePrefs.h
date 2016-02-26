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

#include "PrefsPanel.h"

class ShuttleGui;

class MousePrefs final : public PrefsPanel
{
 public:
   MousePrefs(wxWindow * parent);
   ~MousePrefs();
   bool Apply() override;

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

class MousePrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *Create(wxWindow *parent) override;
};
#endif
