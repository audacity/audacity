/**********************************************************************

  Audacity: A Digital Audio Editor

  MousePrefs.h

**********************************************************************/

#ifndef __AUDACITY_MOUSE_PREFS__
#define __AUDACITY_MOUSE_PREFS__

#include <wx/defs.h>

#include <wx/listctrl.h>

#include "PrefsPanel.h"

class ShuttleGui;

class MousePrefs final : public PrefsPanel
{
 public:
   MousePrefs(wxWindow * parent, wxWindowID winid);
   ~MousePrefs();
   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
   void CreateList();
   void AddItem(wxString const & buttons,
                wxString const & tool,
                wxString const & action,
                wxString const & comment = {});

   wxListCtrl * mList;
};

/// A PrefsPanelFactory that creates one MousePrefs panel.
class MousePrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
