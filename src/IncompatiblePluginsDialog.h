/**********************************************************************

  Audacity: A Digital Audio Editor

  @file IncompatiblePluginsDialog.h

  @author Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <vector>
#include <wx/string.h>
#include "widgets/wxPanelWrapper.h"

class wxStaticText;
class wxTextCtrl;

///Used to display a list of found plugins that aren't compatible
///with Audacity
class IncompatiblePluginsDialog final : public wxDialogWrapper
{
   wxStaticText* mText{nullptr};
   wxTextCtrl* mPluginList{nullptr};
public:

   IncompatiblePluginsDialog(wxWindow *parent, wxWindowID id,
             const std::vector<wxString>& plugins = { },
             const wxPoint& pos = wxDefaultPosition,
             const wxSize& size = wxDefaultSize);

   void SetPlugins(const std::vector<wxString>& plugins);

private:
   void OnPluginManagerClicked(wxCommandEvent&);
   void OnContinueClicked(wxCommandEvent&);
};
