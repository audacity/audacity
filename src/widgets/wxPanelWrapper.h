//
//  wxPanelWrapper.h
//  Audacity
//
//  Created by Paul Licameli on 6/25/16.
//
//

#ifndef __AUDACITY_WXPANEL_WRAPPER__
#define __AUDACITY_WXPANEL_WRAPPER__

#include <wx/panel.h>

class AUDACITY_DLL_API wxPanelWrapper /* not final */ : public wxPanel {
public:
   wxPanelWrapper() : wxPanel {} {}

   wxPanelWrapper(wxWindow * parent, wxWindowID id = wxID_ANY,
                const wxPoint & pos = wxDefaultPosition,
                const wxSize & size = wxDefaultSize,
                // default as for wxPanel:
                long style = wxTAB_TRAVERSAL | wxNO_BORDER);

   static void DoCharHook(wxKeyEvent &event);

private:
   void OnCharHook(wxKeyEvent &event);

   DECLARE_DYNAMIC_CLASS(wxPanelWrapper);

   DECLARE_EVENT_TABLE()
};

#endif
