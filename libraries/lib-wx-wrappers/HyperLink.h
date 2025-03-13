/**********************************************************************

  Audacity: A Digital Audio Editor

  HyperLink.h

**********************************************************************/
#pragma once

#include <wx/wx.h>
#include <wx/hyperlink.h>

class WX_WRAPPERS_API HyperLink : public wxHyperlinkCtrl {
public:
    HyperLink(wxWindow* parent, wxWindowID id, const wxString& label,
                   const wxString& url);
};

