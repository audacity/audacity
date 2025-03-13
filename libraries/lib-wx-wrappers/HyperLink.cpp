/**********************************************************************

  Audacity: A Digital Audio Editor

  HyperLink.cpp

**********************************************************************/

#include "HyperLink.h"

#include "Theme.h"
#include "AllThemeResources.h"

HyperLink::HyperLink(wxWindow* parent, wxWindowID id, const wxString& label,
               const wxString& url)
      : wxHyperlinkCtrl(parent, id, label, url)
{
   this->SetVisitedColour(theTheme.Colour(clrSample));
   this->SetHoverColour(theTheme.Colour(clrSample));
   this->SetNormalColour(theTheme.Colour(clrSample));
}
