/**********************************************************************

  Audacity: A Digital Audio Editor

  LinkingHtmlWindow.h

  Vaughan Johnson
  Dominic Mazzoni

  utility fn and
  descendant of HtmlWindow that opens links in the user's
  default browser

**********************************************************************/

#ifndef __AUDACITY_LINKINGHTMLWINDOW__
#define __AUDACITY_LINKINGHTMLWINDOW__

#include <wx/html/htmlwin.h>
#include <wx/frame.h>

#include "HtmlWindow.h"
#include "wxPanelWrapper.h"

void OpenInDefaultBrowser(const wxHtmlLinkInfo& link);

class AUDACITY_DLL_API LinkingHtmlWindow final : public HtmlWindow
{
 public:
   LinkingHtmlWindow(wxWindow *parent, wxWindowID id = -1,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxHW_SCROLLBAR_AUTO);
   void OnLinkClicked(const wxHtmlLinkInfo& link) override;
   //void OnSetTitle(const wxString& title) override;

};

class BrowserDialog /* not final */ : public wxDialogWrapper
{
public:
   enum { ID = 0 };
   BrowserDialog(wxWindow *pParent, const wxString &title);

   void OnForward(wxCommandEvent & event);
   void OnBackward(wxCommandEvent & event);
   void OnClose(wxCommandEvent & event);
   void OnKeyDown(wxKeyEvent & event);

   void UpdateButtons();
   //void SetLabel(const wxString& label) override;


   HtmlWindow * mpHtml;
   bool mDismissed{};
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_LINKINGHTMLWINDOW__
