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

#include "HtmlWindow.h" // to inherit
#include "wxPanelWrapper.h"

void OpenInDefaultBrowser(const wxHtmlLinkInfo& link);


/// \brief An HtmlWindow that handles linked clicked - usually the
/// link will go to our own local copy of the manual, but it could
/// launch a new browser window.
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


/// Adds some event handling to an HtmlWindow
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
