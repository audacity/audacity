/**********************************************************************

  Audacity: A Digital Audio Editor

  LinkingHtmlWindow.cpp

  Vaughan Johnson
  Dominic Mazzoni

  utility fn and
  descendant of HtmlWindow that opens links in the user's
  default browser

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include "../Audacity.h"

#include <wx/mimetype.h>
#include <wx/filename.h>
#include <wx/uri.h>

#include "LinkingHtmlWindow.h"
#include "../HelpText.h"
#include "../FileNames.h"
#include "ErrorDialog.h"
#include "HelpSystem.h"

BEGIN_EVENT_TABLE(BrowserDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_FORWARD,  BrowserDialog::OnForward)
   EVT_BUTTON(wxID_BACKWARD, BrowserDialog::OnBackward)
   EVT_BUTTON(wxID_CANCEL,   BrowserDialog::OnClose)
   EVT_KEY_DOWN(BrowserDialog::OnKeyDown)
END_EVENT_TABLE()


BrowserDialog::BrowserDialog(wxWindow *pParent, const wxString &title)
   : wxDialogWrapper{ pParent, ID, title }
{

}

void BrowserDialog::OnForward(wxCommandEvent & WXUNUSED(event))
{
   mpHtml->HistoryForward();
   UpdateButtons();
}

void BrowserDialog::OnBackward(wxCommandEvent & WXUNUSED(event))
{
   mpHtml->HistoryBack();
   UpdateButtons();
}

void BrowserDialog::OnClose(wxCommandEvent & WXUNUSED(event))
{
   if (IsModal() && !mDismissed)
   {
      mDismissed = true;
      EndModal(wxID_CANCEL);
   }
   auto parent = GetParent();

#ifdef __WXMAC__
   auto grandparent = GetParent()->GetParent();
#endif

   parent->Destroy();

#ifdef __WXMAC__
   if(grandparent && grandparent->IsShown()) {
      grandparent->Raise();
   }
#endif
}

void BrowserDialog::OnKeyDown(wxKeyEvent & event)
{
   bool bSkip = true;
   if (event.GetKeyCode() == WXK_ESCAPE)
   {
      bSkip = false;
      Close(false);
   }
   event.Skip(bSkip);
}


void BrowserDialog::UpdateButtons()
{
   wxWindow * pWnd;
   if( (pWnd = FindWindowById( wxID_BACKWARD, this )) != NULL )
   {
      pWnd->Enable(mpHtml->HistoryCanBack());
   }
   if( (pWnd = FindWindowById( wxID_FORWARD, this )) != NULL )
   {
      pWnd->Enable(mpHtml->HistoryCanForward());
   }
}

void OpenInDefaultBrowser(const wxHtmlLinkInfo& link)
{
   wxURI uri(link.GetHref());
   wxLaunchDefaultBrowser(uri.BuildURI());
}

LinkingHtmlWindow::LinkingHtmlWindow(wxWindow *parent, wxWindowID id /*= -1*/,
                                       const wxPoint& pos /*= wxDefaultPosition*/,
                                       const wxSize& size /*= wxDefaultSize*/,
                                       long style /*= wxHW_SCROLLBAR_AUTO*/) :
   HtmlWindow(parent, id, pos, size, style)
{
}

void LinkingHtmlWindow::OnLinkClicked(const wxHtmlLinkInfo& link)
{
   wxString href = link.GetHref();
   if( href.StartsWith(wxT("innerlink:")) )
   {
      wxString FileName =
         wxFileName( FileNames::HtmlHelpDir(), href.Mid( 10 ) + wxT(".htm") ).GetFullPath();
      if( wxFileExists( FileName ) )
      {
         HelpSystem::ShowHelpDialog(this, FileName, wxT(""));
         return;
      }
      else
      {
         SetPage( HelpText( href.Mid( 10 )));
         wxGetTopLevelParent(this)->SetLabel( TitleText( href.Mid( 10 )));
      }
   }
   else if( href.StartsWith(wxT("mailto:")) || href.StartsWith(wxT("file:")) )
   {
      OpenInDefaultBrowser( link );
      return;
   }
   else if( !href.StartsWith( wxT("http:"))  && !href.StartsWith( wxT("https:")) )
   {
      HtmlWindow::OnLinkClicked( link );
   }
   else
   {
      OpenInDefaultBrowser(link);
      return;
   }
   BrowserDialog * pDlg = wxDynamicCast(
      GetRelatedFrame()->FindWindow(BrowserDialog::ID), BrowserDialog );
   if( pDlg )
   {
      pDlg->UpdateButtons();
   };
}
