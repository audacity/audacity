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

#include "LinkingHtmlWindow.h"
#include "../HelpText.h"
#include "../FileNames.h"
#include "ErrorDialog.h"
#include "HelpSystem.h"

BEGIN_EVENT_TABLE(BrowserFrame, wxFrame)
   EVT_BUTTON(wxID_FORWARD,  BrowserFrame::OnForward)
   EVT_BUTTON(wxID_BACKWARD, BrowserFrame::OnBackward)
   EVT_BUTTON(wxID_CANCEL,   BrowserFrame::OnClose)
   EVT_KEY_DOWN(BrowserFrame::OnKeyDown)
END_EVENT_TABLE()


void BrowserFrame::OnForward(wxCommandEvent & WXUNUSED(event))
{
   mpHtml->HistoryForward();
   UpdateButtons();
}

void BrowserFrame::OnBackward(wxCommandEvent & WXUNUSED(event))
{
   mpHtml->HistoryBack();
   UpdateButtons();
}

void BrowserFrame::OnClose(wxCommandEvent & WXUNUSED(event))
{
   Close();
}

void BrowserFrame::OnKeyDown(wxKeyEvent & event)
{
   bool bSkip = true;
   if (event.GetKeyCode() == WXK_ESCAPE)
   {
      bSkip = false;
      Close(false);
   }
   event.Skip(bSkip);
}


void BrowserFrame::UpdateButtons()
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
   wxLaunchDefaultBrowser(link.GetHref());
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
         HelpSystem::ShowHelpDialog(NULL, FileName, wxT(""));
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
   else if( !href.StartsWith( wxT("http:")))
   {
      HtmlWindow::OnLinkClicked( link );
   }
   else
   {
      OpenInDefaultBrowser(link);
      return;
   }
   BrowserFrame * pDlg = wxDynamicCast( GetRelatedFrame(), BrowserFrame );
   if( pDlg )
   {
      pDlg->UpdateButtons();
   };
}
