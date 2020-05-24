/**********************************************************************

  Audacity: A Digital Audio Editor

  Registrar.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

********************************************************************//**

\class TrackPanel2
\brief TrackPanel2 is the start of the new TrackPanel.

*//********************************************************************/

#include <wx/wx.h>
#include "ShuttleGui.h"
#include "widgets/LinkingHtmlWindow.h"
#include "SkewedRuler.h"
#include "Registrar.h"
#include "TrackPanel2.h"

TrackPanel * TrackPanel2Factory(wxWindow * parent,
   wxWindowID id,
   const wxPoint & pos,
   const wxSize & size,
   TrackList * tracks,
   ViewInfo * viewInfo,
   TrackPanelListener * listener,
   AdornedRulerPanel * ruler)
{
   return new TrackPanel2(
      parent,
      id,
      pos,
      size,
      tracks,
      viewInfo,
      listener,
      ruler);
}

void ShowExtraDialog()
{
   int k=42;

   wxDialog Dlg(NULL, wxID_ANY, wxString(wxT("Experimental Extra Dialog")));
   ShuttleGui S(&Dlg, eIsCreating);
   S.StartNotebook();
   {
      S.StartNotebookPage( _("Panel 1") );
      S.StartVerticalLay(1);
      {
         HtmlWindow *html = new LinkingHtmlWindow(S.GetParent(), -1,
                                               wxDefaultPosition,
                                               wxSize(600, 359), 
                                               wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER);
         html->SetFocus();
         html->SetPage(wxT("<h1><font color=\"blue\">An Html Window</font></h1>Replace with whatever you like."));
         S.Prop(1).AddWindow( html, wxEXPAND );
      }
      S.EndVerticalLay();
      S.EndNotebookPage();

      S.StartNotebookPage( _("Diagnostics") );
      S.StartVerticalLay(1);
      {
         HtmlWindow *html = new LinkingHtmlWindow(S.GetParent(), -1,
                                               wxDefaultPosition,
                                               wxSize(600, 359), 
                                               wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER);
         html->SetFocus();
         html->SetPage(wxT("<h1>Diagnostics</h1>This is an html diagnostics page"));
         S.Prop(1).AddWindow( html, wxEXPAND );
      }
      S.EndVerticalLay();
      S.EndNotebookPage();
   }
   S.EndNotebook();
   
   wxButton *ok = new wxButton(S.GetParent(), wxID_OK, _("OK... Audacious!"));
   ok->SetDefault();
   S.Prop(0).AddWindow( ok );

   Dlg.Fit();

   Dlg.ShowModal();
}


int TrackPanel2Dispatch( Registrar & R, t_RegistrarDispatchType Type )
{
   switch( Type )
   {
   case RegResource:
      R.pShowFn = ShowExtraDialog;
      break;
   case RegArtist:
      break;
   case RegDataType:
      break;
   case RegCommand:
      break;
   case RegMenuItem:
      break;
   default:
      break;
   }
   return 1;
}

TrackPanel2::TrackPanel2(
   wxWindow * parent, wxWindowID id, const wxPoint & pos, const wxSize & size,
   TrackList * tracks, ViewInfo * viewInfo, TrackPanelListener * listener,
   AdornedRulerPanel * ruler) : 
      TrackPanel(
         parent, id, pos, size,
         tracks, viewInfo, listener, ruler)
{
}


// Here is a sample function that shows that TrackPanel2 is being invoked.
void TrackPanel2::OnPaint(wxPaintEvent & event)
{
// Hmm... Log debug will only show if you open the log window.
// wxLogDebug( wxT("Paint TrackPanel2 requested") );
   TrackPanel::OnPaint( event );
}

