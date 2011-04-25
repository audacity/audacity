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


void ShowTrackPanel()
{
   int k=42;

   wxDialog Dlg(NULL, wxID_ANY, wxString(wxT("Experimental New TrackPanel")));
   ShuttleGui S(&Dlg, eIsCreating);
#if 0
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.StartStatic(wxT(""), false);   
      {
         S.SetBorder(200);
         S.AddFixedText(wxT("AAA"));
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();
#endif
   S.StartNotebook();
   {
      S.StartNotebookPage( _("Panel") );
      S.StartVerticalLay(1);
      {
         HtmlWindow *html = new LinkingHtmlWindow(S.GetParent(), -1,
                                               wxDefaultPosition,
                                               wxSize(600, 359), 
                                               wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER);
         html->SetFocus();
         html->SetPage(wxT("<h1><font color=\"blue\">TrackPanel</font></h1>This will be replaced with the panel"));
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
      R.pShowFn = ShowTrackPanel;
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

