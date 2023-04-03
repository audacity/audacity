/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file auStaticText.cpp

  Paul Licameli split from Theme.cpp

*//*****************************************************************//**

\class auStaticText
\brief is like wxStaticText, except it can be themed.  wxStaticText
can't be.

*//*****************************************************************/
#include "auStaticText.h"

#include "AllThemeResources.h"
#include "Theme.h"

#include <cassert>

#include <wx/dcclient.h>

BEGIN_EVENT_TABLE(auStaticText, wxWindow)
    EVT_PAINT(auStaticText::OnPaint)
    EVT_ERASE_BACKGROUND(auStaticText::OnErase)
END_EVENT_TABLE()

 
auStaticText::auStaticText(wxWindow* parent, wxString textIn) :
 wxWindow(parent, wxID_ANY)
{
   int textWidth, textHeight;

   int fontSize = 11;
   #ifdef __WXMSW__
      fontSize = 9;
   #endif
   wxFont font(fontSize, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   GetTextExtent(textIn, &textWidth, &textHeight, NULL, NULL, &font);

   SetFont( font );
   SetMinSize( wxSize(textWidth, textHeight) );
   SetBackgroundColour( theTheme.Colour( clrMedium));
   SetForegroundColour( theTheme.Colour( clrTrackPanelText));
   SetName(textIn);
   SetLabel(textIn);
}
 
void auStaticText::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   wxPaintDC dc(this);
   //dc.SetTextForeground( theTheme.Colour( clrTrackPanelText));
   dc.Clear();
   dc.DrawText( GetLabel(), 0,0);
}

void auStaticText::ScaleFont(double scale)
{
   if (scale < 0.0)
   {
      assert(scale >= 0.0);
      return;
   }

   int textWidth, textHeight;

   auto font = GetFont();

   const auto oldFontSize = font.GetPointSize();
   const auto newFontSize = static_cast<int>(oldFontSize * scale);

   if (newFontSize != oldFontSize)
   {
      font.SetPointSize(newFontSize);
      GetTextExtent(GetLabel(), &textWidth, &textHeight, NULL, NULL, &font);
      SetFont(font);
      SetMinSize(wxSize(textWidth, textHeight));
   }
}
