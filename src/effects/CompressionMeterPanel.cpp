/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressionMeterPanel.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "CompressionMeterPanel.h"
#include "DynamicRangeProcessorPanelCommon.h"
#include <wx/dcclient.h>

BEGIN_EVENT_TABLE(CompressionMeterPanel, wxPanelWrapper)
EVT_PAINT(CompressionMeterPanel::OnPaint)
END_EVENT_TABLE()

CompressionMeterPanel::CompressionMeterPanel(wxWindow* parent, wxWindowID winid)
    : wxPanelWrapper { parent, winid }
{
}

void CompressionMeterPanel::OnPaint(wxPaintEvent& evt)
{
   using namespace DynamicRangeProcessorPanel;

   wxPaintDC dc(this);

   // Just plain background for now
   dc.SetPen(lineColor);
   dc.SetBrush(backgroundColor);
   dc.DrawRectangle(GetClientRect());

   dc.SetFont(
      { 12, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL });
   dc.SetTextForeground({ 128, 128, 128 });
   const auto text = XO("compression").Translation();
   const auto t = dc.GetTextExtent(text);
   const auto x = (GetClientSize().GetWidth() + t.GetHeight()) / 2;
   const auto y = (GetClientSize().GetHeight() - t.GetWidth()) / 2;
   dc.DrawRotatedText(text, x, y, 270);
}

bool CompressionMeterPanel::AcceptsFocus() const
{
   return false;
}

bool CompressionMeterPanel::AcceptsFocusFromKeyboard() const
{
   return false;
}
