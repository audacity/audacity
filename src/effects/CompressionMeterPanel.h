/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressionMeterPanel.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

class CompressionMeterPanel final : public wxPanelWrapper
{
public:
   CompressionMeterPanel(wxWindow* parent, wxWindowID winid);

   DECLARE_EVENT_TABLE();

private:
   void OnPaint(wxPaintEvent& evt);

   bool AcceptsFocus() const override;
   // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
   bool AcceptsFocusFromKeyboard() const override;
};
