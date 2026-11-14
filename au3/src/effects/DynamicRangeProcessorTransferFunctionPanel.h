/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorTransferFunctionPanel.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorHistory.h"
#include "wxPanelWrapper.h"

class wxPaintEvent;
class wxEraseEvent;

class DynamicRangeProcessorTransferFunctionPanel final : public wxPanelWrapper
{
public:
    static constexpr auto minHeight = 100;
    static constexpr auto rangeDb = 60;

    DynamicRangeProcessorTransferFunctionPanel(
        wxWindow* parent, wxWindowID winid, const CompressorSettings& compressorSettings);

    DECLARE_EVENT_TABLE();

private:
    bool AcceptsFocus() const override;
    // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
    bool AcceptsFocusFromKeyboard() const override;

    const CompressorSettings& mCompressorSettings;
    void OnPaint(wxPaintEvent& evt);
    void OnSize(wxSizeEvent& evt);
};
