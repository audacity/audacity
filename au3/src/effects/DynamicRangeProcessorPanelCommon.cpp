/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorPanelCommon.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DynamicRangeProcessorPanelCommon.h"
#include "wxPanelWrapper.h"
#include <wx/dcclient.h>
#include <wx/pen.h>

namespace DynamicRangeProcessorPanel {
std::unique_ptr<wxGraphicsContext> MakeGraphicsContext(const wxPaintDC& dc)
{
    auto gc = wxGraphicsContext::Create(dc);
    gc->SetAntialiasMode(wxANTIALIAS_DEFAULT);
    gc->SetInterpolationQuality(wxINTERPOLATION_BEST);
    return std::unique_ptr<wxGraphicsContext>(gc);
}

wxColor GetColorMix(const wxColor& a, const wxColor& b, double aWeight)
{
    return wxColor(
        a.Red() * aWeight + b.Red() * (1 - aWeight),
        a.Green() * aWeight + b.Green() * (1 - aWeight),
        a.Blue() * aWeight + b.Blue() * (1 - aWeight),
        a.Alpha() * aWeight + b.Alpha() * (1 - aWeight));
}

float GetGraphDbRange(int height)
{
    const auto factor = std::max(1.f, 1.f * height / graphMinHeight);
    return factor * graphMinRangeDb;
}

wxGraphicsBrush GetGraphBackgroundBrush(wxGraphicsContext& gc, int height)
{
    // It looks like we're not going to use a gradient for the background after
    // all, but keep the way we had it available for a while, in case we change
    // our mind. (Whoever still sees this code in this state in the future please
    // feel free to clean up.)
    constexpr auto useGradient = false;
    return useGradient ? gc.CreateLinearGradientBrush(
        0, 0, 0, height, backgroundColor,
        GetColorMix(backgroundColor, *wxWHITE, 0.75))
           : gc.CreateBrush(backgroundColor);
}

wxRect GetPanelRect(const wxPanelWrapper& panel)
{
#ifndef __WXMAC__
    return panel.GetClientRect();
#else
    auto rect = panel.GetClientRect();
    rect.SetX(rect.GetX() + 1);
    rect.SetY(rect.GetY() + 1);
    rect.SetWidth(rect.GetWidth() - 1);
    rect.SetHeight(rect.GetHeight() - 1);
    return rect;
#endif
}
} // namespace DynamicRangeProcessorPanel
