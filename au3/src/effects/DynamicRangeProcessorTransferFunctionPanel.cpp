/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorTransferFunctionPanel.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DynamicRangeProcessorTransferFunctionPanel.h"
#include "AColor.h"
#include "AllThemeResources.h"
#include "CompressorProcessor.h"
#include "DynamicRangeProcessorPanelCommon.h"
#include "Theme.h"
#include "widgets/LinearDBFormat.h"
#include "widgets/LinearUpdater.h"
#include "widgets/Ruler.h"
#include <cassert>
#include <wx/dcclient.h>
#include <wx/graphics.h>

BEGIN_EVENT_TABLE(DynamicRangeProcessorTransferFunctionPanel, wxPanelWrapper)
EVT_PAINT(DynamicRangeProcessorTransferFunctionPanel::OnPaint)
EVT_SIZE(DynamicRangeProcessorTransferFunctionPanel::OnSize)
END_EVENT_TABLE()

DynamicRangeProcessorTransferFunctionPanel::
DynamicRangeProcessorTransferFunctionPanel(
    wxWindow* parent, wxWindowID winid,
    const CompressorSettings& compressorSettings)
    : wxPanelWrapper{parent, winid}
    , mCompressorSettings{compressorSettings}
{
    SetDoubleBuffered(true);
}

namespace {
constexpr auto WidthPxToDb(double x, int width, double xPixelsPerDb)
{
    return (x + 1 - width) / xPixelsPerDb;
}

constexpr auto WidthDbToPx(double db, int width, double xPixelsPerDb)
{
    return width - 1 + db * xPixelsPerDb;
}

constexpr auto HeightDbToPx(double db, double yPixelsPerDb)
{
    return -db * yPixelsPerDb;
}

auto GetBrush(
    double kneeX, double kneeY, const wxColor& colorAtKnee, const wxSize& size,
    const wxGraphicsContext& gc)
{
    using namespace DynamicRangeProcessorPanel;
    constexpr auto w = .75;
    const wxColor edgeColor = GetColorMix(backgroundColor, colorAtKnee, w);

    const auto xf = size.GetWidth() / 2.; // "f" for "focus"
    const auto yf = size.GetHeight() / 2.;
    const auto radius = size.GetWidth();
    return gc.CreateRadialGradientBrush(
        kneeX, kneeY, xf, yf, radius, colorAtKnee, edgeColor);
}

void DrawTransferFunction(
    wxPaintDC& dc, const wxRect& rect, const CompressorSettings& settings)
{
    const auto X = rect.GetWidth();
    const auto Y = rect.GetHeight();
    const auto xPixelsPerDb
        =1.f * X / DynamicRangeProcessorTransferFunctionPanel::rangeDb;
    const auto yPixelsPerDb
        =1.f * Y / DynamicRangeProcessorTransferFunctionPanel::rangeDb;

    std::vector<wxPoint2DDouble> points;
    points.reserve(X);
    for (int x = rect.GetX(); x < X; ++x) {
        const auto db = WidthPxToDb(x, X, xPixelsPerDb);
        const auto y = HeightDbToPx(
            CompressorProcessor::EvaluateTransferFunction(settings, db),
            yPixelsPerDb);
        points.emplace_back(x, y);
    }

    using namespace DynamicRangeProcessorPanel;

    const auto gc = MakeGraphicsContext(dc);

    const auto kneeX = WidthDbToPx(settings.thresholdDb, X, xPixelsPerDb);
    const auto kneeY
        =HeightDbToPx(settings.thresholdDb + settings.makeupGainDb, yPixelsPerDb);

    // Fill the area above the curve
    wxGraphicsPath path = gc->CreatePath();
    path.MoveToPoint(X, 0);
    path.AddLineToPoint(0, 0);
    std::for_each(points.begin(), points.end(), [&path](const auto& point) {
        path.AddLineToPoint(point);
    });
    path.CloseSubpath();
    gc->SetBrush(GetBrush(kneeX, kneeY, attackColor, rect.GetSize(), *gc));
    gc->FillPath(path);

    // Fill the area below the curve
    path = gc->CreatePath();
    path.MoveToPoint(X, Y);
    path.AddLineToPoint(0, Y);
    std::for_each(points.begin(), points.end(), [&path](const auto& point) {
        path.AddLineToPoint(point);
    });
    path.CloseSubpath();
    gc->SetBrush(GetBrush(kneeX, kneeY, releaseColor, rect.GetSize(), *gc));
    gc->FillPath(path);

    // Draw the curve
    const auto gc2 = MakeGraphicsContext(dc);
    gc2->SetPen(wxPen { targetCompressionColor, targetCompressionLineWidth });
    gc2->DrawLines(points.size(), points.data());
}
} // namespace

void DynamicRangeProcessorTransferFunctionPanel::OnPaint(wxPaintEvent& evt)
{
    wxPaintDC dc(this);
    const auto rect = DynamicRangeProcessorPanel::GetPanelRect(*this);
    DrawTransferFunction(dc, rect, mCompressorSettings);
    dc.SetPen(DynamicRangeProcessorPanel::lineColor);
    dc.SetBrush(*wxTRANSPARENT_BRUSH);
    dc.DrawRectangle(rect);
}

void DynamicRangeProcessorTransferFunctionPanel::OnSize(wxSizeEvent& evt)
{
    Refresh();
}

bool DynamicRangeProcessorTransferFunctionPanel::AcceptsFocus() const
{
    return false;
}

bool DynamicRangeProcessorTransferFunctionPanel::AcceptsFocusFromKeyboard()
const
{
    return false;
}
