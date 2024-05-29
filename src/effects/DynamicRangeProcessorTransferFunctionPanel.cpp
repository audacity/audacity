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
    : wxPanelWrapper { parent, winid }
    , mCompressorSettings { compressorSettings }
{
   SetDoubleBuffered(true);
}

namespace
{
void DrawTransferFunction(
   wxGraphicsContext& ctx, const wxSize& panelSize,
   const CompressorSettings& settings)
{
   const auto X = panelSize.x;
   const auto Y = panelSize.y;
   const auto xPixelsPerDb =
      1.f * X / DynamicRangeProcessorTransferFunctionPanel::rangeDb;
   const auto yPixelsPerDb =
      1.f * Y / DynamicRangeProcessorTransferFunctionPanel::rangeDb;

   std::vector<wxPoint2DDouble> points;
   points.reserve(X);
   for (int x = 0; x < X; ++x)
   {
      const auto db = 1.f * (x + 1 - X) / xPixelsPerDb;
      const int y = std::round(
         -CompressorProcessor::EvaluateTransferFunction(settings, db) *
         yPixelsPerDb);
      points.emplace_back(x, y);
   }

   wxGraphicsPath path = ctx.CreatePath();
   path.MoveToPoint(X, 0);
   path.AddLineToPoint(0, 0);
   std::for_each(
      points.begin(), points.end(),
      [&path](const wxPoint2DDouble& point) { path.AddLineToPoint(point); });
   path.CloseSubpath();
   ctx.SetBrush(wxBrush(DynamicRangeProcessorPanel::attackColor));
   ctx.FillPath(path);

   path = ctx.CreatePath();
   path.MoveToPoint(X, Y);
   path.AddLineToPoint(0, Y);
   std::for_each(
      points.begin(), points.end(),
      [&path](const wxPoint2DDouble& point) { path.AddLineToPoint(point); });
   path.CloseSubpath();
   ctx.SetBrush(wxBrush(DynamicRangeProcessorPanel::releaseColor));
   ctx.FillPath(path);
}
} // namespace

void DynamicRangeProcessorTransferFunctionPanel::OnPaint(wxPaintEvent& evt)
{
   wxPaintDC dc(this);
   dc.Clear();

   dc.SetBrush(DynamicRangeProcessorPanel::backgroundColor);
   dc.SetPen(wxPen(*wxBLACK));
   dc.DrawRectangle(GetSize());

   std::unique_ptr<wxGraphicsContext> gc {
      DynamicRangeProcessorPanel::MakeGraphicsContext(dc)
   };
   DrawTransferFunction(*gc, GetSize(), mCompressorSettings);
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
