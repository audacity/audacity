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
template <typename V, typename Ctx>
void DrawTransferFunction(
   Ctx& ctx, const wxSize& panelSize, const CompressorSettings& settings)
{
   const auto X = panelSize.x;
   const auto Y = panelSize.y;
   const auto xPixelsPerDb =
      1.f * X / DynamicRangeProcessorTransferFunctionPanel::rangeDb;
   const auto yPixelsPerDb =
      1.f * Y / DynamicRangeProcessorTransferFunctionPanel::rangeDb;

   ctx.SetPen(wxPen(
      theTheme.Colour(clrGraphLines),
      DynamicRangeProcessorPanel::transferFunctionLineWidth));
   std::vector<V> points;
   points.reserve(X);
   for (int x = 0; x < X; ++x)
   {
      const auto db = 1.f * (x + 1 - X) / xPixelsPerDb;
      const int y = std::round(
         -CompressorProcessor::EvaluateTransferFunction(settings, db) *
         yPixelsPerDb);
      points.emplace_back(x, y);
   }
   ctx.DrawLines(X, points.data());
}
} // namespace

void DynamicRangeProcessorTransferFunctionPanel::OnPaint(wxPaintEvent& evt)
{
   wxPaintDC dc(this);
   dc.Clear();

   dc.SetBrush(*wxWHITE_BRUSH);
   dc.SetPen(wxPen(*wxBLACK));
   dc.DrawRectangle(GetSize());

   // If a graphics context is available, use it for anti-aliasing.
   if (std::unique_ptr<wxGraphicsContext> gc { wxGraphicsContext::Create(dc) })
   {
      gc->SetAntialiasMode(wxANTIALIAS_DEFAULT);
      DrawTransferFunction<wxPoint2DDouble>(
         *gc, GetSize(), mCompressorSettings);
   }
   else
      DrawTransferFunction<wxPoint>(dc, GetSize(), mCompressorSettings);
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
