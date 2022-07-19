/**********************************************************************

  Audacity: A Digital Audio Editor

  RulerPanel.cpp

  Dominic Mazzoni

*******************************************************************//**

\class RulerPanel
\brief RulerPanel class allows you to work with a Ruler like
  any other wxWindow.

*//******************************************************************/

#include "RulerPanel.h"

#include "LinearUpdater.h"
#include "LogarithmicUpdater.h"

#include <wx/dcclient.h>

BEGIN_EVENT_TABLE(RulerPanel, wxPanelWrapper)
EVT_ERASE_BACKGROUND(RulerPanel::OnErase)
EVT_PAINT(RulerPanel::OnPaint)
EVT_SIZE(RulerPanel::OnSize)
END_EVENT_TABLE()

IMPLEMENT_CLASS(RulerPanel, wxPanelWrapper)

RulerPanel::RulerPanel(wxWindow* parent, wxWindowID id,
   wxOrientation orientation,
   const wxSize& bounds,
   const Range& range,
   RulerFormat format,
   const TranslatableString& units,
   const Options& options,
   const wxPoint& pos /*= wxDefaultPosition*/,
   const wxSize& size /*= wxDefaultSize*/) :
   wxPanelWrapper(parent, id, pos, size)
{
   ruler.SetBounds(0, 0, bounds.x, bounds.y);
   ruler.SetOrientation(orientation);
   ruler.SetRange(range.first, range.second);
   if (options.log)
      ruler.SetUpdater(std::make_unique<LogarithmicUpdater>());
   else
      ruler.SetUpdater(std::make_unique<LinearUpdater>());
   ruler.SetFormat(format);
   ruler.SetUnits(units);
   ruler.SetFlip(options.flip);
   ruler.SetLabelEdges(options.labelEdges);
   ruler.mbTicksAtExtremes = options.ticksAtExtremes;
   if (orientation == wxVERTICAL) {
      wxCoord w;
      ruler.GetMaxSize(&w, NULL);
      SetMinSize(wxSize(w, 150));  // height needed for wxGTK
   }
   else if (orientation == wxHORIZONTAL) {
      wxCoord h;
      ruler.GetMaxSize(NULL, &h);
      SetMinSize(wxSize(wxDefaultCoord, h));
   }
   if (options.hasTickColour)
      ruler.SetTickColour(options.tickColour);
}

RulerPanel::~RulerPanel()
{
}

void RulerPanel::OnErase(wxEraseEvent& WXUNUSED(evt))
{
   // Ignore it to prevent flashing
}

void RulerPanel::OnPaint(wxPaintEvent& WXUNUSED(evt))
{
   wxPaintDC dc(this);

#if defined(__WXMSW__)
   dc.Clear();
#endif

   ruler.Draw(dc);
}

void RulerPanel::OnSize(wxSizeEvent& WXUNUSED(evt))
{
   Refresh();
}

// LL:  We're overloading DoSetSize so that we can update the ruler bounds immediately
//      instead of waiting for a wxEVT_SIZE to come through.  This is needed by (at least)
//      FrequencyPlotDialog since it needs to have an updated ruler before RulerPanel gets the
//      size event.
void RulerPanel::DoSetSize(int x, int y,
   int width, int height,
   int sizeFlags)
{
   wxPanelWrapper::DoSetSize(x, y, width, height, sizeFlags);

   int w, h;
   GetClientSize(&w, &h);

   ruler.SetBounds(0, 0, w - 1, h - 1);
}
