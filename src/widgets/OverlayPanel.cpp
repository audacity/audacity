//
//  OverlayPanel.cpp
//  Audacity
//
//  Created by Paul Licameli on 5/7/16.
//
//


#include "OverlayPanel.h"

#include "Overlay.h"
#include <algorithm>
#include <optional>
#include <wx/dcclient.h>

OverlayPanel::OverlayPanel(wxWindow * parent, wxWindowID id,
             const wxPoint & pos,
             const wxSize & size,
             long style)
: BackedPanel(parent, id, pos, size, style)
{}

void OverlayPanel::AddOverlay( const std::weak_ptr<Overlay> &pOverlay)
{
   if (pOverlay.expired())
      return;
   Compress();
   auto iter = std::lower_bound( mOverlays.begin(), mOverlays.end(),
      pOverlay.lock()->SequenceNumber(),
      []( const OverlayPtr &p, unsigned value ) {
         return p.expired() || p.lock()->SequenceNumber() < value;
      }
   );
   mOverlays.insert(iter, pOverlay);
}

void OverlayPanel::ClearOverlays()
{
   mOverlays.clear();
}

void OverlayPanel::EnqueueRepaintIfRequired(
   bool repaint_all, graphics::Painter& painter)
{
   if (!IsShownOnScreen())
      return;

   // First...
   Compress();
   // ... then assume pointers are not expired

   // Find out the rectangles and outdatedness for each overlay
   const wxSize size(GetSize());
   // See what requires redrawing.  If repainting, all.
   // If not, then whatever is outdated, and whatever will be damaged by
   // undrawing.
   // By redrawing only what needs it, we avoid flashing things like
   // the cursor that are drawn with invert, and also avoid
   // unnecessary work.

   // But first, a quick exit test.
   bool some_overlays_need_repainting =
      repaint_all ||
      std::any_of(
         mOverlays.begin(), mOverlays.end(),
         [&painter, size](const auto& overlay)
         { return overlay.lock()->GetRectangle(painter, size).second; });

   if (!some_overlays_need_repainting)
   {
      // This function (OverlayPanel::DrawOverlays()) is called at
      // fairly high frequency through a timer in TrackPanel. In case
      // there is nothing to do, we exit early because creating the
      // wxClientDC below is expensive, at least on Linux.
      return;
   }

   RequestRefresh();
}

void OverlayPanel::DrawOverlays(bool repaint_all, graphics::Painter& painter)
{
   if ( !IsShownOnScreen() )
      return;

   // First...
   Compress();
   // ... then assume pointers are not expired

   for (auto& weakOverlay : mOverlays)
      weakOverlay.lock()->Draw(*this, painter);
}

void OverlayPanel::Compress()
{
   // remove any expired pointers
   auto begin = mOverlays.begin();
   auto end = mOverlays.end();
   auto newEnd = std::remove_if( begin, end,
      []( const std::weak_ptr<Overlay> &pOverlay ){
         return pOverlay.expired(); } );
   if ( end != newEnd )
      mOverlays.resize( newEnd - begin );
}

BEGIN_EVENT_TABLE(OverlayPanel, BackedPanel)
END_EVENT_TABLE()

// Maybe this class needs a better home
void DCUnchanger::operator () (wxDC *pDC) const
{
   if (pDC) {
      pDC->SetPen(pen);
      pDC->SetBrush(brush);
      pDC->SetLogicalFunction(wxRasterOperationMode(logicalOperation));
   }
}

ADCChanger::ADCChanger(wxDC *pDC)
   : Base{ pDC, ::DCUnchanger{ pDC->GetBrush(), pDC->GetPen(),
      long(pDC->GetLogicalFunction()) } }
{}
