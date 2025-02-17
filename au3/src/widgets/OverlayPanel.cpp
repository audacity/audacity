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

OverlayPanel::OverlayPanel(wxWindow* parent, wxWindowID id,
                           const wxPoint& pos,
                           const wxSize& size,
                           long style)
    : BackedPanel(parent, id, pos, size, style)
{}

void OverlayPanel::AddOverlay(const std::weak_ptr<Overlay>& pOverlay)
{
    if (pOverlay.expired()) {
        return;
    }
    Compress();
    auto iter = std::lower_bound(mOverlays.begin(), mOverlays.end(),
                                 pOverlay.lock()->SequenceNumber(),
                                 []( const OverlayPtr& p, unsigned value ) {
        return p.expired() || p.lock()->SequenceNumber() < value;
    }
                                 );
    mOverlays.insert(iter, pOverlay);
}

void OverlayPanel::ClearOverlays()
{
    mOverlays.clear();
}

void OverlayPanel::DrawOverlays(bool repaint_all, wxDC* pDC)
{
    if (!IsShownOnScreen()) {
        return;
    }

    size_t n_pairs = mOverlays.size();

    using Pair = std::pair<wxRect, bool /*out of date?*/>;
    std::vector< Pair > pairs;
    pairs.reserve(n_pairs);

    // First...
    Compress();
    // ... then assume pointers are not expired

    // Find out the rectangles and outdatedness for each overlay
    wxSize size(GetBackingDC().GetSize());
    for (const auto& pOverlay : mOverlays) {
        pairs.push_back(pOverlay.lock()->GetRectangle(size));
    }

    // See what requires redrawing.  If repainting, all.
    // If not, then whatever is outdated, and whatever will be damaged by
    // undrawing.
    // By redrawing only what needs it, we avoid flashing things like
    // the cursor that are drawn with invert, and also avoid
    // unnecessary work.

    // But first, a quick exit test.
    bool some_overlays_need_repainting
        =repaint_all
          || std::any_of(pairs.begin(), pairs.end(),
                         []( const Pair& pair ){ return pair.second; });

    if (!some_overlays_need_repainting) {
        // This function (OverlayPanel::DrawOverlays()) is called at
        // fairly high frequency through a timer in TrackPanel. In case
        // there is nothing to do, we exit early because creating the
        // wxClientDC below is expensive, at least on Linux.
        return;
    }

    if (!repaint_all) {
        // For each overlay that needs update, any other overlay whose
        // rectangle intersects it will also need update.
        bool done;
        do {
            done = true;
            for (size_t ii = 0; ii < n_pairs - 1; ++ii) {
                for (size_t jj = ii + 1; jj < n_pairs; ++jj) {
                    if (pairs[ii].second != pairs[jj].second
                        && pairs[ii].first.Intersects(pairs[jj].first)) {
                        done = false;
                        pairs[ii].second = pairs[jj].second = true;
                    }
                }
            }
        } while (!done);
    }

    std::optional<wxClientDC> myDC;
    auto& dc = pDC ? *pDC : (myDC.emplace(this), *myDC);

    // Erase
    auto it2 = pairs.begin();
    for (auto pOverlay : mOverlays) {
        if (repaint_all || it2->second) {
            pOverlay.lock()->Erase(dc, GetBackingDC());
        }
        ++it2;
    }

    // Draw
    it2 = pairs.begin();
    for (auto pOverlay : mOverlays) {
        if (repaint_all || it2->second) {
            // Guarantee a clean state of the dc each pass:
            ADCChanger changer{ &dc };

            pOverlay.lock()->Draw(*this, dc);
        }
        ++it2;
    }
}

void OverlayPanel::Compress()
{
    // remove any expired pointers
    auto begin = mOverlays.begin();
    auto end = mOverlays.end();
    auto newEnd = std::remove_if(begin, end,
                                 []( const std::weak_ptr<Overlay>& pOverlay ){
        return pOverlay.expired();
    });
    if (end != newEnd) {
        mOverlays.resize(newEnd - begin);
    }
}

BEGIN_EVENT_TABLE(OverlayPanel, BackedPanel)
END_EVENT_TABLE()

// Maybe this class needs a better home
void DCUnchanger::operator ()(wxDC* pDC) const
{
    if (pDC) {
        pDC->SetPen(pen);
        pDC->SetBrush(brush);
        pDC->SetLogicalFunction(wxRasterOperationMode(logicalOperation));
    }
}

ADCChanger::ADCChanger(wxDC* pDC)
    : Base{pDC, ::DCUnchanger { pDC->GetBrush(), pDC->GetPen(),
                                long(pDC->GetLogicalFunction()) }}
{}
