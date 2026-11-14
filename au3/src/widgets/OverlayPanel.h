//
//  OverlayPanel.h
//  Audacity
//
//  Created by Paul Licameli on 5/1/16.
//
//

#ifndef __AUDACITY_OVERLAY_PANEL__
#define __AUDACITY_OVERLAY_PANEL__

#include <memory>
#include <vector>
#include "BackedPanel.h" // to inherit

class Overlay;

class AUDACITY_DLL_API OverlayPanel /* not final */ : public BackedPanel
{
public:
    OverlayPanel(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size,
                 // default as for wxPanel:
                 long style = wxTAB_TRAVERSAL | wxNO_BORDER);

    // Registers overlay objects.
    // The sequence in which they were registered is the sequence in
    // which they are painted.
    // OverlayPanel is not responsible for their memory management.
    void AddOverlay(const std::weak_ptr<Overlay>& pOverlay);
    void ClearOverlays();

    // Erases and redraws to the client area the overlays that have
    // been previously added with AddOverlay(). If "repaint_all" is
    // true, all overlays will be erased and re-drawn. Otherwise, only
    // the ones that are out-of-date, as well as the intersecting ones,
    // will be erased and re-drawn.
    // pDC can be null, in which case, DrawOverlays() will create a
    // wxClientDC internally when necessary.
    void DrawOverlays(bool repaint_all, wxDC* pDC = nullptr);

private:
    using OverlayPtr = std::weak_ptr<Overlay>;

    void Compress();
    std::vector< OverlayPtr > mOverlays;

    DECLARE_EVENT_TABLE()
    friend class GetInfoCommand;
};

/// Used to restore pen, brush and logical-op in a DC back to what they were.
struct AUDACITY_DLL_API DCUnchanger {
public:
    DCUnchanger() {}

    DCUnchanger(const wxBrush& brush_, const wxPen& pen_, long logicalOperation_)
        : brush(brush_), pen(pen_), logicalOperation(logicalOperation_)
    {}

    void operator ()(wxDC* pDC) const;

    wxBrush brush {};
    wxPen pen {};
    long logicalOperation {};
};

/// Makes temporary drawing context changes that you back out of, RAII style
//  It's like wxDCPenChanger, etc., but simple and general
class AUDACITY_DLL_API ADCChanger : public std::unique_ptr<wxDC, ::DCUnchanger>
{
    using Base = std::unique_ptr<wxDC, ::DCUnchanger>;
public:
    ADCChanger()
        : Base{} {}
    ADCChanger(wxDC* pDC);
};

#endif
