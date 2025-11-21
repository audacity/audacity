/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.cpp

  Dominic Mazzoni


********************************************************************//**

\class AColor
\brief AColor Manages color brushes and pens

It is also a place to document colour usage policy in Audacity

*//********************************************************************/

#include "AColor.h"
#include "AColorResources.h"

#include <wx/colour.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/graphics.h>
#include <wx/settings.h>
#include <wx/utils.h>

#include "AllThemeResources.h"
#include "Theme.h"

bool AColor::inited = false;
wxBrush AColor::lightBrush[2];
wxBrush AColor::mediumBrush[2];
wxBrush AColor::darkBrush[2];
wxPen AColor::lightPen[2];
wxPen AColor::mediumPen[2];
wxPen AColor::darkPen[2];

wxPen AColor::cursorPen;
wxBrush AColor::indicatorBrush[2];
wxPen AColor::indicatorPen[2];
// wxPen AColor::playRegionPen[2];
wxBrush AColor::playRegionBrush[1];

wxBrush AColor::muteBrush[2];
wxBrush AColor::soloBrush;

wxPen AColor::clippingPen;

wxBrush AColor::envelopeBrush;
wxPen AColor::envelopePen;
wxPen AColor::WideEnvelopePen;

wxBrush AColor::labelTextNormalBrush;
wxBrush AColor::labelTextEditBrush;
wxBrush AColor::labelUnselectedBrush;
wxBrush AColor::labelSelectedBrush;
wxBrush AColor::labelSyncLockSelBrush;
wxPen AColor::labelSyncLockSelPen;
wxPen AColor::labelSurroundPen;
wxPen AColor::trackFocusPens[3];
wxPen AColor::snapGuidePen;

wxPen AColor::tooltipPen;
wxBrush AColor::tooltipBrush;

// The spare pen and brush possibly help us cut down on the
// number of pens and brushes we need.
wxPen AColor::sparePen;
wxBrush AColor::spareBrush;

wxPen AColor::uglyPen;
wxBrush AColor::uglyBrush;

namespace {
//Simplified variation of nine patch scale drawing
//https://en.wikipedia.org/wiki/9-slice_scaling
//Bitmap and rect expected to have at least 3px in both directions
void DrawNinePatch(wxDC& dc, wxBitmap& bitmap, const wxRect& r, int mid = 1)
{
    assert(bitmap.GetWidth() == mid + (bitmap.GetWidth() - mid) / 2 + (bitmap.GetWidth() - mid) / 2);
    assert(bitmap.GetHeight() == mid + (bitmap.GetHeight() - mid) / 2 + (bitmap.GetHeight() - mid) / 2);
    assert(r.width >= bitmap.GetWidth() - mid);
    assert(r.height >= bitmap.GetHeight() - mid);
    wxMemoryDC memDC;
    memDC.SelectObject(bitmap);

    // image slices

    const auto uw0 = (bitmap.GetWidth() - mid) / 2;
    const auto uw1 = mid;
    const auto uw2 = bitmap.GetWidth() - uw0 - uw1;

    const auto vh0 = (bitmap.GetHeight() - mid) / 2;
    const auto vh1 = mid;
    const auto vh2 = bitmap.GetHeight() - vh1 - vh0;

    const auto u0 = 0;
    const auto u1 = uw0;
    const auto u2 = uw0 + uw1;

    const auto v0 = 0;
    const auto v1 = vh0;
    const auto v2 = vh0 + vh1;

    //Button geometry

    const auto xw0 = std::min(uw0, r.width / 2);
    const auto xw2 = std::min(uw2, r.width / 2);
    const auto xw1 = std::max(r.width - xw0 - xw2, 0);

    const auto yh0 = std::min(vh0, r.height / 2);
    const auto yh2 = std::min(vh2, r.height / 2);
    const auto yh1 = std::max(r.height - yh0 - yh2, 0);

    const auto x0 = r.x;
    const auto x1 = r.x + xw0;
    const auto x2 = r.x + xw0 + xw1;

    const auto y0 = r.y;
    const auto y1 = r.y + yh0;
    const auto y2 = r.y + yh0 + yh1;

    //Draw corners
    dc.StretchBlit(x0, y0, xw0, yh0, &memDC, u0, v0, uw0, vh0, wxCOPY, true);
    dc.StretchBlit(x2, y0, xw2, yh0, &memDC, u2, v0, uw2, vh0, wxCOPY, true);
    dc.StretchBlit(x0, y2, xw0, yh2, &memDC, u0, v2, uw0, vh2, wxCOPY, true);
    dc.StretchBlit(x2, y2, xw2, yh2, &memDC, u2, v2, uw2, vh2, wxCOPY, true);

    if (xw1 > 0 && yh1 > 0) {//Draw middle
        dc.StretchBlit(x1, y0, xw1, yh0, &memDC, u1, v0, uw1, vh0, wxCOPY, true);

        dc.StretchBlit(x0, y1, xw0, yh1, &memDC, u0, v1, uw0, vh1, wxCOPY, true);
        dc.StretchBlit(x1, y1, xw1, yh1, &memDC, u1, v1, uw1, vh1, wxCOPY, true);
        dc.StretchBlit(x2, y1, xw2, yh1, &memDC, u2, v1, uw2, vh1, wxCOPY, true);

        dc.StretchBlit(x1, y2, xw1, yh2, &memDC, u1, v2, uw1, vh2, wxCOPY, true);
    }
}

int GetButtonImageIndex(bool up, bool selected, bool highlight)
{
    // There are eight button states in the TCP.
    // A theme might not differentiate among them all.  That's up to
    // the theme designer.
    //   Button highlighted (i.e. hovered over) or not.
    //   Track selected or not
    //   Button up or down.
    // Highlight in most themes is lighter than not highlighted.
    if (highlight && selected) {
        return up ? bmpHiliteUpButtonExpandSel : bmpHiliteButtonExpandSel;
    }
    if (highlight) {
        return up ? bmpHiliteUpButtonExpand : bmpHiliteButtonExpand;
    }
    if (selected) {
        return up ? bmpUpButtonExpandSel : bmpDownButtonExpandSel;
    }
    return up ? bmpUpButtonExpand : bmpDownButtonExpand;
}
}

//
// Draw an upward or downward pointing arrow.
//
void AColor::Arrow(wxDC& dc, wxCoord x, wxCoord y, int width, bool down)
{
    if (width & 0x01) {
        width--;
    }

    wxPoint pt[3];
    int half = width / 2;

    if (down) {
        pt[0].x =     0;
        pt[0].y = 0;
        pt[1].x = width;
        pt[1].y = 0;
        pt[2].x =  half;
        pt[2].y = half;
    } else {
        pt[0].x =     0;
        pt[0].y = half;
        pt[1].x =  half;
        pt[1].y = 0;
        pt[2].x = width;
        pt[2].y = half;
    }

    dc.DrawPolygon(3, pt, x, y);
}

//
// Draw a line, inclusive of endpoints,
// compensating for differences in wxWidgets versions across platforms
//
void AColor::Line(wxDC& dc, wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2)
{
    const wxPoint points[] { { x1, y1 }, { x2, y2 } };
    Lines(dc, 2, points);
}

void AColor::Line(wxDC& dc, const wxPoint& from, const wxPoint& to)
{
    Line(dc, from.x, from.y, to.x, to.y);
}

// Draw lines, INCLUSIVE of all endpoints
void AColor::Lines(wxDC& dc, size_t nPoints, const wxPoint points[])
{
    if (nPoints <= 1) {
        if (nPoints == 1) {
            dc.DrawPoint(points[0]);
        }
        return;
    }

    for (size_t ii = 0; ii < nPoints - 1; ++ii) {
        const auto& p1 = points[ii];
        const auto& p2 = points[ii + 1];

        // As of 2.8.9 (possibly earlier), wxDC::DrawLine() on the Mac draws the
        // last point since it is now based on the NEW wxGraphicsContext system.
        // Make the other platforms do the same thing since the other platforms
        // "may" follow they get wxGraphicsContext going.

        // PRL:  as of 3.1.1, I still observe that on Mac, the last point is
        // included, contrary to what documentation says.  Also that on Windows,
        // sometimes it is the first point that is excluded.

#if defined(__WXMAC__) || defined(__WXGTK3__)
        dc.DrawLine(p1, p2);
#else
        dc.DrawPoint(p1);
        if (p1 != p2) {
            dc.DrawLine(p1, p2);
        }
#endif
    }

#if defined(__WXMAC__) || defined(__WXGTK3__)
    ;
#else
    dc.DrawPoint(points[ nPoints - 1 ]);
#endif
}

//
// Draws a focus rectangle (Taken directly from wxWidgets source)
//
void AColor::DrawFocus(wxDC& dc, wxRect& rect)
{
    // draw the pixels manually: note that to behave in the same manner as
    // DrawRect(), we must exclude the bottom and right borders from the
    // rectangle
    wxCoord x1 = rect.GetLeft(),
            y1 = rect.GetTop(),
            x2 = rect.GetRight(),
            y2 = rect.GetBottom();

    // -1 for brush, so it just sets the pen colour, and does not change the brush.
    UseThemeColour(&dc, -1, clrTrackPanelText);

    wxCoord z;
    for ( z = x1 + 1; z < x2; z += 2 ) {
        dc.DrawPoint(z, y1);
    }

    wxCoord shift = z == x2 ? 0 : 1;
    for ( z = y1 + shift; z < y2; z += 2 ) {
        dc.DrawPoint(x2, z);
    }

    shift = z == y2 ? 0 : 1;
    for ( z = x2 - shift; z > x1; z -= 2 ) {
        dc.DrawPoint(z, y2);
    }

    shift = z == x1 ? 0 : 1;
    for ( z = y2 - shift; z > y1; z -= 2 ) {
        dc.DrawPoint(x1, z);
    }
}

void AColor::Bevel(wxDC& dc, bool up, const wxRect& r)
{
    AColor::Dark(&dc, false);

    AColor::Line(dc, r.x, r.y, r.x + r.width, r.y);
    AColor::Line(dc, r.x, r.y, r.x, r.y + r.height);

    AColor::Line(dc, r.x + r.width, r.y, r.x + r.width, r.y + r.height);
    AColor::Line(dc, r.x, r.y + r.height, r.x + r.width, r.y + r.height);
}

void AColor::ButtonStretch(wxDC& dc, bool up, const wxRect& r, bool selected, bool highlight)
{
    DrawNinePatch(
        dc,
        theTheme.Bitmap(GetButtonImageIndex(up, selected, highlight)),
        r, 2
        );
}

void AColor::Bevel2(wxDC& dc, bool up, const wxRect& r, bool bSel, bool bHighlight)
{
    auto& Bmp = theTheme.Bitmap(GetButtonImageIndex(up, bSel, bHighlight));
    wxMemoryDC memDC;
    memDC.SelectObject(Bmp);

    int h = std::min(r.height, Bmp.GetHeight());

    dc.Blit(r.x, r.y, r.width / 2, h, &memDC, 0, 0, wxCOPY, true);
    int r2 = r.width - r.width / 2;
    dc.Blit(r.x + r.width / 2, r.y, r2, h, &memDC,
            Bmp.GetWidth() - r2, 0, wxCOPY, true);
}

void AColor::DrawHStretch(wxDC& dc, const wxRect& rect, wxBitmap& bitmap)
{
    wxMemoryDC srcDC;
    srcDC.SelectObject(bitmap);

    const auto sh = bitmap.GetHeight();
    const auto dh = rect.height;
    const auto w0 = std::min((bitmap.GetWidth() - 1) / 2, rect.width / 2);
    const auto dx0 = rect.x;
    const auto dx1 = rect.x + w0;
    const auto dx2 = rect.x + rect.width - w0;

    dc.StretchBlit(dx0, rect.y, w0, dh, &srcDC, 0, 0, w0, sh, wxCOPY, true);
    dc.StretchBlit(dx1, rect.y, rect.width - w0 * 2, dh, &srcDC, w0, 0, 1, sh, wxCOPY, true);
    dc.StretchBlit(dx2, rect.y, w0, dh, &srcDC, bitmap.GetWidth() - w0, 0, w0, sh, wxCOPY, true);
}

void AColor::DrawFrame(wxDC& dc, const wxRect& r, wxBitmap& bitmap, int mid = 1)
{
    DrawNinePatch(dc, bitmap, r, mid);
}

wxColour AColor::Blend(const wxColour& c1, const wxColour& c2)
{
    wxColour c3(
        (c1.Red() + c2.Red()) / 2,
        (c1.Green() + c2.Green()) / 2,
        (c1.Blue() + c2.Blue()) / 2);
    return c3;
}

void AColor::BevelTrackInfo(wxDC& dc, bool up, const wxRect& r, bool highlight)
{
    Bevel(dc, up, r);
}

// Set colour of and select brush and pen.
// Use -1 to omit brush or pen.
// If pen omitted, then the same colour as the brush will be used.
// alpha for the brush is normally 255, but if set will make a difference
// on mac (only) currently.
void AColor::UseThemeColour(wxDC* dc, int iBrush, int iPen, int alpha)
{
    if (!inited) {
        Init();
    }
    // do nothing if no colours set.
    if ((iBrush == -1) && (iPen == -1)) {
        return;
    }
    wxColour col = wxColour(0, 0, 0);
    if (iBrush != -1) {
        col = theTheme.Colour(iBrush);
        col.Set(col.Red(), col.Green(), col.Blue(), alpha);
        spareBrush.SetColour(col);
        dc->SetBrush(spareBrush);
    }
    if (iPen != -1) {
        col = theTheme.Colour(iPen);
    }
    sparePen.SetColour(col);
    dc->SetPen(sparePen);
}

void AColor::UseThemeColour(wxGraphicsContext* gc, int iBrush, int iPen, int alpha)
{
    if (!inited) {
        Init();
    }
    // do nothing if no colours set.
    if ((iBrush == -1) && (iPen == -1)) {
        return;
    }
    wxColour col = wxColour(0, 0, 0);
    if (iBrush != -1) {
        col = theTheme.Colour(iBrush);
        col.Set(col.Red(), col.Green(), col.Blue(), alpha);
        spareBrush.SetColour(col);
        gc->SetBrush(spareBrush);
    }
    if (iPen != -1) {
        col = theTheme.Colour(iPen);
    }
    sparePen.SetColour(col);
    gc->SetPen(sparePen);
}

void AColor::Light(wxDC* dc, bool selected, bool highlight)
{
    if (!inited) {
        Init();
    }
    int index = (int)selected;
    auto& brush = highlight ? AColor::uglyBrush : lightBrush[index];
    dc->SetBrush(brush);
    auto& pen = highlight ? AColor::uglyPen : lightPen[index];
    dc->SetPen(pen);
}

void AColor::Medium(wxDC* dc, bool selected)
{
    if (!inited) {
        Init();
    }
    int index = (int)selected;
    dc->SetBrush(mediumBrush[index]);
    dc->SetPen(mediumPen[index]);
}

void AColor::MediumTrackInfo(wxDC* dc, bool selected)
{
    UseThemeColour(dc, selected ? clrTrackInfoSelected : clrTrackInfo);
}

void AColor::Dark(wxDC* dc, bool selected, bool highlight)
{
    if (!inited) {
        Init();
    }
    int index = (int)selected;
    auto& brush = highlight ? AColor::uglyBrush : darkBrush[index];
    dc->SetBrush(brush);
    auto& pen = highlight ? AColor::uglyPen : darkPen[index];
    dc->SetPen(pen);
}

void AColor::TrackPanelBackground(wxDC* dc, bool selected)
{
    UseThemeColour(dc, selected ? clrMediumSelected : clrTrackBackground);
}

void AColor::CursorColor(wxDC* dc)
{
    if (!inited) {
        Init();
    }

    dc->SetLogicalFunction(wxCOPY);
    dc->SetPen(cursorPen);
}

void AColor::IndicatorColor(wxDC* dc, bool bIsNotRecording)
{
    if (!inited) {
        Init();
    }
    int index = (int)bIsNotRecording;
    dc->SetPen(indicatorPen[index]);
    dc->SetBrush(indicatorBrush[index]);
}

void AColor::TrackFocusPen(wxDC* dc, int level)
{
    if (!inited) {
        Init();
    }
    dc->SetPen(trackFocusPens[level]);
}

void AColor::SnapGuidePen(wxDC* dc)
{
    if (!inited) {
        Init();
    }
    dc->SetPen(snapGuidePen);
}

void AColor::Mute(wxDC* dc, bool on, bool selected, bool soloing)
{
    if (!inited) {
        Init();
    }
    int index = (int)selected;
    if (on) {
        dc->SetPen(*wxBLACK_PEN);
        dc->SetBrush(muteBrush[(int)soloing]);
    } else {
        dc->SetPen(*wxTRANSPARENT_PEN);
        dc->SetBrush(mediumBrush[index]);
    }
}

void AColor::Solo(wxDC* dc, bool on, bool selected)
{
    if (!inited) {
        Init();
    }
    int index = (int)selected;
    if (on) {
        dc->SetPen(*wxBLACK_PEN);
        dc->SetBrush(soloBrush);
    } else {
        dc->SetPen(*wxTRANSPARENT_PEN);
        dc->SetBrush(mediumBrush[index]);
    }
}

bool AColor::gradient_inited = 0;

void AColor::ReInit()
{
    inited=false;
    Init();
    gradient_inited=0;
    PreComputeGradient();
}

wxColour InvertOfColour(const wxColour& c)
{
    return wxColour(255 - c.Red(), 255 - c.Green(), 255 - c.Blue());
}

// Fix up the cursor colour, if it is 'unacceptable'.
// Unacceptable if it is too close to the background colour.
wxColour CursorColour()
{
    wxColour cCursor = theTheme.Colour(clrCursorPen);
    wxColour cBack = theTheme.Colour(clrMedium);

    int d = theTheme.ColourDistance(cCursor, cBack);

    // Pen colour is fine, if there is plenty of contrast.
    if (d > 200) {
        return theTheme.Colour(clrCursorPen);
    }

    // otherwise return same colour as a selection.
    return theTheme.Colour(clrSelected);
}

void AColor::Init()
{
    if (inited) {
        return;
    }

    wxColour light = theTheme.Colour(clrLight);
    // wxSystemSettings::GetColour(wxSYS_COLOUR_3DHIGHLIGHT);
    wxColour med = theTheme.Colour(clrMedium);
    // wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE);
    wxColour dark = theTheme.Colour(clrDark);
    // wxSystemSettings::GetColour(wxSYS_COLOUR_3DSHADOW);

    wxColour lightSelected = theTheme.Colour(clrLightSelected);
    wxColour medSelected = theTheme.Colour(clrMediumSelected);
    wxColour darkSelected = theTheme.Colour(clrDarkSelected);

    clippingPen.SetColour(0xCC, 0x11, 0x00);

    theTheme.SetPenColour(envelopePen,     clrEnvelope);
    theTheme.SetPenColour(WideEnvelopePen, clrEnvelope);
    theTheme.SetBrushColour(envelopeBrush,   clrEnvelope);

    WideEnvelopePen.SetWidth(3);

    theTheme.SetBrushColour(labelTextNormalBrush,  clrLabelTextNormalBrush);
    theTheme.SetBrushColour(labelTextEditBrush,    clrLabelTextEditBrush);
    theTheme.SetBrushColour(labelUnselectedBrush,  clrLabelUnselectedBrush);
    theTheme.SetBrushColour(labelSelectedBrush,    clrLabelSelectedBrush);
    theTheme.SetBrushColour(labelSyncLockSelBrush, clrSyncLockSel);
    theTheme.SetPenColour(labelSyncLockSelPen,  clrSyncLockSel);
    theTheme.SetPenColour(labelSurroundPen,     clrLabelSurroundPen);

    // These colors were modified to avoid using reserved colors red and green
    // for the buttons.
    theTheme.SetBrushColour(muteBrush[0],      clrMuteButtonActive);
    theTheme.SetBrushColour(muteBrush[1],      clrMuteButtonVetoed);
    theTheme.SetBrushColour(soloBrush,         clrMuteButtonActive);

    cursorPen.SetColour(CursorColour());
    theTheme.SetPenColour(indicatorPen[0],   clrRecordingPen);
    theTheme.SetPenColour(indicatorPen[1],   clrPlaybackPen);
    theTheme.SetBrushColour(indicatorBrush[0], clrRecordingBrush);
    theTheme.SetBrushColour(indicatorBrush[1], clrPlaybackBrush);

    theTheme.SetBrushColour(playRegionBrush[0], clrRulerRecordingBrush);

    //Determine tooltip color
    tooltipPen.SetColour(wxSystemSettingsNative::GetColour(wxSYS_COLOUR_INFOTEXT));
    tooltipBrush.SetColour(wxSystemSettingsNative::GetColour(wxSYS_COLOUR_INFOBK));

    uglyPen.SetColour(wxColour { 0, 255, 0 }); // saturated green
    uglyBrush.SetColour(wxColour { 255, 0, 255 }); // saturated magenta

    // A tiny gradient of yellow surrounding the current focused track
    theTheme.SetPenColour(trackFocusPens[0],  clrTrackFocus0);
    theTheme.SetPenColour(trackFocusPens[1],  clrTrackFocus1);
    theTheme.SetPenColour(trackFocusPens[2],  clrTrackFocus2);

    // A vertical line indicating that the selection or sliding has
    // been snapped to the nearest boundary.
    theTheme.SetPenColour(snapGuidePen,      clrSnapGuide);

    // unselected
    lightBrush[0].SetColour(light);
    mediumBrush[0].SetColour(med);
    darkBrush[0].SetColour(dark);
    lightPen[0].SetColour(light);
    mediumPen[0].SetColour(med);
    darkPen[0].SetColour(dark);

    // selected
    lightBrush[1].SetColour(lightSelected);
    mediumBrush[1].SetColour(medSelected);
    darkBrush[1].SetColour(darkSelected);
    lightPen[1].SetColour(lightSelected);
    mediumPen[1].SetColour(medSelected);
    darkPen[1].SetColour(darkSelected);

    inited = true;
}

// These colours are chosen so that black text shows up OK on them.
const int AColor_midicolors[16][3] = {
    { 255, 102, 102 },          // 1=salmon
    { 204, 0, 0 },              // 2=red
    { 255, 117, 23 },           // 3=orange
    { 255, 255, 0 },            // 4=yellow
    { 0, 204, 0 },              // 5=green
    { 0, 204, 204 },            // 6=turquoise
    { 125, 125, 255 },          // 7=blue
    { 153, 0, 255 },            // 8=blue-violet

    { 140, 97, 54 },            // 9=brown
    { 120, 120, 120 },          // 10=gray (drums)
    { 255, 175, 40 },           // 11=lt orange
    { 102, 255, 102 },          // 12=lt green
    { 153, 255, 255 },          // 13=lt turquoise
    { 190, 190, 255 },          // 14=lt blue
    { 204, 102, 255 },          // 15=lt blue-violet
    { 255, 51, 204 }            // 16=lt red-violet
};

void AColor::MIDIChannel(wxDC* dc, int channel /* 1 - 16 */)
{
    if (channel >= 1 && channel <= 16) {
        const int* colors = AColor_midicolors[channel - 1];

        dc->SetPen(wxPen(wxColour(colors[0],
                                  colors[1], colors[2]), 1, wxPENSTYLE_SOLID));
        dc->SetBrush(wxBrush(wxColour(colors[0],
                                      colors[1], colors[2]), wxBRUSHSTYLE_SOLID));
    } else {
        dc->SetPen(wxPen(wxColour(153, 153, 153), 1, wxPENSTYLE_SOLID));
        dc->SetBrush(wxBrush(wxColour(153, 153, 153), wxBRUSHSTYLE_SOLID));
    }
}

void AColor::LightMIDIChannel(wxDC* dc, int channel /* 1 - 16 */)
{
    if (channel >= 1 && channel <= 16) {
        const int* colors = AColor_midicolors[channel - 1];

        dc->SetPen(wxPen(wxColour(127 + colors[0] / 2,
                                  127 + colors[1] / 2,
                                  127 + colors[2] / 2), 1, wxPENSTYLE_SOLID));
        dc->SetBrush(wxBrush(wxColour(127 + colors[0] / 2,
                                      127 + colors[1] / 2,
                                      127 + colors[2] / 2), wxBRUSHSTYLE_SOLID));
    } else {
        dc->SetPen(wxPen(wxColour(204, 204, 204), 1, wxPENSTYLE_SOLID));
        dc->SetBrush(wxBrush(wxColour(204, 204, 204), wxBRUSHSTYLE_SOLID));
    }
}

void AColor::DarkMIDIChannel(wxDC* dc, int channel /* 1 - 16 */)
{
    if (channel >= 1 && channel <= 16) {
        const int* colors = AColor_midicolors[channel - 1];

        dc->SetPen(wxPen(wxColour(colors[0] / 2,
                                  colors[1] / 2,
                                  colors[2] / 2), 1, wxPENSTYLE_SOLID));
        dc->SetBrush(wxBrush(wxColour(colors[0] / 2,
                                      colors[1] / 2,
                                      colors[2] / 2), wxBRUSHSTYLE_SOLID));
    } else {
        dc->SetPen(wxPen(wxColour(102, 102, 102), 1, wxPENSTYLE_SOLID));
        dc->SetBrush(wxBrush(wxColour(102, 102, 102), wxBRUSHSTYLE_SOLID));
    }
}

unsigned char AColor::gradient_pre[ColorGradientTotal][colorSchemes][gradientSteps][3];

void AColor::PreComputeGradient()
{
    if (gradient_inited) {
        return;
    }
    gradient_inited = 1;

    // Keep in correspondence with enum SpectrogramSettings::ColorScheme

    // colorScheme 0: Color (New)
    std::copy_n(&specColormap[0][0], gradientSteps * 3, &gradient_pre[ColorGradientUnselected][0][0][0]);
    std::copy_n(&selColormap[0][0], gradientSteps * 3, &gradient_pre[ColorGradientTimeSelected][0][0][0]);
    std::copy_n(&freqSelColormap[0][0], gradientSteps * 3, &gradient_pre[ColorGradientTimeAndFrequencySelected][0][0][0]);
    std::fill_n(&gradient_pre[ColorGradientEdge][0][0][0], gradientSteps * 3, 0);

    for (int selected = 0; selected < ColorGradientTotal; selected++) {
        // Get color scheme from Theme
        const int gsteps = 4;
        float gradient[gsteps + 1][3];
        theTheme.Colour(clrSpectro1) = theTheme.Colour(clrUnselected);
        theTheme.Colour(clrSpectro1Sel) = theTheme.Colour(clrSelected);
        int clrFirst = (selected == ColorGradientUnselected) ? clrSpectro1 : clrSpectro1Sel;
        for (int j=0; j < (gsteps + 1); j++) {
            wxColour c = theTheme.Colour(clrFirst + j);
            gradient[ j] [0] = c.Red() / 255.0;
            gradient[ j] [1] = c.Green() / 255.0;
            gradient[ j] [2] = c.Blue() / 255.0;
        }

        // colorScheme 1: Color (from theme)
        for (int i = 0; i < gradientSteps; i++) {
            float r, g, b;
            float value = float(i) / gradientSteps;

            int left = (int)(value * gsteps);
            int right = (left == gsteps ? gsteps : left + 1);

            float rweight = (value * gsteps) - left;
            float lweight = 1.0 - rweight;

            r = (gradient[left][0] * lweight) + (gradient[right][0] * rweight);
            g = (gradient[left][1] * lweight) + (gradient[right][1] * rweight);
            b = (gradient[left][2] * lweight) + (gradient[right][2] * rweight);

            switch (selected) {
            case ColorGradientUnselected:
                // not dimmed
                break;

            case ColorGradientTimeAndFrequencySelected:
                float temp;
                temp = r;
                r = g;
                g = b;
                b = temp;
                break;

            case ColorGradientTimeSelected:
                // partly dimmed
                r *= 0.75f;
                g *= 0.75f;
                b *= 0.75f;
                break;

            // For now edge colour is just black (or white if grey-scale)
            // Later we might invert or something else funky.
            case ColorGradientEdge:
                // fully dimmed
                r = 0;
                g = 0;
                b = 0;
                break;
            }
            gradient_pre[selected][1][i][0] = (unsigned char)(255 * r);
            gradient_pre[selected][1][i][1] = (unsigned char)(255 * g);
            gradient_pre[selected][1][i][2] = (unsigned char)(255 * b);
        }

        // colorScheme 3: Inverse Grayscale
        for (int i = 0; i < gradientSteps; i++) {
            float r, g, b;
            float value = float(i) / gradientSteps;

            r = g = b = value;

            switch (selected) {
            case ColorGradientUnselected:
                // not dimmed
                break;

            case ColorGradientTimeAndFrequencySelected:
            // else fall through to SAME grayscale colour as normal selection.
            // The white lines show it up clearly enough.

            case ColorGradientTimeSelected:
                // partly dimmed
                r = r * 0.75f + 0.25f;
                g = g * 0.75f + 0.25f;
                b = b * 0.75f + 0.25f;
                break;

            case ColorGradientEdge:
                r = 1.0f;
                g = 1.0f;
                b = 1.0f;
                break;
            }
            gradient_pre[selected][3][i][0] = (unsigned char)(255 * r);
            gradient_pre[selected][3][i][1] = (unsigned char)(255 * g);
            gradient_pre[selected][3][i][2] = (unsigned char)(255 * b);
        }

        // colorScheme 2: Grayscale (=Old grayscale)
        for (int i = 0; i < gradientSteps; i++) {
            float r, g, b;
            float value = float(i) / gradientSteps;

            r = g = b = 0.84 - 0.84 * value;

            switch (selected) {
            case ColorGradientUnselected:
                // not dimmed
                break;

            case ColorGradientTimeAndFrequencySelected:
            // else fall through to SAME grayscale colour as normal selection.
            // The white lines show it up clearly enough.

            case ColorGradientTimeSelected:
                // partly dimmed
                r *= 0.75f;
                g *= 0.75f;
                b *= 0.75f;
                break;

            // For now edge colour is just black (or white if grey-scale)
            // Later we might invert or something else funky.
            case ColorGradientEdge:
                // fully dimmed
                r = 1.0f;
                g = 1.0f;
                b = 1.0f;
                break;
            }
            gradient_pre[selected][2][i][0] = (unsigned char)(255 * r);
            gradient_pre[selected][2][i][1] = (unsigned char)(255 * g);
            gradient_pre[selected][2][i][2] = (unsigned char)(255 * b);
        }
    }
}

void AColor::ApplyUpdatedImages()
{
    ReInit();
    theTheme.Publish({});
}
