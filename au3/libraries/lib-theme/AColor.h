/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.h

  Dominic Mazzoni

  Manages color brushes and pens and provides utility
  drawing functions

**********************************************************************/

#ifndef __AUDACITY_COLOR__
#define __AUDACITY_COLOR__

#include <memory>
#include <wx/brush.h> // member variable
#include <wx/pen.h> // member variable

class wxDC;
class wxGraphicsContext;
class wxRect;

class THEME_API AColor
{
public:

    enum ColorGradientChoice {
        ColorGradientUnselected = 0,
        ColorGradientTimeSelected,
        ColorGradientTimeAndFrequencySelected,
        ColorGradientEdge,

        ColorGradientTotal // keep me last
    };

    static void Init();
    static void ReInit();

    static void Arrow(wxDC& dc, wxCoord x, wxCoord y, int width, bool down = true);

    // Draw a line, INCLUSIVE of both endpoints
    // (unlike what wxDC::DrawLine() documentation specifies)
    static void Line(wxDC& dc, wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2);
    static void Line(wxDC& dc, const wxPoint& from, const wxPoint& to);

    // Draw lines, INCLUSIVE of all endpoints
    static void Lines(wxDC& dc, size_t nPoints, const wxPoint points[]);

    static void DrawFocus(wxDC& dc, wxRect& r);
    static void Bevel(wxDC& dc, bool up, const wxRect& r);
    static void Bevel2(wxDC& dc, bool up, const wxRect& r, bool bSel=false, bool bHighlight = false);
    // Draw image filling the rect.
    // In horizontal direction only center pixels are stretched.
    // In vertical direction image stretched proportionally.
    static void DrawHStretch(wxDC& dc, const wxRect& rect, wxBitmap& bitmap);
    static void DrawFrame(wxDC& dc, const wxRect& r, wxBitmap& bitmap, int mid);
    /**
     * \brief Draw a button that fills a given rect
     */
    static void ButtonStretch(wxDC& dc, bool up, const wxRect& r, bool selected = false, bool highlight = false);
    static void BevelTrackInfo(wxDC& dc, bool up, const wxRect& r, bool highlight = false);
    static wxColour Blend(const wxColour& c1, const wxColour& c2);

    static void UseThemeColour(wxDC* dc, int iBrush, int iPen=-1, int alpha = 255);
    static void UseThemeColour(wxGraphicsContext* gc, int iBrush, int iPen=-1, int alpha = 255);
    static void TrackPanelBackground(wxDC* dc, bool selected);

    static void Light(wxDC* dc, bool selected, bool highlight = false);
    static void Medium(wxDC* dc, bool selected);
    static void MediumTrackInfo(wxDC* dc, bool selected);
    static void Dark(wxDC* dc, bool selected, bool highlight = false);

    static void CursorColor(wxDC* dc);
    static void IndicatorColor(wxDC* dc, bool bIsNotRecording);

    static void Mute(wxDC* dc, bool on, bool selected, bool soloing);
    static void Solo(wxDC* dc, bool on, bool selected);

    // In all of these, channel is 1-indexed (1 through 16); if out of bounds
    // (either due to being explicitly set to 0 or due to an allegro file with
    // more than 16 channels) a gray color is returned.

    static void MIDIChannel(wxDC* dc, int channel /* 1 - 16 */);
    static void LightMIDIChannel(wxDC* dc, int channel /* 1 - 16 */);
    static void DarkMIDIChannel(wxDC* dc, int channel /* 1 - 16 */);

    static void TrackFocusPen(wxDC* dc, int level /* 0 - 2 */);
    static void SnapGuidePen(wxDC* dc);

    static void PreComputeGradient();

    static void ApplyUpdatedImages();

    // Member variables

    static wxBrush lightBrush[2];
    static wxBrush mediumBrush[2];
    static wxBrush darkBrush[2];
    static wxPen lightPen[2];
    static wxPen mediumPen[2];
    static wxPen darkPen[2];

    static wxPen cursorPen;
    static wxPen indicatorPen[2];
    static wxBrush indicatorBrush[2];
    // static wxPen playRegionPen[2];
    static wxBrush playRegionBrush[1];

    static wxBrush muteBrush[2];
    static wxBrush soloBrush;

    static wxPen clippingPen;

    static wxPen envelopePen;
    static wxPen WideEnvelopePen;
    static wxBrush envelopeBrush;

    static wxBrush labelTextNormalBrush;
    static wxBrush labelTextEditBrush;
    static wxBrush labelUnselectedBrush;
    static wxBrush labelSelectedBrush;
    static wxBrush labelSyncLockSelBrush;
    static wxPen labelSyncLockSelPen;
    static wxPen labelSurroundPen;

    static wxPen trackFocusPens[3];
    static wxPen snapGuidePen;

    static wxPen tooltipPen;
    static wxBrush tooltipBrush;

    static bool gradient_inited;
    static const int colorSchemes = 4;
    static const int gradientSteps = 256;
    static unsigned char gradient_pre[ColorGradientTotal][colorSchemes][gradientSteps][3];

    // For experiments in mouse-over highlighting only
    static wxPen uglyPen;
    static wxBrush uglyBrush;

private:
    static wxPen sparePen;
    static wxBrush spareBrush;
    static bool inited;
};

inline void GetColorGradient(float value,
                             AColor::ColorGradientChoice selected,
                             int colorScheme,
                             unsigned char* __restrict red,
                             unsigned char* __restrict green,
                             unsigned char* __restrict blue)
{
    int idx = value * (AColor::gradientSteps - 1);

    *red = AColor::gradient_pre[selected][colorScheme][idx][0];
    *green = AColor::gradient_pre[selected][colorScheme][idx][1];
    *blue = AColor::gradient_pre[selected][colorScheme][idx][2];
}

#endif
