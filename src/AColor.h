/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.h

  Dominic Mazzoni

  Manages color brushes and pens and provides utility
  drawing functions

**********************************************************************/

#ifndef __AUDACITY_COLOR__
#define __AUDACITY_COLOR__

#include "MemoryX.h"
#include <wx/brush.h>
#include <wx/pen.h>

class wxDC;
class wxRect;

struct DCUnchanger {
public:
   DCUnchanger() {}

   DCUnchanger(const wxBrush &brush_, const wxPen &pen_, long logicalOperation_)
   : brush(brush_), pen(pen_), logicalOperation(logicalOperation_)
   {}

   void operator () (wxDC *pDC) const;

   wxBrush brush {};
   wxPen pen {};
   long logicalOperation {};
};

// Like wxDCPenChanger, etc., but simple and general
// Make temporary drawing context changes that you back out of, RAII style

class ADCChanger : public std::unique_ptr<wxDC, ::DCUnchanger>
{
   using Base = std::unique_ptr<wxDC, ::DCUnchanger>;
public:
   ADCChanger() : Base{} {}
   ADCChanger(wxDC *pDC);
};

class AColor {
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

   static void Arrow(wxDC & dc, wxCoord x, wxCoord y, int width, bool down = true);
   static void Line(wxDC & dc, wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2);
   static void DrawFocus(wxDC & dc, wxRect & r);
   static void Bevel(wxDC & dc, bool up, const wxRect & r);
   static void Bevel2(wxDC & dc, bool up, const wxRect & r);
   static void BevelTrackInfo(wxDC & dc, bool up, const wxRect & r);
   static wxColour Blend(const wxColour & c1, const wxColour & c2);

   static void UseThemeColour( wxDC * dc, int iIndex );
   static void TrackPanelBackground(wxDC * dc, bool selected);

   static void Light(wxDC * dc, bool selected);
   static void Medium(wxDC * dc, bool selected);
   static void MediumTrackInfo(wxDC * dc, bool selected);
   static void Dark(wxDC * dc, bool selected);

   static void CursorColor(wxDC * dc);
   static void IndicatorColor(wxDC * dc, bool bIsNotRecording);
   static void PlayRegionColor(wxDC * dc, bool locked);

   static void Mute(wxDC * dc, bool on, bool selected, bool soloing);
   static void Solo(wxDC * dc, bool on, bool selected);

   static void MIDIChannel(wxDC * dc, int channel /* 1 - 16 */ );
   static void LightMIDIChannel(wxDC * dc, int channel /* 1 - 16 */ );
   static void DarkMIDIChannel(wxDC * dc, int channel /* 1 - 16 */ );

   static void TrackFocusPen(wxDC * dc, int level /* 0 - 2 */);
   static void SnapGuidePen(wxDC * dc);

   static void PreComputeGradient();

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
   static wxPen playRegionPen[2];
   static wxBrush playRegionBrush[2];

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
   static wxPen labelUnselectedPen;
   static wxPen labelSelectedPen;
   static wxPen labelSyncLockSelPen;
   static wxPen labelSurroundPen;

   static wxPen trackFocusPens[3];
   static wxPen snapGuidePen;

   static wxPen tooltipPen;
   static wxBrush tooltipBrush;

   static bool gradient_inited;
   static const int gradientSteps = 512;
   static unsigned char gradient_pre[ColorGradientTotal][2][gradientSteps][3];

 private:
   static wxPen sparePen;
   static wxBrush spareBrush;
   static bool inited;

};

inline void GetColorGradient(float value,
                             AColor::ColorGradientChoice selected,
                             bool grayscale,
                             unsigned char * __restrict red,
                             unsigned char * __restrict green,
                             unsigned char * __restrict blue) {

   int idx = value * (AColor::gradientSteps - 1);

   *red = AColor::gradient_pre[selected][grayscale][idx][0];
   *green = AColor::gradient_pre[selected][grayscale][idx][1];
   *blue = AColor::gradient_pre[selected][grayscale][idx][2];
}

#endif
