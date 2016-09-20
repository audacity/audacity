
/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.cpp

  Dominic Mazzoni


********************************************************************//**

\class AColor
\brief AColor Manages color brushes and pens

It is also a place to document colour usage policy in Audacity

*//********************************************************************/

#include "Audacity.h"
#include "AColor.h"

#include <wx/colour.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/settings.h>
#include <wx/utils.h>

#include "Theme.h"
#include "Experimental.h"
#include "AllThemeResources.h"

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
wxPen AColor::playRegionPen[2];
wxBrush AColor::playRegionBrush[2];

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
wxPen AColor::labelUnselectedPen;
wxPen AColor::labelSelectedPen;
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

//
// Draw an upward or downward pointing arrow.
//
void AColor::Arrow(wxDC & dc, wxCoord x, wxCoord y, int width, bool down)
{
   if (width & 0x01) {
      width--;
   }

   wxPoint pt[3];
   int half = width / 2;

   if (down) {
      pt[0].x =     0; pt[0].y = 0;
      pt[1].x = width; pt[1].y = 0;
      pt[2].x =  half; pt[2].y = half;
   }
   else {
      pt[0].x =     0; pt[0].y = half;
      pt[1].x =  half; pt[1].y = 0;
      pt[2].x = width; pt[2].y = half;
   }

   dc.DrawPolygon(3, pt, x, y);
}

//
// Draw a line while accounting for differences in wxWidgets versions
//
void AColor::Line(wxDC & dc, wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2)
{
   // As of 2.8.9 (possibly earlier), wxDC::DrawLine() on the Mac draws the
   // last point since it is now based on the NEW wxGraphicsContext system.
   // Make the other platforms do the same thing since the other platforms
   // "may" follow they get wxGraphicsContext going.
#if defined(__WXMAC__) || defined(__WXGTK3__)
   dc.DrawLine(x1, y1, x2, y2);
#else
   bool point = false;

   if (x1 == x2) {
      if (y1 < y2) {
         y2++;
      }
      else if (y2 < y1) {
         y1++;
      }
      else {
         point = true;
      }
   }
   else if (y1 == y2) {
      if (x1 < x2) {
         x2++;
      }
      else if (x2 < x1) {
         x1++;
      }
      else {
         point = true;
      }
   }
   else {
      dc.DrawPoint(x2, y2);
   }

   if (point) {
      dc.DrawPoint(x2, y2);
   }
   else {
      dc.DrawLine(x1, y1, x2, y2);
   }
#endif
}

//
// Draws a focus rectangle (Taken directly from wxWidgets source)
//
void AColor::DrawFocus(wxDC & dc, wxRect & rect)
{
   // draw the pixels manually: note that to behave in the same manner as
   // DrawRect(), we must exclude the bottom and right borders from the
   // rectangle
   wxCoord x1 = rect.GetLeft(),
         y1 = rect.GetTop(),
         x2 = rect.GetRight(),
         y2 = rect.GetBottom();

#ifdef __WXMAC__
   // Why must this be different?
   // Otherwise nothing is visible if you do as for the
   // other platforms.
   dc.SetPen(wxPen(wxT("MEDIUM GREY"), 1, wxSOLID));

   dc.SetLogicalFunction(wxCOPY);
#else
   dc.SetPen(wxPen(wxT("MEDIUM GREY"), 0, wxSOLID));

   // this seems to be closer than what Windows does than wxINVERT although
   // I'm still not sure if it's correct
   dc.SetLogicalFunction(wxAND_REVERSE);
#endif

   wxCoord z;
   for ( z = x1 + 1; z < x2; z += 2 )
      dc.DrawPoint(z, y1);

   wxCoord shift = z == x2 ? 0 : 1;
   for ( z = y1 + shift; z < y2; z += 2 )
      dc.DrawPoint(x2, z);

   shift = z == y2 ? 0 : 1;
   for ( z = x2 - shift; z > x1; z -= 2 )
      dc.DrawPoint(z, y2);

   shift = z == x1 ? 0 : 1;
   for ( z = y2 - shift; z > y1; z -= 2 )
      dc.DrawPoint(x1, z);

   dc.SetLogicalFunction(wxCOPY);
}

void AColor::Bevel(wxDC & dc, bool up, const wxRect & r)
{
   if (up)
      AColor::Light(&dc, false);
   else
      AColor::Dark(&dc, false);

   AColor::Line(dc, r.x, r.y, r.x + r.width, r.y);
   AColor::Line(dc, r.x, r.y, r.x, r.y + r.height);

   if (!up)
      AColor::Light(&dc, false);
   else
      AColor::Dark(&dc, false);

   AColor::Line(dc, r.x + r.width, r.y, r.x + r.width, r.y + r.height);
   AColor::Line(dc, r.x, r.y + r.height, r.x + r.width, r.y + r.height);
}

void AColor::Bevel2(wxDC & dc, bool up, const wxRect & r)
{
   wxBitmap & Bmp = theTheme.Bitmap( up ? bmpUpButtonLarge : bmpDownButtonLarge );
   wxMemoryDC memDC;
   memDC.SelectObject(Bmp);
   int h = wxMin( r.height, Bmp.GetHeight() );


   dc.Blit( r.x,r.y,r.width/2, h, &memDC, 0, 0 );
   dc.Blit( r.x+r.width/2,r.y,r.width/2, h, &memDC, 
      Bmp.GetWidth() - r.width/2, 0 );
}

wxColour AColor::Blend( const wxColour & c1, const wxColour & c2 )
{
   wxColour c3(
      (c1.Red() + c2.Red())/2,
      (c1.Green() + c2.Green())/2,
      (c1.Blue() + c2.Blue())/2);
   return c3;
}

void AColor::BevelTrackInfo(wxDC & dc, bool up, const wxRect & r)
{
#ifndef EXPERIMENTAL_THEMING
   Bevel( dc, up, r );
#else
   wxColour col;
   col = Blend( theTheme.Colour( clrTrackInfo ), up ? wxColour( 255,255,255):wxColour(0,0,0));

   wxPen pen( col );
   dc.SetPen( pen );

   dc.DrawLine(r.x, r.y, r.x + r.width, r.y);
   dc.DrawLine(r.x, r.y, r.x, r.y + r.height);

   col = Blend( theTheme.Colour( clrTrackInfo ), up ? wxColour(0,0,0): wxColour(255,255,255));

   pen.SetColour( col );
   dc.SetPen( pen );

   dc.DrawLine(r.x + r.width, r.y, r.x + r.width, r.y + r.height);
   dc.DrawLine(r.x, r.y + r.height, r.x + r.width + 1, r.y + r.height);
#endif
}

void AColor::UseThemeColour( wxDC * dc, int iIndex )
{
   if (!inited)
      Init();
   wxColour col = theTheme.Colour( iIndex );
   spareBrush.SetColour( col );
   dc->SetBrush( spareBrush );
   sparePen.SetColour( col );
   dc->SetPen( sparePen );
}

void AColor::Light(wxDC * dc, bool selected)
{
   if (!inited)
      Init();
   int index = (int) selected;
   dc->SetBrush(lightBrush[index]);
   dc->SetPen(lightPen[index]);
}

void AColor::Medium(wxDC * dc, bool selected)
{
   if (!inited)
      Init();
   int index = (int) selected;
   dc->SetBrush(mediumBrush[index]);
   dc->SetPen(mediumPen[index]);
}

#if 0
#ifdef EXPERIMENTAL_THEMING
   UseThemeColour( dc, selected ? clrMediumSelected : clrMedium);
#endif
#ifdef EXPERIMENTAL_THEMING
   UseThemeColour( dc, selected ? clrLightSelected : clrLight);
#endif
#endif

void AColor::MediumTrackInfo(wxDC * dc, bool selected)
{
#ifdef EXPERIMENTAL_THEMING
   UseThemeColour( dc, selected ? clrTrackInfoSelected : clrTrackInfo );
#else
   Medium( dc, selected );
#endif
}


void AColor::Dark(wxDC * dc, bool selected)
{
   if (!inited)
      Init();
   int index = (int) selected;
   dc->SetBrush(darkBrush[index]);
   dc->SetPen(darkPen[index]);
}

void AColor::TrackPanelBackground(wxDC * dc, bool selected)
{
#ifdef EXPERIMENTAL_THEMING
   UseThemeColour( dc, selected ? clrDarkSelected : clrDark);
#else
   Dark( dc, selected );
#endif
}

void AColor::CursorColor(wxDC * dc)
{
   if (!inited)
      Init();
#if defined(__WXMAC__) || defined(__WXGTK3__)
   dc->SetLogicalFunction(wxCOPY);
   dc->SetPen(wxColor(0, 0, 0, 128));
#else
   dc->SetLogicalFunction(wxINVERT);
   dc->SetPen(cursorPen);
#endif
}

void AColor::IndicatorColor(wxDC * dc, bool bIsNotRecording)
{
   if (!inited)
      Init();
   int index = (int) bIsNotRecording;
   dc->SetPen(indicatorPen[index]);
   dc->SetBrush(indicatorBrush[index]);
}

void AColor::PlayRegionColor(wxDC * dc, bool locked)
{
   if (!inited)
      Init();
   dc->SetPen(playRegionPen[(int)locked]);
   dc->SetBrush(playRegionBrush[(int)locked]);
}

void AColor::TrackFocusPen(wxDC * dc, int level)
{
   if (!inited)
      Init();
   dc->SetPen(trackFocusPens[level]);
}

void AColor::SnapGuidePen(wxDC * dc)
{
   if (!inited)
      Init();
   dc->SetPen(snapGuidePen);
}

void AColor::Mute(wxDC * dc, bool on, bool selected, bool soloing)
{
   if (!inited)
      Init();
   int index = (int) selected;
   if (on) {
      dc->SetPen(*wxBLACK_PEN);
      dc->SetBrush(muteBrush[(int) soloing]);
   }
   else {
      dc->SetPen(*wxTRANSPARENT_PEN);
      dc->SetBrush(mediumBrush[index]);
   }
}

void AColor::Solo(wxDC * dc, bool on, bool selected)
{
   if (!inited)
      Init();
   int index = (int) selected;
   if (on) {
      dc->SetPen(*wxBLACK_PEN);
      dc->SetBrush(soloBrush);
   }
   else {
      dc->SetPen(*wxTRANSPARENT_PEN);
      dc->SetBrush(mediumBrush[index]);
   }
}

void AColor::ReInit()
{
   inited=false;
   Init();
}

void AColor::Init()
{
   if (inited)
      return;

   wxColour light =
       wxSystemSettings::GetColour(wxSYS_COLOUR_3DHIGHLIGHT);
   wxColour med = wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE);
   wxColour dark =
       wxSystemSettings::GetColour(wxSYS_COLOUR_3DSHADOW);

   clippingPen.SetColour(0xCC, 0x11, 0x00);

   theTheme.SetPenColour(   envelopePen,     clrEnvelope );
   theTheme.SetPenColour(   WideEnvelopePen, clrEnvelope );
   theTheme.SetBrushColour( envelopeBrush,   clrEnvelope );

   WideEnvelopePen.SetWidth( 3 );

   theTheme.SetBrushColour( labelTextNormalBrush,  clrLabelTextNormalBrush );
   theTheme.SetBrushColour( labelTextEditBrush,    clrLabelTextEditBrush );
   theTheme.SetBrushColour( labelUnselectedBrush,  clrLabelUnselectedBrush );
   theTheme.SetBrushColour( labelSelectedBrush,    clrLabelSelectedBrush );
   theTheme.SetBrushColour( labelSyncLockSelBrush, clrSyncLockSel );
   theTheme.SetPenColour( labelUnselectedPen,   clrLabelUnselectedPen );
   theTheme.SetPenColour( labelSelectedPen,     clrLabelSelectedPen );
   theTheme.SetPenColour( labelSyncLockSelPen,  clrSyncLockSel );
   theTheme.SetPenColour( labelSurroundPen,     clrLabelSurroundPen );

   // These colors were modified to avoid using reserved colors red and green
   // for the buttons.
   theTheme.SetBrushColour( muteBrush[0],      clrMuteButtonActive);
   theTheme.SetBrushColour( muteBrush[1],      clrMuteButtonVetoed);
   theTheme.SetBrushColour( soloBrush,         clrMuteButtonActive);

   theTheme.SetPenColour(   cursorPen,         clrCursorPen);
   theTheme.SetPenColour(   indicatorPen[0],   clrRecordingPen);
   theTheme.SetPenColour(   indicatorPen[1],   clrPlaybackPen);
   theTheme.SetBrushColour( indicatorBrush[0], clrRecordingBrush);
   theTheme.SetBrushColour( indicatorBrush[1], clrPlaybackBrush);

   theTheme.SetBrushColour( playRegionBrush[0],clrRulerRecordingBrush);
   theTheme.SetPenColour(   playRegionPen[0],  clrRulerRecordingPen);
   theTheme.SetBrushColour( playRegionBrush[1],clrRulerPlaybackBrush);
   theTheme.SetPenColour(   playRegionPen[1],  clrRulerPlaybackPen);

   //Determine tooltip color
   tooltipPen.SetColour( wxSystemSettingsNative::GetColour(wxSYS_COLOUR_INFOTEXT) );
   tooltipBrush.SetColour( wxSystemSettingsNative::GetColour(wxSYS_COLOUR_INFOBK) );

   // A tiny gradient of yellow surrounding the current focused track
   theTheme.SetPenColour(   trackFocusPens[0],  clrTrackFocus0);
   theTheme.SetPenColour(   trackFocusPens[1],  clrTrackFocus1);
   theTheme.SetPenColour(   trackFocusPens[2],  clrTrackFocus2);

   // A vertical line indicating that the selection or sliding has
   // been snapped to the nearest boundary.
   theTheme.SetPenColour(   snapGuidePen,      clrSnapGuide);

#if defined(__WXMSW__) || defined(__WXGTK__)
   // unselected
   lightBrush[0].SetColour(light);
   mediumBrush[0].SetColour(med);
   darkBrush[0].SetColour(dark);
   lightPen[0].SetColour(light);
   mediumPen[0].SetColour(med);
   darkPen[0].SetColour(dark);

   // selected
   lightBrush[1].SetColour(204, 204, 255);
   mediumBrush[1].SetColour(200, 200, 214);
   darkBrush[1].SetColour(148, 148, 170);
   lightPen[1].SetColour(204, 204, 255);
   mediumPen[1].SetColour(200, 200, 214);
   darkPen[1].SetColour(0, 0, 0);

#else

#if defined(__WXMAC__)          // && defined(TARGET_CARBON)

   // unselected
   lightBrush[0].SetColour(246, 246, 255);
   mediumBrush[0].SetColour(220, 220, 220);
   darkBrush[0].SetColour(140, 140, 160);
   lightPen[0].SetColour(246, 246, 255);
   mediumPen[0].SetColour(220, 220, 220);
   darkPen[0].SetColour(140, 140, 160);

   // selected
   lightBrush[1].SetColour(204, 204, 255);
   mediumBrush[1].SetColour(180, 180, 192);
   darkBrush[1].SetColour(148, 148, 170);
   lightPen[1].SetColour(204, 204, 255);
   mediumPen[1].SetColour(180, 180, 192);
   darkPen[1].SetColour(148, 148, 170);

#else

   // unselected
   lightBrush[0].SetColour(255, 255, 255);
   mediumBrush[0].SetColour(204, 204, 204);
   darkBrush[0].SetColour(130, 130, 130);
   lightPen[0].SetColour(255, 255, 255);
   mediumPen[0].SetColour(204, 204, 204);
   darkPen[0].SetColour(130, 130, 130);

   // selected
   lightBrush[1].SetColour(204, 204, 255);
   mediumBrush[1].SetColour(180, 180, 192);
   darkBrush[1].SetColour(148, 148, 170);
   lightPen[1].SetColour(204, 204, 255);
   mediumPen[1].SetColour(180, 180, 192);
   darkPen[1].SetColour(0, 0, 0);

#endif

#endif

   inited = true;
}

const int AColor_midicolors[16][3] = {
   {255, 102, 102},             // 1=salmon
   {204, 0, 0},                 // 2=red
   {255, 117, 23},              // 3=orange
   {255, 255, 0},               // 4=yellow
   {0, 204, 0},                 // 5=green
   {0, 204, 204},               // 6=turquoise
   {0, 0, 204},                 // 7=blue
   {153, 0, 255},               // 8=blue-violet

   {140, 97, 54},               // 9=brown
   {120, 120, 120},             // 10=gray (drums)
   {255, 175, 40},              // 11=lt orange
   {102, 255, 102},             // 12=lt green
   {153, 255, 255},             // 13=lt turquoise
   {153, 153, 255},             // 14=lt blue
   {204, 102, 255},             // 15=lt blue-violet
   {255, 51, 204}
};                              // 16=lt red-violet

void AColor::MIDIChannel(wxDC * dc, int channel /* 1 - 16 */ )
{
   if (channel >= 1 && channel <= 16) {
      const int *colors = AColor_midicolors[channel - 1];

      dc->SetPen(wxPen(wxColour(colors[0],
                                colors[1], colors[2]), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(colors[0],
                                    colors[1], colors[2]), wxSOLID));
   } else {
      dc->SetPen(wxPen(wxColour(153, 153, 153), 1, wxSOLID));// DONT-THEME Midi, unused.
      dc->SetBrush(wxBrush(wxColour(153, 153, 153), wxSOLID));
   }

}

void AColor::LightMIDIChannel(wxDC * dc, int channel /* 1 - 16 */ )
{
   if (channel >= 1 && channel <= 16) {
      const int *colors = AColor_midicolors[channel - 1];

      dc->SetPen(wxPen(wxColour(127 + colors[0] / 2,
                                127 + colors[1] / 2,
                                127 + colors[2] / 2), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(127 + colors[0] / 2,
                                    127 + colors[1] / 2,
                                    127 + colors[2] / 2), wxSOLID));
   } else {
      dc->SetPen(wxPen(wxColour(204, 204, 204), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(204, 204, 204), wxSOLID));
   }

}

void AColor::DarkMIDIChannel(wxDC * dc, int channel /* 1 - 16 */ )
{
   if (channel >= 1 && channel <= 16) {
      const int *colors = AColor_midicolors[channel - 1];

      dc->SetPen(wxPen(wxColour(colors[0] / 2,
                                colors[1] / 2,
                                colors[2] / 2), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(colors[0] / 2,
                                    colors[1] / 2,
                                    colors[2] / 2), wxSOLID));
   } else {
      dc->SetPen(wxPen(wxColour(102, 102, 102), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(102, 102, 102), wxSOLID));
   }

}

bool AColor::gradient_inited = 0;

unsigned char AColor::gradient_pre[ColorGradientTotal][2][gradientSteps][3];

void AColor::PreComputeGradient() {
   {
      if (!gradient_inited) {
         gradient_inited = 1;

         for (int selected = 0; selected < ColorGradientTotal; selected++)
            for (int grayscale = 0; grayscale <= 1; grayscale++) {
               float r, g, b;

               int i;
               for (i=0; i<gradientSteps; i++) {
                  float value = float(i)/gradientSteps;

                  if (grayscale) {
                     r = g = b = 0.84 - 0.84 * value;
                  } else {
                     const int gsteps = 4;
                     float gradient[gsteps + 1][3] = {
                        {float(0.75), float(0.75), float(0.75)},    // lt gray
                        {float(0.30), float(0.60), float(1.00)},    // lt blue
                        {float(0.90), float(0.10), float(0.90)},    // violet
                        {float(1.00), float(0.00), float(0.00)},    // red
                        {float(1.00), float(1.00), float(1.00)}     // white
                     };

                     int left = (int)(value * gsteps);
                     int right = (left == gsteps ? gsteps : left + 1);

                     float rweight = (value * gsteps) - left;
                     float lweight = 1.0 - rweight;

                     r = (gradient[left][0] * lweight) + (gradient[right][0] * rweight);
                     g = (gradient[left][1] * lweight) + (gradient[right][1] * rweight);
                     b = (gradient[left][2] * lweight) + (gradient[right][2] * rweight);
                  }

                  switch (selected) {
                  case ColorGradientUnselected:
                     // not dimmed
                     break;

                  case ColorGradientTimeAndFrequencySelected:
                     if( !grayscale )
                     {
                        float temp;
                        temp = r;
                        r = g;
                        g = b;
                        b = temp;
                        break;
                     }
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
                     r = 1.0f * grayscale;
                     g = 1.0f * grayscale;
                     b = 1.0f * grayscale;
                     break;
                  }
                  gradient_pre[selected][grayscale][i][0] = (unsigned char) (255 * r);
                  gradient_pre[selected][grayscale][i][1] = (unsigned char) (255 * g);
                  gradient_pre[selected][grayscale][i][2] = (unsigned char) (255 * b);
               }
            }
      }
   }
}
