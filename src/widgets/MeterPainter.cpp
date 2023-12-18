/**********************************************************************

  Audacity: A Digital Audio Editor

  MeterPainter.cpp

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from MeterPanel.cpp

*******************************************************************/
#include "MeterPainter.h"

#include <wx/dcbuffer.h>
#include <wx/settings.h>
#include "AColor.h"
#include "Theme.h"
#include "AllThemeResources.h"

MeterPainter::MeterPainter(bool clip, bool gradient, bool input, int bgColor)
   : mClip{ clip }
   , mGradient{ gradient }
{
   SetBackgroundColor(bgColor);
   mPeakPeakPen = wxPen(theTheme.Colour(clrMeterPeak), 1, wxPENSTYLE_SOLID);

   if (input) {
      mPen       = wxPen(   theTheme.Colour( clrMeterInputPen         ), 1, wxPENSTYLE_SOLID);
      mBrush     = wxBrush( theTheme.Colour( clrMeterInputBrush       ), wxBRUSHSTYLE_SOLID);
      mRMSBrush  = wxBrush( theTheme.Colour( clrMeterInputRMSBrush    ), wxBRUSHSTYLE_SOLID);
      mClipBrush = wxBrush( theTheme.Colour( clrMeterInputClipBrush   ), wxBRUSHSTYLE_SOLID);
//      mLightPen  = wxPen(   theTheme.Colour( clrMeterInputLightPen    ), 1, wxSOLID);
//      mDarkPen   = wxPen(   theTheme.Colour( clrMeterInputDarkPen     ), 1, wxSOLID);
   }
   else {
      mPen       = wxPen(   theTheme.Colour( clrMeterOutputPen        ), 1, wxPENSTYLE_SOLID);
      mBrush     = wxBrush( theTheme.Colour( clrMeterOutputBrush      ), wxBRUSHSTYLE_SOLID);
      mRMSBrush  = wxBrush( theTheme.Colour( clrMeterOutputRMSBrush   ), wxBRUSHSTYLE_SOLID);
      mClipBrush = wxBrush( theTheme.Colour( clrMeterOutputClipBrush  ), wxBRUSHSTYLE_SOLID);
//      mLightPen  = wxPen(   theTheme.Colour( clrMeterOutputLightPen   ), 1, wxSOLID);
//      mDarkPen   = wxPen(   theTheme.Colour( clrMeterOutputDarkPen    ), 1, wxSOLID);
   }

//   mDisabledBkgndBrush = wxBrush(theTheme.Colour( clrMeterDisabledBrush), wxSOLID);
   // No longer show a difference in the background colour when not monitoring.
   // We have the tip instead.
   mDisabledBkgndBrush = mBkgndBrush;
}

void MeterPainter::SetBackgroundColor(int bgColor)
{
   wxColour backgroundColour = theTheme.Colour(bgColor);
   mBkgndBrush = wxBrush(backgroundColour, wxBRUSHSTYLE_SOLID);
}

void MeterPainter::AllocateBitmap(wxDC &dc, int width, int height)
{
   mBitmap = std::make_unique<wxBitmap>();
   mBitmap->Create(width, height, dc);
   wxMemoryDC memdc;
   memdc.SelectObject(*mBitmap);
   memdc.SetPen(*wxTRANSPARENT_PEN);
   memdc.SetBrush(mBkgndBrush);
   memdc.DrawRectangle(0, 0, width, height);
   assert(GetBitmap());
}

void MeterPainter::FillBitmap(const MeterBar &bar, bool dB, int dBRange)
{
   if (!mBitmap)
      return;

   wxMemoryDC dc;
   dc.SelectObject(*mBitmap);

   const auto bg = mBkgndBrush.GetColour();

   // Setup the colors for the 3 sections of the meter bars
   wxColor green(117, 215, 112);
   wxColor yellow(255, 255, 0);
   wxColor red(255, 0, 0);

   // Bug #2473 - (Sort of) Hack to make text on meters more
   // visible with darker backgrounds. It would be better to have
   // different colors entirely and as part of the theme.
   if (bg.GetLuminance() < 0.25) {
      green = wxColor(117-100, 215-100, 112-100);
      yellow = wxColor(255-100, 255-100, 0);
      red = wxColor(255-100, 0, 0);
   }
   else if (bg.GetLuminance() < 0.50) {
      green = wxColor(117-50, 215-50, 112-50);
      yellow = wxColor(255-50, 255-50, 0);
      red = wxColor(255-50, 0, 0);
   }

   // Give it a recessed look
   AColor::Bevel(dc, false, bar.bevel);

   // Draw the clip indicator bevel
   if (mClip)
      AColor::Bevel(dc, false, bar.rClip);

   // Cache bar rect
   wxRect rect = bar.rect;

   if (mGradient) {
      // Calculate the size of the two gradiant segments of the meter
      double gradw;
      double gradh;
      if (dB) {
         gradw = (double) rect.GetWidth() / dBRange * 6.0;
         gradh = (double) rect.GetHeight() / dBRange * 6.0;
      }
      else {
         gradw = (double) rect.GetWidth() / 100 * 25;
         gradh = (double) rect.GetHeight() / 100 * 25;
      }

      if (bar.vert) {
         // Draw the "critical" segment (starts at top of meter and works down)
         rect.SetHeight(gradh);
         dc.GradientFillLinear(rect, red, yellow, wxSOUTH);

         // Draw the "warning" segment
         rect.SetTop(rect.GetBottom());
         dc.GradientFillLinear(rect, yellow, green, wxSOUTH);

         // Draw the "safe" segment
         rect.SetTop(rect.GetBottom());
         rect.SetBottom(bar.rect.GetBottom());
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.SetBrush(green);
         dc.DrawRectangle(rect);
      }
      else {
         // Draw the "safe" segment
         rect.SetWidth(rect.GetWidth() - (int) (gradw + gradw + 0.5));
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.SetBrush(green);
         dc.DrawRectangle(rect);

         // Draw the "warning"  segment
         rect.SetLeft(rect.GetRight() + 1);
         rect.SetWidth(floor(gradw));
         dc.GradientFillLinear(rect, green, yellow);

         // Draw the "critical" segment
         rect.SetLeft(rect.GetRight() + 1);
         rect.SetRight(bar.rect.GetRight());
         dc.GradientFillLinear(rect, yellow, red);
      }
#ifdef EXPERIMENTAL_METER_LED_STYLE
      if (!bar.vert)
      {
         wxRect rect = bar.rect;
         wxPen BackgroundPen;
         BackgroundPen
            .SetColour(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE));
         dc.SetPen( BackgroundPen );
         int i;
         for(i = 0; i < rect.width; ++i)
         {
            // 2 pixel spacing between the LEDs
            if( (i%7)<2 ){
               AColor::Line( dc, i + rect.x, rect.y, i + rect.x,
                  rect.y + rect.height );
            } else {
               // The LEDs have triangular ends.
               // This code shapes the ends.
               int j = abs( (i%7)-4);
               AColor::Line( dc, i + rect.x, rect.y, i + rect.x,
                  rect.y + j + 1);
               AColor::Line( dc, i + rect.x, rect.y + rect.height - j,
                  i + rect.x, rect.y + rect.height);
            }
         }
      }
#endif
   }
}

void MeterBar::SetRectangles(wxRect bounding, bool vertical, bool clip)
{
   // Save the orientation
   vert = vertical;

   // Create the bar rectangle and reduce to fit inside the bevel
   rect = bevel = bounding;
   rect.x += 1;
   rect.width -= 1;
   rect.y += 1;
   rect.height -= 1;

   if (vert) {
      if (clip) {
         // Create the clip rectangle
         rClip = bevel;
         rClip.height = 3;

         // Make room for the clipping indicator
         bevel.y += 3 + gap;
         bevel.height -= 3 + gap;
         rect.y += 3 + gap;
         rect.height -= 3 + gap;
      }
   }
   else {
      if (clip) {
         // Make room for the clipping indicator
         bevel.width -= 4;
         rect.width -= 4;

         // Create the indicator rectangle
         rClip = bevel;
         rClip.x = bevel.GetRight() + 1 + gap; // +1 for bevel
         rClip.width = 3;
      }
   }
}

void MeterPainter::DrawMeterBar(wxDC &dc, bool disabled,
   const MeterBar &bar, Stats &stats) const
{
   if (!mBitmap)
      return;
   auto &bitmap = *mBitmap;

   // Cache some metrics
   wxCoord x = bar.rect.GetLeft();
   wxCoord y = bar.rect.GetTop();
   wxCoord w = bar.rect.GetWidth();
   wxCoord h = bar.rect.GetHeight();
   wxCoord ht;
   wxCoord wd;

   // Setup for erasing the background
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBkgndBrush);

   if (mGradient)
   {
      // Map the predrawn bitmap into the source DC
      wxMemoryDC srcDC;
      srcDC.SelectObject(bitmap);

      if (bar.vert)
      {
         // Copy as much of the predrawn meter bar as is required for the
         // current peak.
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         ht = (int)(stats.peak * (h - 1) + 0.5);

         // Blank out the rest
         if (h - ht)
         {
            // ht includes peak value...not really needed but doesn't hurt
            dc.DrawRectangle(x, y, w, h - ht);
         }

         // Copy as much of the predrawn meter bar as is required for the
         // current peak.
         // +/-1 to include the peak position
         if (ht)
         {
            dc.Blit(x, y + h - ht - 1, w, ht + 1, &srcDC, x, y + h - ht - 1);
         }

         // Draw the "recent" peak hold line using the predrawn meter bar so that
         // it will be the same color as the original level.
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         ht = (int)(stats.peakHold * (h - 1) + 0.5);
         if (ht > 1)
         {
            dc.Blit(x, y + h - ht - 1, w, 2, &srcDC, x, y + h - ht - 1);
         }

         // Draw the "maximum" peak hold line
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPeakPeakPen);
         ht = (int)(stats.peakPeakHold * (h - 1) + 0.5);
         if (ht > 0)
         {
            AColor::Line(dc, x, y + h - ht - 1, x + w - 1, y + h - ht - 1);
            if (ht > 1)
            {
               AColor::Line(dc, x, y + h - ht, x + w - 1, y + h - ht);
            }
         }
      }
      else
      {
         // Calculate the peak position
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         wd = (int)(stats.peak * (w - 1) + 0.5);

         // Blank out the rest
         if (w - wd)
         {
            // wd includes peak value...not really needed but doesn't hurt
            dc.DrawRectangle(x + wd, y, w - wd, h);
         }

         // Copy as much of the predrawn meter bar as is required for the
         // current peak.  But, only blit() if there's something to copy
         // to prevent display corruption.
         // +1 to include peak position
         if (wd)
         {
            dc.Blit(x, y, wd + 1, h, &srcDC, x, y);
         }

         // Draw the "recent" peak hold line using the predrawn meter bar so that
         // it will be the same color as the original level.
         // -1 to give a 2 pixel width
         wd = (int)(stats.peakHold * (w - 1) + 0.5);
         if (wd > 1)
         {
            dc.Blit(x + wd - 1, y, 2, h, &srcDC, x + wd, y);
         }

         // Draw the "maximum" peak hold line using a themed color
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPeakPeakPen);
         wd = (int)(stats.peakPeakHold * (w - 1) + 0.5);
         if (wd > 0)
         {
            AColor::Line(dc, x + wd, y, x + wd, y + h - 1);
            if (wd > 1)
            {
               AColor::Line(dc, x + wd - 1, y, x + wd - 1, y + h - 1);
            }
         }
      }
   }
   else
   {
      if (bar.vert)
      {
         // Calculate the peak position
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         ht = (int)(stats.peak * (h - 1) + 0.5);

         // Blank out the rest
         if (h - ht)
         {
            // ht includes peak value...not really needed but doesn't hurt
            dc.DrawRectangle(x, y, w, h - ht);
         }

         // Draw the peak level
         // +/-1 to include the peak position
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.SetBrush(mBrush);
         if (ht)
         {
            dc.DrawRectangle(x, y + h - ht - 1, w, ht + 1);
         }

         // Draw the "recent" peak hold line
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPen);
         ht = (int)(stats.peakHold * (h - 1) + 0.5);
         if (ht > 0)
         {
            AColor::Line(dc, x, y + h - ht - 1, x + w - 1, y + h - ht - 1);
            if (ht > 1)
            {
               AColor::Line(dc, x, y + h - ht, x + w - 1, y + h - ht);
            }
         }

         // Calculate the rms position
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         // +1 to include the rms position
         ht = (int)(stats.rms * (h - 1) + 0.5);

         // Draw the RMS level
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.SetBrush(mRMSBrush);
         if (ht)
         {
            dc.DrawRectangle(x, y + h - ht - 1, w, ht + 1);
         }

         // Draw the "maximum" peak hold line
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPeakPeakPen);
         ht = (int)(stats.peakPeakHold * (h - 1) + 0.5);
         if (ht > 0)
         {
            AColor::Line(dc, x, y + h - ht - 1, x + w - 1, y + h - ht - 1);
            if (ht > 1)
            {
               AColor::Line(dc, x, y + h - ht, x + w - 1, y + h - ht);
            }
         }
      }
      else
      {
         // Calculate the peak position
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         wd = (int)(stats.peak * (w - 1) + 0.5);

         // Blank out the rest
         if (w - wd)
         {
            // wd includes peak value...not really needed but doesn't hurt
            dc.DrawRectangle(x + wd, y, w - wd, h);
         }

         // Draw the peak level
         // +1 to include peak position
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.SetBrush(mBrush);
         if (wd)
         {
            dc.DrawRectangle(x, y, wd + 1, h);
         }

         // Draw the "recent" peak hold line
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPen);
         wd = (int)(stats.peakHold * (w - 1) + 0.5);
         if (wd > 0)
         {
            AColor::Line(dc, x + wd, y, x + wd, y + h - 1);
            if (wd > 1)
            {
               AColor::Line(dc, x + wd - 1, y, x + wd - 1, y + h - 1);
            }
         }

         // Calculate the rms position
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         wd = (int)(stats.rms * (w - 1) + 0.5);

         // Draw the rms level
         // +1 to include the rms position
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.SetBrush(mRMSBrush);
         if (wd)
         {
            dc.DrawRectangle(x, y, wd + 1, h);
         }

         // Draw the "maximum" peak hold line using a themed color
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPeakPeakPen);
         wd = (int)(stats.peakPeakHold * (w - 1) + 0.5);
         if (wd > 0)
         {
            AColor::Line(dc, x + wd, y, x + wd, y + h - 1);
            if (wd > 1)
            {
               AColor::Line(dc, x + wd - 1, y, x + wd - 1, y + h - 1);
            }
         }
      }
   }

   // If meter had a clipping indicator, draw or erase it
   // LLL:  At least I assume that's what "mClip" is supposed to be for as
   //       it is always "true".
   if (mClip)
   {
      if (stats.clipping)
       {
         dc.SetBrush(mClipBrush);
      }
      else
      {
         dc.SetBrush(mBkgndBrush);
      }
      dc.SetPen(*wxTRANSPARENT_PEN);
      wxRect r(bar.rClip.GetX() + 1,
               bar.rClip.GetY() + 1,
               bar.rClip.GetWidth() - 1,
               bar.rClip.GetHeight() - 1);
      dc.DrawRectangle(r);
   }
}
