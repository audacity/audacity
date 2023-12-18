/**********************************************************************

  Audacity: A Digital Audio Editor

  MeterPainter.h

  Dominic Mazzoni

  Paul Licameli split from MeterPanel.h

  VU Meter, for displaying recording/playback level

  This is a bunch of common code that can display many different
  forms of VU meters and other displays.

**********************************************************************/
#ifndef __AUDACITY_METER_PAINTER__
#define __AUDACITY_METER_PAINTER__

#include <wx/brush.h> // member variable
#include <wx/pen.h> // member variable
#include "PeakAndRmsMeter.h"

struct MeterBar {
   // How many pixels between items?
   static constexpr int gap = 2;

   //! Given the bounding rectangle, subdivide it
   void SetRectangles(wxRect bounding, bool vertical, bool clip);

   bool   vert{};
   wxRect bevel{}; // Bevel around bar
   wxRect rect{}; // True bar drawing area
   wxRect rClip{}; // Rectangle for clipping, nonoverlapping with bevel
};

class MeterPainter {
public:
   using Stats = PeakAndRmsMeter::Stats;

   MeterPainter(bool clip, bool gradient, bool input,
      int bgColor //!< Theme color code
   );
   void SetBackgroundColor(int bgColor);

   //! Destroy any existing bitmap first; make new one filled with bg color
   /*!
    @post `GetBitmap() != nullptr`
    */
   void AllocateBitmap(wxDC &dc, int width, int height);

   //! Color the bitmap as for maximum levels
   void FillBitmap(const MeterBar &bar, bool dB, int dBRange);

   //! Blit parts of the stored bitmap to dc and fill the rest as background,
   //! according to levels in stats
   void DrawMeterBar(wxDC &dc, bool disabled,
      const MeterBar &meterBar, Stats &stats) const;

   bool GetGradient() const { return mGradient; }
   void SetGradient(bool gradient) { mGradient = gradient; }

   bool GetClip() const { return mClip; }
   void SetClip(bool clip) { mClip = clip; }

   wxBitmap *GetBitmap() { return mBitmap.get(); }

private:
   wxPen     mPen;
   wxPen     mPeakPeakPen;
   wxBrush   mBrush;
   wxBrush   mRMSBrush;
   wxBrush   mClipBrush;
   wxBrush   mDisabledBkgndBrush;
   wxBrush   mBkgndBrush;
   std::unique_ptr<wxBitmap> mBitmap;
   bool mClip;
   bool mGradient;
};

#endif
