/**********************************************************************

  Audacity: A Digital Audio Editor

  ZoomInfo.h

  Paul Licameli split from ViewInfo.h

**********************************************************************/

#ifndef __AUDACITY_ZOOM_INFO__
#define __AUDACITY_ZOOM_INFO__

#include "ClientData.h" // to inherit
#include "Prefs.h" // to inherit

#ifdef __GNUC__
#define CONST
#else
#define CONST const
#endif

class AudacityProject;

// See big pictorial comment in TrackPanel.cpp for explanation of these numbers
enum : int {
   // Constants related to x coordinates in the track panel
   kBorderThickness = 1,
   kShadowThickness = 1,

   kLeftInset = 4,
   kRightInset = kLeftInset,
   kLeftMargin = kLeftInset + kBorderThickness,
   kRightMargin = kRightInset + kShadowThickness + kBorderThickness,

   kTrackInfoWidth = 100 - kLeftMargin,
};

// The subset of ViewInfo information (other than selection)
// that is sufficient for purposes of TrackArtist,
// and for computing conversions between track times and pixel positions.
class SCREEN_GEOMETRY_API ZoomInfo /* not final */
   // Note that ViewInfo inherits from ZoomInfo but there are no virtual functions.
   // That's okay if we pass always by reference and never copy, suffering "slicing."
   : public ClientData::Base
   , protected PrefsListener
{
public:
   ZoomInfo(double start, double pixelsPerSecond);
   ~ZoomInfo();

   // Be sure we don't slice
   ZoomInfo(const ZoomInfo&) PROHIBITED;
   ZoomInfo& operator= (const ZoomInfo&) PROHIBITED;

   void UpdatePrefs() override;

   int vpos;                    // vertical scroll pos

   double h;                    // h pos in secs

protected:
   double zoom;                 // pixels per second

public:
   float dBr;                   // decibel scale range

   // do NOT use this once to convert a pixel width to a duration!
   // Instead, call twice to convert start and end times,
   // and take the difference.
   // origin specifies the pixel corresponding to time h
   double PositionToTime(wxInt64 position,
      wxInt64 origin = 0
      , bool ignoreFisheye = false
   ) const;

   // do NOT use this once to convert a duration to a pixel width!
   // Instead, call twice to convert start and end positions,
   // and take the difference.
   // origin specifies the pixel corresponding to time h
   wxInt64 TimeToPosition(double time,
      wxInt64 origin = 0
      , bool ignoreFisheye = false
   ) const;

   // This always ignores the fisheye.  Use with caution!
   // You should prefer to call TimeToPosition twice, for endpoints, and take the difference!
   double TimeRangeToPixelWidth(double timeRange) const;

   double OffsetTimeByPixels(double time, wxInt64 offset, bool ignoreFisheye = false) const
   {
      return PositionToTime(offset + TimeToPosition(time, ignoreFisheye), ignoreFisheye);
   }

   int GetWidth() const { return mWidth; }
   void SetWidth( int width ) { mWidth = width; }

   int GetVRulerWidth() const { return mVRulerWidth; }
   void SetVRulerWidth( int width ) { mVRulerWidth = width; }
   int GetVRulerOffset() const { return kTrackInfoWidth + kLeftMargin; }

   // The x-coordinate of the start of the displayed track data
   int GetLeftOffset() const
      { return GetVRulerOffset() + GetVRulerWidth() + 1; }
   // The number of pixel columns for display of track data
   int GetTracksUsableWidth() const
   {
      return
         std::max( 0, GetWidth() - ( GetLeftOffset() + kRightMargin ) );
   }

   // Returns the time corresponding to the pixel column one past the track area
   // (ignoring any fisheye)
   double GetScreenEndTime() const
   {
      auto width = GetTracksUsableWidth();
      return PositionToTime(width, 0, true);
   }

   bool ZoomInAvailable() const;
   bool ZoomOutAvailable() const;

   static double GetDefaultZoom()
   { return 44100.0 / 512.0; }


   // Limits zoom to certain bounds
   void SetZoom(double pixelsPerSecond);

   // This function should not be used to convert positions to times and back
   // Use TimeToPosition and PositionToTime and OffsetTimeByPixels instead
   double GetZoom() const;

   static double GetMaxZoom( );
   static double GetMinZoom( );

   // Limits zoom to certain bounds
   // multipliers above 1.0 zoom in, below out
   void ZoomBy(double multiplier);

   struct Interval {
      CONST wxInt64 position; CONST double averageZoom; CONST bool inFisheye;
      Interval(wxInt64 p, double z, bool i)
         : position(p), averageZoom(z), inFisheye(i) {}
   };
   typedef std::vector<Interval> Intervals;

   // Find an increasing sequence of pixel positions.  Each is the start
   // of an interval, or is the end position.
   // Each of the disjoint intervals should be drawn
   // separately.
   // It is guaranteed that there is at least one entry and the position of the
   // first entry equals origin.
   // @param origin specifies the pixel position corresponding to time ViewInfo::h.
   void FindIntervals
      (double rate, Intervals &results, wxInt64 width, wxInt64 origin = 0) const;

   enum FisheyeState {
      HIDDEN,
      PINNED,

      NUM_STATES,
   };
   FisheyeState GetFisheyeState() const
   { return HIDDEN; } // stub

   // Return true if the mouse position is anywhere in the fisheye
   // origin specifies the pixel corresponding to time h
   bool InFisheye(wxInt64 /*position*/, wxInt64 WXUNUSED(origin = 0)) const
   {return false;} // stub

   // These accessors ignore the fisheye hiding state.
   // Inclusive:
   wxInt64 GetFisheyeLeftBoundary(wxInt64 WXUNUSED(origin = 0)) const
   {return 0;} // stub
   // Exclusive:
   wxInt64 GetFisheyeRightBoundary(wxInt64 WXUNUSED(origin = 0)) const
   {return 0;} // stub

   int mWidth{ 0 };
   int mVRulerWidth{ 36 };
};

#endif
