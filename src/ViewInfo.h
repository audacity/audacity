/**********************************************************************

  Audacity: A Digital Audio Editor

  ViewInfo.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_VIEWINFO__
#define __AUDACITY_VIEWINFO__

#include "SelectedRegion.h"


class Track;


// The subset of ViewInfo information (other than selection)
// that is sufficient for purposes of TrackArtist,
// and for computing conversions between track times and pixel positions.
class AUDACITY_DLL_API ZoomInfo
{
public:
   ZoomInfo(double start, double duration, double pixelsPerSecond);
   ~ZoomInfo();

   int vpos;                    // vertical scroll pos

   double h;                    // h pos in secs

   double screen;               // screen width in secs
   double zoom;                 // pixels per second

public:

   // do NOT use this once to convert a pixel width to a duration!
   // Instead, call twice to convert start and end times,
   // and take the difference.
   // origin specifies the pixel corresponding to time h
   double PositionToTime(wxInt64 position,
      wxInt64 origin = 0
      ) const;

   // do NOT use this once to convert a duration to a pixel width!
   // Instead, call twice to convert start and end positions,
   // and take the difference.
   // origin specifies the pixel corresponding to time h
   wxInt64 TimeToPosition(double time,
      wxInt64 origin = 0
      ) const;

   double OffsetTimeByPixels(double time, wxInt64 offset) const
   {
      return PositionToTime(offset + TimeToPosition(time));
   }

   bool ZoomInAvailable() const;
   bool ZoomOutAvailable() const;

   // Return pixels, but maybe not a whole number
   double GetScreenWidth() const
   { return screen * zoom; }

   void SetScreenWidth(wxInt64 width)
   { screen = width / zoom; }

   static double GetDefaultZoom()
   { return 44100.0 / 512.0; }

   // There is NO GetZoom()!
   // Use TimeToPosition and PositionToTime and OffsetTimeByPixels!

   // Limits zoom to certain bounds
   void SetZoom(double pixelsPerSecond);

   // Limits zoom to certain bounds
   // multipliers above 1.0 zoom in, below out
   void ZoomBy(double multiplier);
};

class AUDACITY_DLL_API ViewInfo
   : public ZoomInfo
{
public:
   ViewInfo(double start, double screenDuration, double pixelsPerSecond);

   double GetBeforeScreenWidth() const
   {
      return h * zoom;
   }
   void SetBeforeScreenWidth(wxInt64 width, double lowerBoundTime = 0.0);

   double GetTotalWidth() const
   { return total * zoom; }

   bool ZoomedAll() const
   { return screen >= total; }

   // Current selection

   SelectedRegion selectedRegion;

   // Scroll info

   Track *track;                // first visible track

   double total;                // total width in secs
   // Current horizontal scroll bar positions, in pixels
   wxInt64 sbarH;
   wxInt64 sbarScreen;
   wxInt64 sbarTotal;

   // Internal wxScrollbar positions are only int in range, so multiply
   // the above values with the following member to get the actual
   // scroll bar positions as reported by the horizontal wxScrollbar's members
   // i.e. units are scroll increments per pixel
   double sbarScale;

   // Vertical scroll step
   int scrollStep;

   // Other stuff, mainly states (true or false) related to autoscroll and
   // drawing the waveform. Maybe this should be put somewhere else?

   bool bUpdateTrackIndicator;

   void WriteXMLAttributes(XMLWriter &xmlFile);
   bool ReadXMLAttribute(const wxChar *attr, const wxChar *value);
};

#endif
