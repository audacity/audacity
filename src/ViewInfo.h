/**********************************************************************

  Audacity: A Digital Audio Editor

  ViewInfo.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_VIEWINFO__
#define __AUDACITY_VIEWINFO__

#include <utility>
#include <vector>
#include <wx/event.h> // inherit wxEvtHandler
#include "ClientData.h"
#include "SelectedRegion.h"
#include "MemoryX.h"
#include "Prefs.h"


#ifdef __GNUC__
#define CONST
#else
#define CONST const
#endif

class AudacityProject;

// The subset of ViewInfo information (other than selection)
// that is sufficient for purposes of TrackArtist,
// and for computing conversions between track times and pixel positions.
class AUDACITY_DLL_API ZoomInfo /* not final */
   // Note that ViewInfo inherits from ZoomInfo but there are no virtual functions.
   // That's okay if we pass always by reference and never copy, suffering "slicing."
   : public ClientData::Base
   , protected PrefsListener
{
public:
   static ZoomInfo &Get( AudacityProject &project );
   static const ZoomInfo &Get( const AudacityProject &project );

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
};

class PlayRegion
{
public:
   PlayRegion() = default;

   PlayRegion( const PlayRegion& ) = delete;
   PlayRegion &operator= ( const PlayRegion &that )
   {
      mLocked = that.mLocked;
      // Guarantee the equivalent un-swapped order of endpoints
      mStart = that.GetStart();
      mEnd = that.GetEnd();
      return *this;
   }

   bool Locked() const { return mLocked; }
   void SetLocked( bool locked ) { mLocked = locked; }

   bool Empty() const { return GetStart() == GetEnd(); }
   double GetStart() const
   {
      if ( mEnd < 0 )
         return mStart;
      else
         return std::min( mStart, mEnd );
   }
   double GetEnd() const
   {
      if ( mStart < 0 )
         return mEnd;
      else
         return std::max( mStart, mEnd );
   }

   void SetStart( double start ) { mStart = start; }
   void SetEnd( double end ) { mEnd = end; }
   void SetTimes( double start, double end ) { mStart = start, mEnd = end; }

   void Order()
   {
      if ( mStart >= 0 && mEnd >= 0 && mStart > mEnd)
         std::swap( mStart, mEnd );
   }

private:
   // Times:
   double mStart{ -1.0 };
   double mEnd{ -1.0 };

   bool mLocked{ false };
};

class AUDACITY_DLL_API ViewInfo final
   : public wxEvtHandler, public ZoomInfo
{
public:
   static ViewInfo &Get( AudacityProject &project );
   static const ViewInfo &Get( const AudacityProject &project );

   ViewInfo(double start, double screenDuration, double pixelsPerSecond);

   static int UpdateScrollPrefsID();
   void UpdatePrefs() override;
   void UpdateSelectedPrefs( int id ) override;

   double GetBeforeScreenWidth() const
   {
      return h * zoom;
   }
   void SetBeforeScreenWidth(wxInt64 beforeWidth, wxInt64 screenWidth, double lowerBoundTime = 0.0);

   double GetTotalWidth() const
   { return total * zoom; }

   // Current selection

   SelectedRegion selectedRegion;
   PlayRegion playRegion;

   // Scroll info

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

   bool bScrollBeyondZero;
   bool bAdjustSelectionEdges;

   // During timer update, grab the volatile stream time just once, so that
   // various other drawing code can use the exact same value.
   double mRecentStreamTime;

   void WriteXMLAttributes(XMLWriter &xmlFile) const;
   bool ReadXMLAttribute(const wxChar *attr, const wxChar *value);

   // Receive track panel timer notifications
   void OnTimer(wxCommandEvent &event);
};

#endif
