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
#include "SelectedRegion.h"
#include "MemoryX.h"
#include "ZoomInfo.h" // to inherit


// See big pictorial comment in TrackPanel.cpp for explanation of these numbers
enum : int {
   // constants related to y coordinates in the track panel
   kTopInset = 4,
   kTopMargin = kTopInset + kBorderThickness,
   kBottomMargin = kShadowThickness + kBorderThickness,
   kSeparatorThickness = kBottomMargin + kTopMargin,
};

enum : int {
   kTrackInfoBtnSize = 18, // widely used dimension, usually height
   kTrackInfoSliderHeight = 25,
   kTrackInfoSliderWidth = 84,
   kTrackInfoSliderAllowance = 5,
   kTrackInfoSliderExtra = 5,
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

   int GetHeight() const { return mHeight; }
   void SetHeight( int height ) { mHeight = height; }

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

private:
   int mHeight{ 0 };
};

#endif
