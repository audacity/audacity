/**********************************************************************

  Audacity: A Digital Audio Editor

  ViewInfo.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_VIEWINFO__
#define __AUDACITY_VIEWINFO__

#include <utility>
#include <vector>
#include <wx/weakref.h> // member variable
#include "SelectedRegion.h"
#include <memory>
#include "Observer.h"
#include "Prefs.h"
#include "XMLMethodRegistry.h"
#include "ZoomInfo.h" // to inherit

struct NotifyingSelectedRegionMessage : Observer::Message {};

// This heavyweight wrapper of the SelectedRegion structure emits events
// on mutating operations, that other classes can listen for.
class SCREEN_GEOMETRY_API NotifyingSelectedRegion
   : public Observer::Publisher<NotifyingSelectedRegionMessage>
   , public wxTrackable
{
public:
   // Expose SelectedRegion's const accessors
   double t0 () const { return mRegion.t0(); }
   double t1 () const { return mRegion.t1(); }
   double f0 () const { return mRegion.f0(); }
   double f1 () const { return mRegion.f1(); }
   double fc () const { return mRegion.fc(); }
   bool isPoint() const { return mRegion.isPoint(); }
   double duration() const { return mRegion.duration(); }

   // Writing and reading of persistent fields -- the read is mutating but
   // does not emit events
   void WriteXMLAttributes
      (XMLWriter &xmlFile,
       const char *legacyT0Name, const char *legacyT1Name) const
   { mRegion.WriteXMLAttributes(xmlFile, legacyT0Name, legacyT1Name); }

   //! Return some information used for deserialization purposes by ViewInfo
   static XMLMethodRegistryBase::Mutators<NotifyingSelectedRegion>
      Mutators(const char *legacyT0Name, const char *legacyT1Name);

   // const-only access allows assignment from this into a SelectedRegion
   // or otherwise passing it into a function taking const SelectedRegion&
   operator const SelectedRegion & () const { return mRegion; }

   // These are the event-emitting operations
   NotifyingSelectedRegion& operator = ( const SelectedRegion &other );
   
   // Returns true iff the bounds got swapped
   bool setTimes(double t0, double t1);

   // Returns true iff the bounds got swapped
   bool setT0(double t, bool maySwap = true);

   // Returns true iff the bounds got swapped
   bool setT1(double t, bool maySwap = true);

   void collapseToT0();

   void collapseToT1();

   void move(double delta);

   // Returns true iff the bounds got swapped
   bool setFrequencies(double f0, double f1);

   // Returns true iff the bounds got swapped
   bool setF0(double f, bool maySwap = true);

   // Returns true iff the bounds got swapped
   bool setF1(double f, bool maySwap = true);

private:
   void Notify( bool delayed = false );

   SelectedRegion mRegion;
};

enum : int {
   kVerticalPadding = 7, /*!< Width of padding below each channel group */
};

enum : int {
   kTrackInfoBtnSize = 18, // widely used dimension, usually height
   kTrackEffectsBtnHeight = 28,
   kTrackInfoSliderHeight = 25,
   kTrackInfoSliderWidth = 84,
   kTrackInfoSliderAllowance = 5,
   kTrackInfoSliderExtra = 5,
};

struct PlayRegionMessage : Observer::Message {};

class SCREEN_GEOMETRY_API PlayRegion
   : public Observer::Publisher<PlayRegionMessage>
{
public:
   PlayRegion() = default;

   PlayRegion( const PlayRegion& ) = delete;
   PlayRegion &operator= ( const PlayRegion &that )
   {
      mActive = that.mActive;
      // Guarantee the equivalent un-swapped order of endpoints
      mStart = that.GetStart();
      mEnd = that.GetEnd();
      mLastActiveStart = that.GetLastActiveStart();
      mLastActiveEnd = that.GetLastActiveEnd();
      return *this;
   }

   bool Active() const { return mActive; }
   void SetActive( bool active );

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
   double GetLastActiveStart() const
   {
      if ( mLastActiveEnd < 0 )
         return mLastActiveStart;
      else
         return std::min( mLastActiveStart, mLastActiveEnd );
   }
   double GetLastActiveEnd() const
   {
      if ( mLastActiveStart < 0 )
         return mLastActiveEnd;
      else
         return std::max( mLastActiveStart, mLastActiveEnd );
   }

   void SetStart( double start );
   void SetEnd( double end );
   void SetTimes( double start, double end );
   // Set current and last active times the same regardless of activation:
   void SetAllTimes( double start, double end );

   //! Set to an invalid state
   void Clear();
   //! Test whether in invalid state
   bool IsClear() const;
   //! Test whether last active region is in invalid state
   bool IsLastActiveRegionClear() const;

   void Order();

private:
   void Notify();

   // Times:
   static constexpr auto invalidValue = std::numeric_limits<double>::min();

   double mStart { invalidValue };
   double mEnd { invalidValue };
   double mLastActiveStart { invalidValue };
   double mLastActiveEnd { invalidValue };

   bool mActive{ false };
};

extern SCREEN_GEOMETRY_API const TranslatableString LoopToggleText;

class SCREEN_GEOMETRY_API ViewInfo final
   : public ZoomInfo
{
public:
   static ViewInfo &Get( AudacityProject &project );
   static const ViewInfo &Get( const AudacityProject &project );

   ViewInfo(double start, double screenDuration, double pixelsPerSecond);
   ViewInfo( const ViewInfo & ) PROHIBITED;
   ViewInfo &operator=( const ViewInfo & ) PROHIBITED;

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

   NotifyingSelectedRegion selectedRegion;
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

   void WriteXMLAttributes(XMLWriter &xmlFile) const;

private:
   int mHeight{ 0 };

   struct ProjectFileIORegistration;
};

extern SCREEN_GEOMETRY_API BoolSetting ScrollingPreference;
#endif
