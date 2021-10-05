/**********************************************************************

Audacity: A Digital Audio Editor

TimeShiftHandle.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TIMESHIFT_HANDLE__
#define __AUDACITY_TIMESHIFT_HANDLE__

#include <functional>
#include <unordered_map>
#include <vector>

#include "AttachedVirtualFunction.h"
#include "../../UIHandle.h"

class SnapManager;
class Track;
using TrackArray = std::vector<Track*>;
class TrackList;

class Track;
class TrackInterval;

class ViewInfo;
class wxMouseState;

//! Abstract base class for policies to manipulate a track type with the Time Shift tool
class AUDACITY_DLL_API TrackShifter {
public:
   TrackShifter();
   TrackShifter(const TrackShifter&) PROHIBITED;
   TrackShifter &operator=(const TrackShifter&) PROHIBITED;

   virtual ~TrackShifter() = 0;
   //! There is always an associated track
   virtual Track &GetTrack() const = 0;

   //! Possibilities for HitTest on the clicked track
   enum class HitTestResult {
      Miss,      //!< Don't shift anything
      Selection, //!< Shfit chosen intervals of this track; may shift other tracks' intervals
      Intervals, //!< Shift intervals only of selected track and sister channels
      Track      //!< Shift selected track and sister channels only, as a whole
   };

   //! Optional, more complete information for hit testing
   struct HitTestParams {
      wxRect rect;
      wxCoord xx, yy;
   };

   //! Decide how shift behaves, based on the track that is clicked in
   /*! If the return value is Intervals or Selection,
       then some intervals may be marked moving as a side effect */
   /*!
    @pre `!pParams || (time == pParams->viewInfo.PositionToTime(pParams->xx, pParams->rect.x))`
    */
   virtual HitTestResult HitTest(
      double time, //!< A time value to test
      const ViewInfo &viewInfo,
      HitTestParams *pParams = nullptr //!< Optional extra information
   ) = 0;

   using Intervals = std::vector<TrackInterval>;

   //! Return special intervals of the track that will not move
   const Intervals &FixedIntervals() const { return mFixed; }

   //! Return special intervals of the track that may move
   const Intervals &MovingIntervals() const { return mMoving; }
   
   //! Change intervals satisfying a predicate from fixed to moving
   void UnfixIntervals(
      std::function< bool( const TrackInterval& ) > pred );

   //! Change all intervals from fixed to moving
   void UnfixAll();

   //! Notifies the shifter that a region is selected, so it may update its fixed and moving intervals
   /*! Default behavior:  if any part of the track is selected, unfix all parts of it. */
   virtual void SelectInterval( const TrackInterval &interval );

   //! Whether unfixing of an interval should propagate to all overlapping intervals in the sync lock group
   virtual bool SyncLocks() = 0;

   //! Given amount to shift by horizontally, maybe adjust it from zero to suggest minimum distance
   /*!
    Any interval placement constraints, not necessarily met at the suggested offset
    Default implementation returns the argument
    @post `fabs(r) >= fabs(desiredOffset)`
    @post `r * desiredOffset >= 0` (i.e. signs are not opposite)
    @post (where `r` is return value)
    */
   virtual double HintOffsetLarger( double desiredOffset );

   //! Given amount to shift by horizontally, do any preferred rounding, before placement constraint checks
   /*! Default implementation returns argument */
   virtual double QuantizeOffset( double desiredOffset );

   //! Given amount to shift by horizontally, maybe adjust it toward zero to meet placement constraints
   /*!
    Default implementation returns the argument
    @post `fabs(r) <= fabs(desiredOffset)`
    @post `r * desiredOffset >= 0` (i.e. signs are not opposite)
    @post (where `r` is return value)
    */
   virtual double AdjustOffsetSmaller( double desiredOffset );

   //! Whether intervals may migrate to the other track, not yet checking all placement constraints */
   /*! Default implementation returns false */
   virtual bool MayMigrateTo( Track &otherTrack );

   //! Remove all moving intervals from the track, if possible
   /*! Default implementation does nothing */
   virtual Intervals Detach();

   //! Test whether intervals can fit into another track, maybe adjusting the offset slightly
   /*! Default implementation does nothing and returns false */
   virtual bool AdjustFit(
      const Track &otherTrack,
      const Intervals &intervals, /*!<
         Assume these came from Detach() and only after MayMigrateTo returned true for otherTrack */
      double &desiredOffset, //!< [in,out]
      double tolerance //! Nonnegative ceiling for allowed changes in fabs(desiredOffset)
   );

   //! Put moving intervals into the track, which may have migrated from another
   /*! @return success
   
       In case of failure, track states are unspecified
    
       Default implementation does nothing and returns true */
   virtual bool Attach( Intervals intervals );

   //! When dragging is done, do (once) the final steps of migration (which may be expensive)
   /*! @return success
   
       In case of failure, track states are unspecified
    
       Default implementation does nothing and returns true */
   virtual bool FinishMigration();

   //! Shift all moving intervals horizontally
   //! Default moves the whole track, provided `!AllFixed()`; else does nothing
   virtual void DoHorizontalOffset( double offset );

   // This function is used by the keyboard interface for time shifting.
   // It adjusts t0 after the move to take into account
   // any rounding errors if this is necessary.
   virtual double AdjustT0(double t0) const;

protected:
   /*! Unfix any of the intervals that intersect the given one; may be useful to override `SelectInterval()` */
   void CommonSelectInterval( const TrackInterval &interval );

   /*! May be useful to override `MayMigrateTo()`, if certain other needed overrides are given.
       Returns true, iff: tracks have same type, and corresponding positions in their channel groups,
       which have same size */
   bool CommonMayMigrateTo( Track &otherTrack );

   //! Derived class constructor can initialize all intervals reported by the track as fixed, none moving
   /*! This can't be called by the base class constructor, when GetTrack() isn't yet callable */
   void InitIntervals();

   bool AllFixed() const {
      return mAllFixed && mMoving.empty();
   }

   Intervals mFixed;
   Intervals mMoving;

private:
   bool mAllFixed = true; /*!<
      Becomes false after `UnfixAll()`, even if there are no intervals, or if any one interval was unfixed */
};

//! Used in default of other reimplementations to shift any track as a whole, invoking Track::Offset()
class CoarseTrackShifter final : public TrackShifter {
public:
   CoarseTrackShifter( Track &track );
   ~CoarseTrackShifter() override;
   Track &GetTrack() const override { return *mpTrack; }

   HitTestResult HitTest( double, const ViewInfo&, HitTestParams* ) override;

   //! Returns false
   bool SyncLocks() override;

private:
   std::shared_ptr<Track> mpTrack;
};

struct MakeTrackShifterTag;
using MakeTrackShifter = AttachedVirtualFunction<
   MakeTrackShifterTag, std::unique_ptr<TrackShifter>, Track, AudacityProject&>;
DECLARE_EXPORTED_ATTACHED_VIRTUAL(AUDACITY_DLL_API, MakeTrackShifter);

class ViewInfo;

struct AUDACITY_DLL_API ClipMoveState {
   ClipMoveState() = default;

   ClipMoveState(const ClipMoveState&) PROHIBITED;
   ClipMoveState& operator =(const ClipMoveState&) PROHIBITED;

   ClipMoveState(ClipMoveState&&) = default;
   ClipMoveState& operator =(ClipMoveState&&) = default;

   using ShifterMap = std::unordered_map<Track*, std::unique_ptr<TrackShifter>>;
   
   //! Will associate a TrackShifter with each track in the list
   void Init(
      AudacityProject &project,
      Track &capturedTrack, //<! pHit if not null associates with this track
      TrackShifter::HitTestResult hitTestResult, //!< must not be `Miss`
      std::unique_ptr<TrackShifter> pHit, /*!<
         If null, implies `Track`, overriding previous argument */
      double clickTime,
      const ViewInfo &viewInfo,
      TrackList &trackList, bool syncLocked );

   //! Return pointer to the first fixed interval of the captured track, if there is one
   /*! Pointer may be invalidated by operations on the associated TrackShifter */
   const TrackInterval *CapturedInterval() const;

   //! Do sliding of tracks and intervals, maybe adjusting the offset
   /*! @return actual slide amount, maybe adjusted toward zero from desired */
   double DoSlideHorizontal( double desiredSlideAmount );

   //! Offset tracks or intervals horizontally, without adjusting the offset
   void DoHorizontalOffset( double offset );

   std::shared_ptr<Track> mCapturedTrack;

   bool initialized{ false };
   bool movingSelection {};
   bool wasMoved{ false };
   double hSlideAmount {};
   ShifterMap shifters;
   wxInt64 snapLeft { -1 }, snapRight { -1 };

   int mMouseClickX{};

   void clear()
   {
      initialized = false;
      wasMoved = false;
      movingSelection = false;
      hSlideAmount = 0;
      shifters.clear();
      snapLeft = snapRight = -1;
      mMouseClickX = 0;
   }
};

class AUDACITY_DLL_API TimeShiftHandle : public UIHandle
{
   TimeShiftHandle(const TimeShiftHandle&) = delete;
   static HitTestPreview HitPreview
      (const AudacityProject *pProject, bool unsafe);

public:
   explicit TimeShiftHandle
   ( const std::shared_ptr<Track> &pTrack, bool gripHit );

   TimeShiftHandle &operator=(TimeShiftHandle&&) = default;

   bool IsGripHit() const { return mGripHit; }

   // Try to move clips from one track to another, before also moving
   // by some horizontal amount, which may be slightly adjusted to fit the
   // destination tracks.
   static bool DoSlideVertical(
      ViewInfo &viewInfo, wxCoord xx,
      ClipMoveState &state, TrackList &trackList,
      Track &dstTrack, double &desiredSlideAmount );

   static UIHandlePtr HitAnywhere
      (std::weak_ptr<TimeShiftHandle> &holder,
       const std::shared_ptr<Track> &pTrack, bool gripHit);
   static UIHandlePtr HitTest
      (std::weak_ptr<TimeShiftHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const std::shared_ptr<Track> &pTrack);

   virtual ~TimeShiftHandle();

   void Enter(bool forward, AudacityProject *) override;

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   bool StopsOnKeystroke() override { return true; }

   bool Clicked() const;

protected:
   std::shared_ptr<Track> GetTrack() const;
   //There were attempt to move clip/track horizontally, or to move it vertically
   bool WasMoved() const;
private:
   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   wxRect DrawingArea(
      TrackPanelDrawingContext &,
      const wxRect &rect, const wxRect &panelRect, unsigned iPass ) override;

   wxRect mRect{};

   bool mDidSlideVertically{};
   bool mSlideUpDownOnly{};

   bool mSnapPreferRightEdge{};

   // Handles snapping the selection boundaries or track boundaries to
   // line up with existing tracks or labels.  mSnapLeft and mSnapRight
   // are the horizontal index of pixels to display user feedback
   // guidelines so the user knows when such snapping is taking place.
   std::shared_ptr<SnapManager> mSnapManager{};

   ClipMoveState mClipMoveState{};
   bool mGripHit {};
};

#endif
