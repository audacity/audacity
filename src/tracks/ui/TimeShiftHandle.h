/**********************************************************************

Audacity: A Digital Audio Editor

TimeShiftHandle.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TIMESHIFT_HANDLE__
#define __AUDACITY_TIMESHIFT_HANDLE__

#include <functional>
#include <unordered_map>

#include "../../AttachedVirtualFunction.h"
#include "../../UIHandle.h"

class SnapManager;
class Track;
using TrackArray = std::vector<Track*>;
class TrackList;

class Track;
class TrackInterval;

//! Abstract base class for policies to manipulate a track type with the Time Shift tool
class TrackShifter {
public:
   virtual ~TrackShifter() = 0;
   //! There is always an associated track
   virtual Track &GetTrack() const = 0;

   //! Possibilities for HitTest on the clicked track
   enum class HitTestResult {
      Miss,      //!< Don't shift anything
      Intervals, //<! May shift other tracks' intervals, if clicked in selection
      Track      //<! Shift selected track only as a whole
   };

   //! Decide how shift behaves, based on the track that is clicked in
   /*! If the return value is Intervals, then some intervals may be marked moving as a side effect */
   virtual HitTestResult HitTest( double time ) = 0;

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

protected:
   /*! Unfix any of the intervals that intersect the given one; may be useful to override `SelectInterval()` */
   void CommonSelectInterval( const TrackInterval &interval );

   //! Derived class constructor can initialize all intervals reported by the track as fixed, none moving
   /*! This can't be called by the base class constructor, when GetTrack() isn't yet callable */
   void InitIntervals();

   Intervals mFixed;
   Intervals mMoving;
};

//! Used in default of other reimplementations to shift any track as a whole, invoking Track::Offset()
class CoarseTrackShifter final : public TrackShifter {
public:
   CoarseTrackShifter( Track &track );
   ~CoarseTrackShifter() override;
   Track &GetTrack() const override { return *mpTrack; }

   HitTestResult HitTest( double ) override;

   //! Returns false
   bool SyncLocks() override;

private:
   std::shared_ptr<Track> mpTrack;
};

struct MakeTrackShifterTag;
using MakeTrackShifter = AttachedVirtualFunction<
   MakeTrackShifterTag, std::unique_ptr<TrackShifter>, Track>;

class ViewInfo;
class WaveClip;
class WaveTrack;

class TrackClip
{
public:
   TrackClip(Track *t, WaveClip *c);

   ~TrackClip();

   Track *track;
   Track *origTrack;
   WaveClip *clip;

   // These fields are used only during time-shift dragging
   WaveTrack *dstTrack;
   std::shared_ptr<WaveClip> holder;
};

using TrackClipArray = std::vector <TrackClip>;

struct ClipMoveState {
   using ShifterMap = std::unordered_map<Track*, std::unique_ptr<TrackShifter>>;
   
   //! Will associate a TrackShifter with each track in the list
   void Init(
      Track &capturedTrack, //<! pHit if not null associates with this track
      std::unique_ptr<TrackShifter> pHit, /*!<
         If null, only capturedTrack (with any sister channels) shifts, as a whole */
      double clickTime,
      const ViewInfo &viewInfo,
      TrackList &trackList, bool syncLocked );

   //! Return pointer to the first fixed interval of the captured track, if there is one
   /*! Pointer may be invalidated by operations on the associated TrackShifter */
   const TrackInterval *CapturedInterval() const;

   /*! @return actual slide amount, maybe adjusted toward zero from desired */
   double DoSlideHorizontal( double desiredSlideAmount, TrackList &trackList );

   std::shared_ptr<Track> mCapturedTrack;

   // non-NULL only if click was in a WaveTrack and without Shift key:
   WaveClip *capturedClip {};

   bool movingSelection {};
   double hSlideAmount {};
   ShifterMap shifters;
   TrackClipArray capturedClipArray {};
   wxInt64 snapLeft { -1 }, snapRight { -1 };

   int mMouseClickX{};

   void clear()
   {
      capturedClip = nullptr;
      movingSelection = false;
      hSlideAmount = 0;
      shifters.clear();
      capturedClipArray.clear();
      snapLeft = snapRight = -1;
      mMouseClickX = 0;
   }
};

class TimeShiftHandle final : public UIHandle
{
   TimeShiftHandle(const TimeShiftHandle&) = delete;
   static HitTestPreview HitPreview
      (const AudacityProject *pProject, bool unsafe);

public:
   explicit TimeShiftHandle
   ( const std::shared_ptr<Track> &pTrack, bool gripHit );

   TimeShiftHandle &operator=(TimeShiftHandle&&) = default;

   bool IsGripHit() const { return mGripHit; }
   std::shared_ptr<Track> GetTrack() const = delete;

   // Try to move clips from one WaveTrack to another, before also moving
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
