/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveClipAdjustBorderHandle.cpp

 Vitaly Sverchinsky

 **********************************************************************/

#include "WaveClipAdjustBorderHandle.h"
#include "ProjectAudioIO.h"
#include "RefreshCode.h"

#include <wx/event.h>

#include "../../../../TrackArtist.h"
#include "../../../../Snap.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../../images/Cursors.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "WaveChannelView.h"
#include "HitTestResult.h"
#include "TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "ProjectHistory.h"
#include "UndoManager.h"
#include "WaveClipUtilities.h"

namespace {

   void TrimLeftTo(WaveTrack& track, WaveTrack::Interval& interval, double t)
   {
      const auto rate = track.GetRate();
      const auto t0 = interval.GetPlayStartTime();
      interval.TrimLeftTo(t0 + std::floor((t - t0) * rate) / rate);
   }

   void TrimRightTo(WaveTrack& track, WaveTrack::Interval& interval, double t)
   {
      const auto rate = track.GetRate();
      const auto t1 = interval.GetPlayEndTime();
      interval.TrimRightTo(t1 - std::floor((t1 - t) * rate) / rate);
   }

   void StretchLeftTo(WaveTrack& track, WaveTrack::Interval& interval, double t)
   {
      const auto rate = track.GetRate();
      const auto t1 = interval.GetPlayEndTime();
      const auto duration = std::floor((t1 - t) * rate) / rate;
      if(duration >= 1.0 / rate)
         interval.Stretch(duration, true);
   }

   void StretchRightTo(WaveTrack& track, WaveTrack::Interval& interval, double t)
   {
      const auto rate = track.GetRate();
      const auto t0 = interval.GetPlayStartTime();
      const auto duration = std::floor((t - t0) * rate) / rate;
      if(duration >= 1.0 / rate)
         interval.Stretch(duration);
   }
}

//Different policies implement different adjustment scenarios
 class WaveClipAdjustBorderHandle::AdjustPolicy
 {
 public:
    virtual ~AdjustPolicy();

    virtual bool Init(const TrackPanelMouseEvent& event) = 0;
    virtual UIHandle::Result Drag(const TrackPanelMouseEvent& event, AudacityProject& project) = 0;
    virtual void Finish(AudacityProject& project) = 0;
    virtual void Cancel() = 0;

    virtual void Draw(
        TrackPanelDrawingContext &context,
        const wxRect &rect,
        unsigned iPass);

    virtual wxRect DrawingArea(
        TrackPanelDrawingContext&,
        const wxRect &rect,
        const wxRect &panelRect,
        unsigned iPass);
 };

WaveClipAdjustBorderHandle::AdjustPolicy::~AdjustPolicy() = default;

namespace {
double GetLeftAdjustLimit(const WaveTrack::Interval& interval,
                          const WaveTrack& track,
                          bool adjustingLeftBorder,
                          bool isStretchMode)
{
   if (!adjustingLeftBorder)
      return interval.Start() + 1.0 / track.GetRate();

   const auto prevInterval = track.GetNextInterval(interval, PlaybackDirection::backward);
   if(isStretchMode)
      return prevInterval ? prevInterval->End() :
                            std::numeric_limits<double>::lowest();
   if(prevInterval)
      return std::max(interval.GetClip(0)->GetSequenceStartTime(),
                prevInterval->End());
   return interval.GetClip(0)->GetSequenceStartTime();
}

double GetRightAdjustLimit(
   const WaveTrack::Interval& interval, const WaveTrack& track, bool adjustingLeftBorder,
   bool isStretchMode)
{
   if (adjustingLeftBorder)
      return interval.End() - 1.0 / track.GetRate();

   const auto nextInterval = track.GetNextInterval(interval, PlaybackDirection::forward);
   if (isStretchMode)
      return nextInterval ? nextInterval->Start() :
                            std::numeric_limits<double>::max();

   if(nextInterval)
      return std::min(interval.GetClip(0)->GetSequenceEndTime(),
                      nextInterval->Start());
   return interval.GetClip(0)->GetSequenceEndTime();
}
} // namespace

class AdjustClipBorder final : public WaveClipAdjustBorderHandle::AdjustPolicy
{
public:
   using AdjustHandler = std::function<void(WaveTrack&, WaveTrack::Interval&, double)>;

private:
   std::shared_ptr<WaveTrack> mTrack;
   std::shared_ptr<WaveTrack::Interval> mInterval;
   int mDragStartX{ };
   const bool mAdjustingLeftBorder;
   const bool mIsStretchMode;
   const double mInitialBorderPosition;
   double mBorderPosition;
   const std::pair<double, double> mRange;
   AdjustHandler mAdjustHandler;

   std::unique_ptr<SnapManager> mSnapManager;
   SnapResults mSnap;

   void TrimTo(double t)
   {
      mBorderPosition = std::clamp(t, mRange.first, mRange.second);
      mAdjustHandler(*mTrack, *mInterval, t);
   }

   //Search for a good snap points among all tracks except
   //one to which moving interval belongs to
   static SnapPointArray FindSnapPoints(
      const WaveTrack* currentTrack,
      const std::pair<double, double> range)
   {
      SnapPointArray result;

      auto addSnapPoint = [&](double t, const Track* track)
      {
         if(t > range.second || t < range.first)
            return;

         for(const auto& snapPoint : result)
            if(snapPoint.t == t)
               return;
         result.emplace_back(t, track);
      };

      if(const auto trackList = currentTrack->GetOwner())
      {
         for(const auto track : as_const(*trackList))
         {
            if(track == currentTrack)
            {
               //skip track that interval belongs to
               continue;
            }

            for(const auto& interval : track->Intervals())
            {
               addSnapPoint(interval->Start(), track);
               if(interval->Start() != interval->End())
                  addSnapPoint(interval->End(), track);
            }
         }
      }
      return result;
   }

public:
   AdjustClipBorder(AdjustHandler adjustHandler,
                    std::shared_ptr<WaveTrack> track,
                    std::shared_ptr<WaveTrack::Interval> interval,
                    bool adjustLeftBorder,
                    bool isStretchMode,
                    const ZoomInfo& zoomInfo)
      : mTrack { std::move(track) }
      , mInterval { std::move(interval) }
      , mAdjustingLeftBorder { adjustLeftBorder }
      , mIsStretchMode { isStretchMode }
      , mInitialBorderPosition { adjustLeftBorder ? mInterval->Start() :
                                             mInterval->End() }
      , mBorderPosition { mInitialBorderPosition } 
      , mRange { GetLeftAdjustLimit( *mInterval, *mTrack, adjustLeftBorder, isStretchMode),
                 GetRightAdjustLimit(*mInterval, *mTrack, adjustLeftBorder, isStretchMode) }
      , mAdjustHandler { std::move(adjustHandler) }
   {
      assert(mRange.first <= mRange.second);
      if(const auto trackList = mTrack->GetOwner())
      {
         mSnapManager = std::make_unique<SnapManager>(
            *trackList->GetOwner(),
            FindSnapPoints(mTrack.get(), mRange),
            zoomInfo);
      }
   }

   bool Init(const TrackPanelMouseEvent& event) override
   {
      if (event.event.LeftDown())
      {
         mDragStartX = event.event.GetX();
         return true;
      }
      return false;
   }

   UIHandle::Result Drag(const TrackPanelMouseEvent& event, AudacityProject& project) override
   {
      const auto eventX = event.event.GetX();
      const auto dx = eventX - mDragStartX;

      const auto& viewInfo = ViewInfo::Get(project);
      
      const auto eventT = viewInfo.PositionToTime(
         viewInfo.TimeToPosition(mInitialBorderPosition, event.rect.x) + dx,
         event.rect.x
      );

      const auto offset = sampleCount(floor((eventT - mInitialBorderPosition) * mTrack->GetRate())).as_double()
         / mTrack->GetRate();
      const auto t = std::clamp(mInitialBorderPosition + offset, mRange.first, mRange.second);
      const auto wasSnapped = mSnap.Snapped();
      if(mSnapManager)
         mSnap = mSnapManager->Snap(mTrack.get(), t, !mAdjustingLeftBorder);
      if(mSnap.Snapped())
      {
         if (mSnap.outTime >= mRange.first && mSnap.outTime <= mRange.second)
         {
            //Make sure that outTime belongs to the adjustment range after snapping
            TrimTo(mSnap.outTime);
            return RefreshCode::RefreshAll;
         }
         mSnap = {};
      }
      TrimTo(t);
      //If there was a snap line, make sure it is removed
      //from the screen by redrawing whole TrackPanel
      return wasSnapped ? RefreshCode::RefreshAll : RefreshCode::RefreshCell;
   }

   void Finish(AudacityProject& project) override
   {
      const auto dt = std::abs(mInitialBorderPosition - mBorderPosition);
      if (dt != 0)
      {
         if (mIsStretchMode)
         {
            PushClipSpeedChangedUndoState(
               project, 100.0 / mInterval->GetStretchRatio());
         }
         else if (mAdjustingLeftBorder)
         {
            /*i18n-hint: This is about trimming a clip, a length in seconds like "2.4 seconds" is shown*/
            ProjectHistory::Get(project).PushState(XO("Adjust left trim by %.02f seconds").Format(dt),
               /*i18n-hint: This is about trimming a clip, a length in seconds like "2.4s" is shown*/
                XO("Trim by %.02fs").Format(dt));
         }
         else
         {
            /*i18n-hint: This is about trimming a clip, a length in seconds like "2.4 seconds" is shown*/
            ProjectHistory::Get(project).PushState(XO("Adjust right trim by %.02f seconds").Format(dt),
            /*i18n-hint: This is about trimming a clip, a length in seconds like "2.4s" is shown*/
                XO("Trim by %.02fs").Format(dt));
         }
      }
   }

   void Cancel() override
   {
      TrimTo(mInitialBorderPosition);
   }

   void Draw(TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override
   {
      if(iPass == TrackArtist::PassSnapping && mSnap.Snapped())
      {
         auto &dc = context.dc;
         SnapManager::Draw(&dc, rect.x + mSnap.outCoord, -1);
      }
   }

   wxRect DrawingArea(TrackPanelDrawingContext&, const wxRect& rect, const wxRect& panelRect, unsigned iPass) override
   {
      if(iPass == TrackArtist::PassSnapping)
        return TrackPanelDrawable::MaximizeHeight(rect, panelRect);
      return rect;
   }
};

HitTestPreview WaveClipAdjustBorderHandle::HitPreviewTrim(const AudacityProject*, bool unsafe, bool isLeftBorder)
{
   static auto disabledCursor =
      MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
   static auto trimCursorLeft =
      MakeCursor(wxCURSOR_SIZEWE, ClipTrimLeftXpm , 16, 16);
   static auto trimCursorRight =
      MakeCursor(wxCURSOR_SIZEWE, ClipTrimRightXpm, 16, 16);
   auto message = XO("Click and drag to move clip boundary in time");

   return {
      message,
      (unsafe
         ? &*disabledCursor
         : &*(isLeftBorder ? trimCursorLeft : trimCursorRight))
   };
}

HitTestPreview WaveClipAdjustBorderHandle::HitPreviewStretch(const AudacityProject*, bool unsafe, bool isLeftBorder)
{
   static auto disabledCursor =
      MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
   static auto stretchCursorLeft =
      MakeCursor(wxCURSOR_SIZEWE, ClipStretchLeftXpm, 16, 16);
   static auto stretchCursorRight =
      MakeCursor(wxCURSOR_SIZEWE, ClipStretchRightXpm, 16, 16);
   auto message = XO("Click and drag to stretch clip");

   return {
      message,
      (unsafe
         ? &*disabledCursor
         : &*(isLeftBorder ? stretchCursorLeft : stretchCursorRight))
   };
}

WaveClipAdjustBorderHandle::WaveClipAdjustBorderHandle(std::unique_ptr<AdjustPolicy>& adjustPolicy,
                                                       bool stretchMode,
                                                       bool leftBorder)
   : mAdjustPolicy{ std::move(adjustPolicy) }
   , mIsStretchMode{stretchMode}
   , mIsLeftBorder{leftBorder}
{

}

WaveClipAdjustBorderHandle::~WaveClipAdjustBorderHandle() = default;

WaveClipAdjustBorderHandle::WaveClipAdjustBorderHandle(WaveClipAdjustBorderHandle&&) noexcept = default;

WaveClipAdjustBorderHandle& WaveClipAdjustBorderHandle::operator=(WaveClipAdjustBorderHandle&&) noexcept = default;

void WaveClipAdjustBorderHandle::AdjustPolicy::Draw(TrackPanelDrawingContext&, const wxRect&, unsigned) { }

wxRect WaveClipAdjustBorderHandle::AdjustPolicy::DrawingArea(TrackPanelDrawingContext&, const wxRect& rect, const wxRect&, unsigned)
{
   return rect;
}

UIHandlePtr WaveClipAdjustBorderHandle::HitAnywhere(
   std::weak_ptr<WaveClipAdjustBorderHandle>& holder,
   const std::shared_ptr<WaveTrack>& waveTrack,
   const AudacityProject* pProject,
   const TrackPanelMouseState& state)
{
   assert(waveTrack->IsLeader());

   const auto rect = state.rect;

   const auto px = state.state.m_x;

   auto& zoomInfo = ViewInfo::Get(*pProject);

   std::shared_ptr<WaveTrack::Interval> leftInterval;
   std::shared_ptr<WaveTrack::Interval> rightInterval;

   //Test left and right boundaries of each clip
   //to determine which kind of adjustment is
   //more appropriate
   for(const auto& interval : waveTrack->Intervals())
   {
      const auto& clip = *interval->GetClip(0);
      if(!WaveChannelView::ClipDetailsVisible(clip, zoomInfo, rect))
         continue;

      auto clipRect = ClipParameters::GetClipRect(clip, zoomInfo, rect);
      if(std::abs(px - clipRect.GetLeft()) <= BoundaryThreshold * 2)
         rightInterval = interval;
      else if (std::abs(px - clipRect.GetRight()) <= BoundaryThreshold * 2)
         leftInterval = interval;
   }

   std::shared_ptr<WaveTrack::Interval> adjustedInterval;
   bool adjustLeftBorder {false};
   if (leftInterval && rightInterval)
   {
      //between adjacent clips
      if(ClipParameters::GetClipRect(*leftInterval->GetClip(0), zoomInfo, rect).GetRight() > px)
      {
         adjustedInterval = leftInterval;
         adjustLeftBorder = false;
      }
      else
      {
         adjustedInterval = rightInterval;
         adjustLeftBorder = true;
      }
   }
   else
   {
      adjustedInterval = leftInterval ? leftInterval : rightInterval;
      if (adjustedInterval)
      {
         //single clip case, determine the border,
         //hit testing area differs from one
         //used for general case
         const auto clipRect = ClipParameters::GetClipRect(*adjustedInterval->GetClip(0), zoomInfo, rect);
         if (std::abs(px - clipRect.GetLeft()) <= BoundaryThreshold)
            adjustLeftBorder = true;
         else if (std::abs(px - clipRect.GetRight()) <= BoundaryThreshold)
            adjustLeftBorder = false;
         else
            adjustedInterval.reset();
      }
   }

   if(adjustedInterval)
   {
      const auto isStretchMode = state.state.AltDown();
      AdjustClipBorder::AdjustHandler adjustHandler = isStretchMode
         ? (adjustLeftBorder ? StretchLeftTo : StretchRightTo)
         : (adjustLeftBorder ? TrimLeftTo : TrimRightTo);

      std::unique_ptr<AdjustPolicy> policy =
         std::make_unique<AdjustClipBorder>(
            adjustHandler, waveTrack, adjustedInterval, adjustLeftBorder,
            isStretchMode, zoomInfo);

      return AssignUIHandlePtr(
         holder,
         std::make_shared<WaveClipAdjustBorderHandle>(policy,
            isStretchMode,
            adjustLeftBorder));
   }
   return { };
}

UIHandlePtr WaveClipAdjustBorderHandle::HitTest(std::weak_ptr<WaveClipAdjustBorderHandle>& holder,
    WaveChannelView& view, const AudacityProject* pProject,
    const TrackPanelMouseState& state)
{
    auto waveTrack = std::dynamic_pointer_cast<WaveTrack>(view.FindTrack());
    //For multichannel tracks, show adjustment handle only for the leader track
    if (!waveTrack->IsLeader())
        return {};

    std::vector<UIHandlePtr> results;

    const auto rect = state.rect;

    auto py = state.state.m_y;

    if (py >= rect.GetTop() &&
        py <= (rect.GetTop() + static_cast<int>(rect.GetHeight() * 0.3)))
    {
        return HitAnywhere(holder, waveTrack, pProject, state);
    }
    return {};
}



HitTestPreview WaveClipAdjustBorderHandle::Preview(const TrackPanelMouseState& mouseState, AudacityProject* pProject)
{
   const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
   return mIsStretchMode
      ? HitPreviewStretch(pProject, unsafe, mIsLeftBorder)
      : HitPreviewTrim(pProject, unsafe, mIsLeftBorder);
}

UIHandle::Result WaveClipAdjustBorderHandle::Click
(const TrackPanelMouseEvent& event, AudacityProject* pProject)
{
   if (!ProjectAudioIO::Get(*pProject).IsAudioActive())
   {
      if (mAdjustPolicy->Init(event))
         return RefreshCode::RefreshNone;
   }
   return RefreshCode::Cancelled;
}

UIHandle::Result WaveClipAdjustBorderHandle::Drag
(const TrackPanelMouseEvent& event, AudacityProject* project)
{
   return mAdjustPolicy->Drag(event, *project);
}

UIHandle::Result WaveClipAdjustBorderHandle::Release
(const TrackPanelMouseEvent& event, AudacityProject* project,
    wxWindow* pParent)
{
   mAdjustPolicy->Finish(*project);
   return RefreshCode::RefreshAll;
}

UIHandle::Result WaveClipAdjustBorderHandle::Cancel(AudacityProject* pProject)
{
   mAdjustPolicy->Cancel();
   return RefreshCode::RefreshAll;
}

void WaveClipAdjustBorderHandle::Draw(TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass)
{
   mAdjustPolicy->Draw(context, rect, iPass);
}

wxRect WaveClipAdjustBorderHandle::DrawingArea(TrackPanelDrawingContext& context, const wxRect& rect,
   const wxRect& panelRect, unsigned iPass)
{
   return mAdjustPolicy->DrawingArea(context, rect, panelRect, iPass);
}
