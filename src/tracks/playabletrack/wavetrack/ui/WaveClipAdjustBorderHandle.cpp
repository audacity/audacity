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

namespace {

    std::vector<std::shared_ptr<WaveClip>> FindClipsInChannels(double start, double end, WaveTrack* track) {
        std::vector<std::shared_ptr<WaveClip>> result;
        for (auto channel : TrackList::Channels(track))
        {
            for (auto& clip : channel->GetClips())
            {
                if (clip->GetPlayStartTime() == start && clip->GetPlayEndTime() == end)
                    result.push_back(clip);
            }
        }
        return result;
    }

   void TrimLeftTo(WaveClip& clip, double t)
   {
      clip.TrimLeftTo(t);
   }

   void TrimRightTo(WaveClip& clip, double t)
   {
      clip.TrimRightTo(t);
   }

   void StretchLeftTo(WaveClip& clip, double t)
   {
      //clip.StretchLeftTo(t);
   }

   void StretchRightTo(WaveClip& clip, double t)
   {
      //clip.StretchRightTo(t);
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

class AdjustClipBorder final : public WaveClipAdjustBorderHandle::AdjustPolicy
{
public:
   using AdjustHandler = std::function<void(WaveClip&, double)>;

private:
   std::shared_ptr<WaveTrack> mTrack;
   std::vector<std::shared_ptr<WaveClip>> mClips;
   double mInitialBorderPosition{};
   int mDragStartX{ };
   std::pair<double, double> mRange;
   const bool mAdjustingLeftBorder;
   AdjustHandler mAdjustHandler;

   std::unique_ptr<SnapManager> mSnapManager;
   SnapResults mSnap;

   void TrimTo(double t) const
   {
      t = std::clamp(t, mRange.first, mRange.second);
      for(auto& clip : mClips)
         mAdjustHandler(*clip, t);
   }

   //Search for a good snap points among all tracks, including
   //the one to which the adjusted clip belongs, but not counting
   //the borders of adjusted clip
   static SnapPointArray FindSnapPoints(
      const WaveTrack* currentTrack, 
      WaveClip* adjustedClip,
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
         for(const auto track : as_const(*trackList).Leaders())
         {
            const auto isSameTrack = (track == currentTrack) ||
               (track->GetLinkType() == Track::LinkType::Aligned && *trackList->Find(currentTrack) == track) ||
               (currentTrack->GetLinkType() == Track::LinkType::Aligned && *trackList->Find(track) == currentTrack);
            for(const auto& interval : track->Intervals())
            {
               if(isSameTrack)
               {
                  auto waveTrackIntervalData =
                     std::dynamic_pointer_cast<const WaveTrack::Interval>(
                        interval);
                  if(waveTrackIntervalData->GetClip(0).get() == adjustedClip)
                  //exclude boundaries of the adjusted clip
                     continue;
               }
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
                    const std::shared_ptr<WaveTrack>& track, 
                    const std::shared_ptr<WaveClip>& clip, 
                    bool adjustLeftBorder, 
                    const ZoomInfo& zoomInfo)
      : mTrack(track)
      , mAdjustingLeftBorder(adjustLeftBorder)
      , mAdjustHandler(std::move(adjustHandler))
   {
      auto clips = track->GetClips();

      wxASSERT(std::find(clips.begin(), clips.end(), clip) != clips.end());

      if (track->IsAlignedWithLeader() || track->GetLinkType() == Track::LinkType::Aligned)
         //find clips in other channels which are also should be trimmed
         mClips = FindClipsInChannels(clip->GetPlayStartTime(), clip->GetPlayEndTime(), track.get());
      else
         mClips.push_back(clip);

      if (mAdjustingLeftBorder)
      {
         auto left = clip->GetSequenceStartTime();
         for (const auto& other : clips)
            if (other->GetPlayStartTime() < clip->GetPlayStartTime() && other->GetPlayEndTime() > left)
               left = other->GetPlayEndTime();
         //not less than 1 sample length
         mRange = std::make_pair(left, clip->GetPlayEndTime() - 1.0 / clip->GetRate());

         mInitialBorderPosition = mClips[0]->GetPlayStartTime();
      }
      else
      {
         auto right = clip->GetSequenceEndTime();
         for (const auto& other : clips)
            if (other->GetPlayStartTime() > clip->GetPlayStartTime() && other->GetPlayStartTime() < right)
               right = other->GetPlayStartTime();
         //not less than 1 sample length
         mRange = std::make_pair(clip->GetPlayStartTime() + 1.0 / clip->GetRate(), right);

         mInitialBorderPosition = mClips[0]->GetPlayEndTime();
      }

      if(const auto trackList = track->GetOwner())
      {
         mSnapManager = std::make_unique<SnapManager>(
            *trackList->GetOwner(),
            FindSnapPoints(track.get(), clip.get(), mRange),
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

      const auto eventT = viewInfo.PositionToTime(viewInfo.TimeToPosition(mInitialBorderPosition, event.rect.x) + dx, event.rect.x);

      const auto offset = sampleCount(floor((eventT - mInitialBorderPosition) * mClips[0]->GetRate())).as_double() / mClips[0]->GetRate();
      const auto t = std::clamp(mInitialBorderPosition + offset, mRange.first, mRange.second);
      const auto wasSnapped = mSnap.Snapped();
      if(mSnapManager)
         mSnap = mSnapManager->Snap(mTrack.get(), t, !mAdjustingLeftBorder);
      if(mSnap.Snapped())
      {
         if(mSnap.outTime >= mRange.first && mSnap.outTime <= mRange.second)
         {
            //Make sure that outTime belongs to the adjustment range after snapping
            TrimTo(mSnap.outTime);
            return RefreshCode::RefreshAll;
         }
         else
         {
            //Otherwise snapping cannot be performed
            mSnap = {};
            TrimTo(t);
         }
      }
      else
         TrimTo(t);
      //If there was a snap line, make sure it is removed
      //from the screen by redrawing whole TrackPanel
      return wasSnapped ? RefreshCode::RefreshAll : RefreshCode::RefreshCell;
   }

   void Finish(AudacityProject& project) override
   {
      if (mClips[0]->GetPlayStartTime() != mInitialBorderPosition)
      {
         if (mAdjustingLeftBorder)
         {
            auto dt = std::abs(mClips[0]->GetPlayStartTime() - mInitialBorderPosition);
            ProjectHistory::Get(project).PushState(XO("Clip-Trim-Left"),
                XO("Moved by %.02f").Format(dt));
         }
         else
         {
            auto dt = std::abs(mInitialBorderPosition - mClips[0]->GetPlayEndTime());
            ProjectHistory::Get(project).PushState(XO("Clip-Trim-Right"),
                XO("Moved by %.02f").Format(dt));
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

class AdjustBetweenBorders final : public WaveClipAdjustBorderHandle::AdjustPolicy
{
   std::pair<double, double> mRange;
   std::vector<std::shared_ptr<WaveClip>> mLeftClips;
   std::vector<std::shared_ptr<WaveClip>> mRightClips;
   double mInitialBorderPosition{};
   int mDragStartX{ };

   void TrimTo(double t)
   {
      t = std::clamp(t, mRange.first, mRange.second);

      for (auto& clip : mLeftClips)
         clip->TrimRightTo(t);
      for (auto& clip : mRightClips)
         clip->TrimLeftTo(t);
   }

public:
   AdjustBetweenBorders(
      WaveTrack* track, 
      std::shared_ptr<WaveClip>& leftClip, 
      std::shared_ptr<WaveClip>& rightClip)
   {
      auto clips = track->GetClips();

      wxASSERT(std::find(clips.begin(), clips.end(), leftClip) != clips.end());
      wxASSERT(std::find(clips.begin(), clips.end(), rightClip) != clips.end());

      if (track->IsAlignedWithLeader() || track->GetLinkType() == Track::LinkType::Aligned)
      {
         //find clips in other channels whose border should be also adjusted
         mLeftClips = FindClipsInChannels(leftClip->GetPlayStartTime(), leftClip->GetPlayEndTime(), track);
         mRightClips = FindClipsInChannels(rightClip->GetPlayStartTime(), rightClip->GetPlayEndTime(), track);
      }
      else
      {
         mLeftClips.push_back(leftClip);
         mRightClips.push_back(rightClip);
      }

      mRange = std::make_pair(
         //not less than 1 sample length
         mLeftClips[0]->GetPlayStartTime() + 1.0 / mLeftClips[0]->GetRate(),
         mRightClips[0]->GetPlayEndTime() - 1.0 / mRightClips[0]->GetRate()
      );
      mInitialBorderPosition = mRightClips[0]->GetPlayStartTime();
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
      const auto newX = event.event.GetX();
      const auto dx = newX - mDragStartX;

      auto& viewInfo = ViewInfo::Get(project);

      auto eventT = viewInfo.PositionToTime(viewInfo.TimeToPosition(mInitialBorderPosition, event.rect.x) + dx, event.rect.x);
      auto offset = sampleCount(
         floor(
            (eventT - mInitialBorderPosition) * mLeftClips[0]->GetRate()
         )
      ).as_double() / mLeftClips[0]->GetRate();
      
      TrimTo(mInitialBorderPosition + offset);

      return RefreshCode::RefreshCell;
   }

   void Finish(AudacityProject& project) override
   {
      if (mRightClips[0]->GetPlayStartTime() != mInitialBorderPosition)
      {
         auto dt = std::abs(mRightClips[0]->GetPlayStartTime() - mInitialBorderPosition);
         ProjectHistory::Get(project).PushState(XO("Clip-Trim-Between"),
               XO("Moved by %.02f").Format(dt));
      }
   }

   void Cancel() override
   {
      TrimTo(mInitialBorderPosition);
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
    const auto rect = state.rect;

    auto px = state.state.m_x;

    auto& zoomInfo = ViewInfo::Get(*pProject);

    std::shared_ptr<WaveClip> leftClip;
    std::shared_ptr<WaveClip> rightClip;

    //Test left and right boundaries of each clip
    //to determine which kind of adjustment is
    //more appropriate
    for (auto& clip : waveTrack->GetClips())
    {
        if (!WaveChannelView::ClipDetailsVisible(*clip, zoomInfo, rect))
           continue;

        auto clipRect = ClipParameters::GetClipRect(*clip.get(), zoomInfo, rect);
        
        //double the hit testing area in case if clip are close to each other
        if (std::abs(px - clipRect.GetLeft()) <= BoundaryThreshold * 2)
           rightClip = clip;
        else if (std::abs(px - clipRect.GetRight()) <= BoundaryThreshold * 2)
           leftClip = clip;
    }
    
    std::shared_ptr<WaveClip> adjustedClip;
    bool adjustLeftBorder {false};
    if (leftClip && rightClip)
    {
       //between adjacent clips
       if(ClipParameters::GetClipRect(*leftClip, zoomInfo, rect).GetRight() > px)
       {
          adjustedClip = leftClip;
          adjustLeftBorder = false;
       }
       else
       {
          adjustedClip = rightClip;
          adjustLeftBorder = true;
       }
    }
    else
    {
       adjustedClip = leftClip ? leftClip : rightClip;
       if (adjustedClip)
       {
          //single clip case, determine the border,
          //hit testing area differs from one
          //used for general case
          const auto clipRect = ClipParameters::GetClipRect(*adjustedClip, zoomInfo, rect);
          if (std::abs(px - clipRect.GetLeft()) <= BoundaryThreshold)
             adjustLeftBorder = true;
          else if (std::abs(px - clipRect.GetRight()) <= BoundaryThreshold)
             adjustLeftBorder = false;
          else
             adjustedClip.reset();
       }
    }

    if(adjustedClip)
    {
       const auto isStretchMode = state.state.AltDown();
       AdjustClipBorder::AdjustHandler adjustHandler = isStretchMode
          ? (adjustLeftBorder ? StretchLeftTo : StretchRightTo)
          : (adjustLeftBorder ? TrimLeftTo : TrimRightTo);

       std::unique_ptr<AdjustPolicy> policy =
          std::make_unique<AdjustClipBorder>(
             adjustHandler,
             waveTrack,
             adjustedClip,
             adjustLeftBorder,
             zoomInfo);

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
    if (!waveTrack->IsLeader() && waveTrack->IsAlignedWithLeader())
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
