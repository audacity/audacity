/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveClipTrimHandle.cpp

 Vitaly Sverchinsky

 **********************************************************************/

#include "WaveClipTrimHandle.h"
#include "ProjectAudioIO.h"
#include "RefreshCode.h"


#include "../../../images/Cursors.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "WaveTrackView.h"
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
}

WaveClipTrimHandle::ClipTrimPolicy::~ClipTrimPolicy() { }

class WaveClipTrimHandle::AdjustBorder : public WaveClipTrimHandle::ClipTrimPolicy
{
   std::vector<std::shared_ptr<WaveClip>> mClips;
   double mInitialBorderPosition{};
   int mDragStartX{ };
   std::pair<double, double> mRange;
   bool mAdjustingLeftBorder;

   void TrimTo(double t)
   {
      t = std::clamp(t, mRange.first, mRange.second);
      if (mAdjustingLeftBorder)
      {
         for (auto& clip : mClips)
            clip->TrimLeftTo(t);
      }
      else
      {
         for (auto& clip : mClips)
            clip->TrimRightTo(t);
      }
   }

public:
   AdjustBorder(WaveTrack* track, std::shared_ptr<WaveClip>& clip, bool left)
      : mAdjustingLeftBorder(left)
   {
      auto clips = track->GetClips();

      wxASSERT(std::find(clips.begin(), clips.end(), clip) != clips.end());

      if (track->IsAlignedWithLeader() || track->GetLinkType() == Track::LinkType::Aligned)
         //find clips in other channels which are also should be trimmed
         mClips = FindClipsInChannels(clip->GetPlayStartTime(), clip->GetPlayEndTime(), track);
      else
         mClips.push_back(clip);

      if (mAdjustingLeftBorder)
      {
         auto left = -std::numeric_limits<double>::infinity();
         for (auto& other : clips)
            if (other->GetPlayStartTime() < clip->GetPlayStartTime() && other->GetPlayEndTime() > left)
            {
               auto maxOffset = clip->SamplesToTime(clip->TimeToSamples(clip->GetPlayStartTime() - other->GetPlayEndTime()));
               left = clip->GetPlayStartTime() - maxOffset;
            }
         //not less than 1 sample length
         mRange = std::make_pair(left, clip->GetPlayEndTime() - 1.0 / clip->GetRate());

         mInitialBorderPosition = mClips[0]->GetPlayStartTime();
      }
      else
      {
         auto right = std::numeric_limits<double>::infinity();
         for (auto& other : clips)
            if (other->GetPlayStartTime() > clip->GetPlayStartTime() && other->GetPlayStartTime() < right)
            {
               auto maxOffset = clip->SamplesToTime(clip->TimeToSamples(other->GetPlayStartTime() - clip->GetPlayEndTime()));
               right = clip->GetPlayEndTime() + maxOffset;
            }
         //not less than 1 sample length
         mRange = std::make_pair(clip->GetPlayStartTime() + 1.0 / clip->GetRate(), right);

         mInitialBorderPosition = mClips[0]->GetPlayEndTime();
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

   void Trim(const TrackPanelMouseEvent& event, AudacityProject& project) override
   {
      const auto newX = event.event.GetX();
      const auto dx = newX - mDragStartX;

      auto& viewInfo = ViewInfo::Get(project);

      auto eventT = viewInfo.PositionToTime(viewInfo.TimeToPosition(mInitialBorderPosition, event.rect.x) + dx, event.rect.x);
      auto offset = sampleCount(floor((eventT - mInitialBorderPosition) * mClips[0]->GetRate())).as_double() / mClips[0]->GetRate();
      auto t = std::clamp(mInitialBorderPosition + offset, mRange.first, mRange.second);

      TrimTo(t);
   }

   void Finish(AudacityProject& project) override
   {
      if (mClips[0]->GetPlayStartTime() != mInitialBorderPosition)
      {
         if (mAdjustingLeftBorder)
         {
            auto dt = std::abs(mClips[0]->GetPlayStartTime() - mInitialBorderPosition);
            ProjectHistory::Get(project).PushState(XO("Clip-Trim-Left"),
                XO("Moved by %.02f").Format(dt), UndoPush::CONSOLIDATE);
         }
         else
         {
            auto dt = std::abs(mClips[0]->GetPlayEndTime() - mInitialBorderPosition);
            ProjectHistory::Get(project).PushState(XO("Clip-Trim-Right"),
                XO("Moved by %.02f").Format(dt), UndoPush::CONSOLIDATE);
         }
      }
   }

   void Cancel() override
   {
      TrimTo(mInitialBorderPosition);
   }
};


HitTestPreview WaveClipTrimHandle::HitPreview(const AudacityProject*, bool unsafe)
{
    static auto disabledCursor =
        ::MakeCursor(wxCURSOR_NO_ENTRY, DisabledCursorXpm, 16, 16);
    static auto slideCursor =
        MakeCursor(wxCURSOR_SIZEWE, TimeCursorXpm, 16, 16);
    auto message = XO("Click and drag to move clip boundary in time");

    return {
       message,
       (unsafe
        ? &*disabledCursor
        : &*slideCursor)
    };
}


WaveClipTrimHandle::WaveClipTrimHandle(std::unique_ptr<ClipTrimPolicy>& clipTrimPolicy)
   : mClipTrimPolicy(std::move(clipTrimPolicy))
{
}

UIHandlePtr WaveClipTrimHandle::HitAnywhere(std::weak_ptr<WaveClipTrimHandle>& holder, WaveTrack* waveTrack, const AudacityProject* pProject, const TrackPanelMouseState& state)
{
    const auto rect = state.rect;

    auto px = state.state.m_x;

    auto& zoomInfo = ViewInfo::Get(*pProject);

    std::unique_ptr<ClipTrimPolicy> clipTrimPolicy;

    //Test left and right boundaries of each clip
    //and determine the maximum offsets allowed for trimming,
    //which are constrained either by other clips, own length,
    //or another own edge
    for (auto& clip : waveTrack->GetClips())
    {
        if (!WaveTrackView::ClipDetailsVisible(*clip, zoomInfo, rect))
           continue;

        auto clipRect = ClipParameters::GetClipRect(*clip.get(), zoomInfo, rect);
        
        if (std::abs(px - clipRect.GetLeft()) <= BoundaryThreshold)
        {
           clipTrimPolicy = std::make_unique<AdjustBorder>(waveTrack, clip, true);
           return AssignUIHandlePtr(
              holder,
              std::make_shared<WaveClipTrimHandle>(clipTrimPolicy)
           );
        }
        else if (std::abs(px - clipRect.GetRight()) <= BoundaryThreshold)
        {
           clipTrimPolicy = std::make_unique<AdjustBorder>(waveTrack, clip, false);
           return AssignUIHandlePtr(
              holder,
              std::make_shared<WaveClipTrimHandle>(clipTrimPolicy)
           );
        }
    }

    return {};
}

UIHandlePtr WaveClipTrimHandle::HitTest(std::weak_ptr<WaveClipTrimHandle>& holder,
    WaveTrackView& view, const AudacityProject* pProject,
    const TrackPanelMouseState& state)
{
    auto waveTrack = std::dynamic_pointer_cast<WaveTrack>(view.FindTrack()->SubstitutePendingChangedTrack());
    //For multichannel tracks, show trim handle only for the leader track
    if (!waveTrack->IsLeader() && waveTrack->IsAlignedWithLeader())
        return {};

    std::vector<UIHandlePtr> results;

    const auto rect = state.rect;

    auto px = state.state.m_x;
    auto py = state.state.m_y;

    if (py >= rect.GetTop() && 
        py <= (rect.GetTop() + static_cast<int>(rect.GetHeight() * 0.3)))
    {
        return HitAnywhere(holder, waveTrack.get(), pProject, state);
    }
    return {};
}



HitTestPreview WaveClipTrimHandle::Preview(const TrackPanelMouseState& mouseState, AudacityProject* pProject)
{
    const bool unsafe = ProjectAudioIO::Get(*pProject).IsAudioActive();
    return HitPreview(pProject, unsafe);
}

UIHandle::Result WaveClipTrimHandle::Click
(const TrackPanelMouseEvent& event, AudacityProject* pProject)
{
   if (mClipTrimPolicy->Init(event))
      return RefreshCode::RefreshNone;
   return RefreshCode::Cancelled;
}

UIHandle::Result WaveClipTrimHandle::Drag
(const TrackPanelMouseEvent& event, AudacityProject* project)
{
   mClipTrimPolicy->Trim(event, *project);
   return RefreshCode::RefreshCell;
}

UIHandle::Result WaveClipTrimHandle::Release
(const TrackPanelMouseEvent& event, AudacityProject* project,
    wxWindow* pParent)
{
   mClipTrimPolicy->Finish(*project);
   return RefreshCode::RefreshNone;
}

UIHandle::Result WaveClipTrimHandle::Cancel(AudacityProject* pProject)
{
   mClipTrimPolicy->Cancel();
   return RefreshCode::RefreshCell;
}
