#include "WaveTrackView.h"

#include <QPainter>
#include <unordered_set>

#include "WaveTrack.h"
#include "WaveClip.h"
#include "SelectedRegion.h"
#include "SyncLock.h"
#include "TimelineContext.h"
#include "ViewInfo.h"

WaveTrackView::WaveTrackView(QQuickItem* parent)
   : TimelineView(parent)
{
}

WaveTrackView::~WaveTrackView() = default;

WaveTrackAdapter* WaveTrackView::getTrack() const
{
   return mTrack;
}

void WaveTrackView::setTrack(WaveTrackAdapter* track)
{
   if(mTrack == track)
      return;

   if(mTrack != nullptr)
   {
      mTrackEventsSubscription.Reset();
      disconnect(mTrack, nullptr, this, nullptr);
   }
   if(track != nullptr)
   {
      mTrackEventsSubscription =
         track->GetAsWaveTrack()->Subscribe(*this, &WaveTrackView::OnWaveTrackClipEvent);

      //requests repaint
      connect(track, &WaveTrackAdapter::muteChanged, this, &WaveTrackView::OnMuteChanged);
      connect(track, &WaveTrackAdapter::soloChanged, this, &WaveTrackView::OnSoloChanged);
   }
   ResetItemsCache();
   mTrack = track;

   mClipItems.clear();
   mClipItemsPool.clear();

   update();
}

void WaveTrackView::UpdateItemsCache(TimelineContext& trackPanelView)
{
   assert(mTrack != nullptr);

   const auto track = mTrack->GetAsWaveTrack();

   const auto viewRect = QRect(
      0,
      0,
      static_cast<int>(width()),
      static_cast<int>(height()));

   const ViewInfo viewInfo(trackPanelView.offset(),  trackPanelView.zoom());

   std::unordered_set<const WaveClip*> usedKeys;
   for(const auto& interval : track->Intervals())
   {
      const auto key = interval.get();
      //If clip is "too small" draw a placeholder instead of
      //attempting to fit the contents into a few pixels
      if (!WaveClipItem::ClipDetailsVisible(*key, viewInfo, viewRect))
         continue;

      const auto left = (interval->GetPlayStartTime() - trackPanelView.offset()) * trackPanelView.zoom();
      const auto width = (interval->GetPlayEndTime() - interval->GetPlayStartTime()) * trackPanelView.zoom();

      if(left >= viewRect.right() || left + width <= viewRect.left())
         continue;

      usedKeys.insert(key);

      if(mClipItems.find(key) != mClipItems.end())
         continue;//already in use

      //take from the pool or create a new one
      std::unique_ptr<WaveClipItem> drawable;
      if(!mClipItemsPool.empty())
      {
         drawable = std::move(mClipItemsPool.back());
         mClipItemsPool.pop_back();
      }
      else
         drawable = std::make_unique<WaveClipItem>(*mTrack->GetAsWaveTrack());
      
      drawable->SetInterval(interval);
      ItemAdded(drawable.get());

      mClipItems[key] = std::move(drawable);
   }
   //erase items that aren't on the screen any more 
   for(auto it = mClipItems.begin(); it != mClipItems.end();)
   {
      if(usedKeys.find(it->first) != usedKeys.end())
      {
         ++it;
         continue;
      }
      ItemRemoved(it->second.get());
      mClipItemsPool.push_back(std::move(it->second));
      it = mClipItems.erase(it);
   }
}

void WaveTrackView::OnWaveTrackClipEvent(WaveTrackMessage)
{
   //TODO: add/remove individual clip, don't reset the whole cache
   ResetItemsCache();
}

void WaveTrackView::OnMuteChanged(bool mute)
{
   update();
}

void WaveTrackView::OnSoloChanged(bool solo)
{
   update();
}

