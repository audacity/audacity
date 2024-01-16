#pragma once

#include "TimelineView.h"
#include "WaveTrack.h"
#include "WaveTrackAdapter.h"

#include "items/WaveClipItem.h"

class WaveTrackView : public TimelineView
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(WaveTrackAdapter* track READ getTrack WRITE setTrack)

   WaveTrackAdapter* mTrack{};
   Observer::Subscription mTrackEventsSubscription;

   std::unordered_map<const WaveClip*, std::unique_ptr<WaveClipItem>> mClipItems;
   std::vector<std::unique_ptr<WaveClipItem>> mClipItemsPool;
public:
   WaveTrackView(QQuickItem* parent = nullptr);
   ~WaveTrackView() override;

   WaveTrackAdapter* getTrack() const;
   void setTrack(WaveTrackAdapter* track);

private:

   void UpdateItemsCache(TimelineContext& trackPanelView) override;

   void OnWaveTrackClipEvent(WaveTrackClipEvent);

private slots:

   void OnMuteChanged(bool mute);
   void OnSoloChanged(bool solo);

};
