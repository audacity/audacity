#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "processing/processingtypes.h"

#include "TimelineView.h"
#include "WaveTrack.h"

#include "items/WaveClipItem.h"

class WaveTrackView : public TimelineView
{
    Q_OBJECT

    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId)

    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    WaveTrackView(QQuickItem* parent = nullptr);
    ~WaveTrackView() override;

    QVariant trackId() const { return QVariant::fromValue(m_trackId); }
    void setTrackId(QVariant trackId);

private:
    void UpdateItemsCache(TimelineContext& trackPanelView) override;

private:

    WaveTrack* waveTrack(const au::processing::TrackId& trackId) const;

    au::processing::TrackId m_trackId = -1;
    WaveTrack* m_waveTrack = nullptr;

    std::unordered_map<const WaveClip*, std::unique_ptr<WaveClipItem>> mClipItems;
    std::vector<std::unique_ptr<WaveClipItem>> mClipItemsPool;
};
