#include "WaveTrackView.h"

#include <QPainter>
#include <unordered_set>

#include "WaveTrack.h"
#include "WaveClip.h"
#include "SelectedRegion.h"
#include "SyncLock.h"
#include "TimelineContext.h"
#include "ViewInfo.h"

#include "log.h"

WaveTrackView::WaveTrackView(QQuickItem* parent)
    : TimelineView(parent)
{
}

WaveTrackView::~WaveTrackView() = default;

void WaveTrackView::setTrackId(QVariant _trackId)
{
    au::processing::TrackId trackId = _trackId.toInt();
    if(m_trackId == trackId) {
        return;
    }

    ResetItemsCache();

    m_trackId = trackId;
    m_waveTrack = waveTrack(m_trackId);
    LOGDA() << "m_trackId: " << m_trackId << " m_waveTrack: " << m_waveTrack;

    mClipItems.clear();
    mClipItemsPool.clear();

    update();
}

WaveTrack* WaveTrackView::waveTrack(const au::processing::TrackId &trackId) const
{
    au::project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return nullptr;
    }

    AudacityProject* project = reinterpret_cast<AudacityProject*>(prj->au3ProjectPtr());

    TrackId au3TrackId = TrackId(trackId);
    Track* track = nullptr;
    TrackList& tracks = TrackList::Get(*project);
    for ( Track* t : tracks) {
        if (t->GetId() == au3TrackId) {
            track = t;
            break;
        }
    }
    WaveTrack* waveTrack = dynamic_cast<WaveTrack*>(track);
    return waveTrack;
}

void WaveTrackView::UpdateItemsCache(TimelineContext& trackPanelView)
{
    LOGDA() << "m_trackId: " << m_trackId << " m_waveTrack: " << m_waveTrack;
    assert(m_waveTrack != nullptr);

    const auto viewRect = QRect(
                0,
                0,
                static_cast<int>(width()),
                static_cast<int>(height()));

    const ViewInfo viewInfo(trackPanelView.offset(),  trackPanelView.zoom());

    std::unordered_set<const WaveClip*> usedKeys;
    for(const auto& interval : m_waveTrack->Intervals())
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
            drawable = std::make_unique<WaveClipItem>(*m_waveTrack);

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


