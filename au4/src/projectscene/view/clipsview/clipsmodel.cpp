#include "clipsmodel.h"

using namespace au::projectscene;
using namespace au::processing;

void ClipsModel::load()
{
    ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    // if (!prj) {
    //     return;
    // }
    // for (const Track& t : prj->trackList()) {
    //     TrackClipsItem* titem = new TrackClipsItem(this);
    //     QList<ClipItem*> clips;
    //     for (const Clip& c : t.clips()) {
    //         ClipItem* citem = new ClipItem(titem);
    //         citem->setWaveSource(WaveSource(c.wave()));
    //         clips << citem;
    //     }

    //     titem->setClips(clips);

    //     m_tracks << titem;
    // }

    emit tracksChanged();
}

QList<TrackClipsItem*> ClipsModel::tracks() const
{
    return m_tracks;
}

void ClipsModel::setTracks(const QList<TrackClipsItem*>& newTracks)
{
    if (m_tracks == newTracks) {
        return;
    }
    m_tracks = newTracks;
    emit tracksChanged();
}
