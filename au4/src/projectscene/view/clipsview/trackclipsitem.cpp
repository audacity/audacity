#include "trackclipsitem.h"

using namespace au::projectscene;

TrackClipsItem::TrackClipsItem(QObject* parent)
    : QObject(parent)
{}

QList<ClipItem*> TrackClipsItem::clips() const
{
    return m_clips;
}

void TrackClipsItem::setClips(QList<ClipItem*> list)
{
    m_clips = list;
    emit clipsChanged();
}
