/*
* Audacity: A Digital Audio Editor
*/

#include "playregionmodel.h"

#include "playback/iplayer.h"

namespace au {
PlayRegionModel::PlayRegionModel(QObject* parent)
    : QObject{parent}
{
    playbackController()->loopRegionChanged().onNotify(this, [this]() {
        onLoopRegionChanged();
    });
}

double PlayRegionModel::start() const
{
    return m_start;
}

void PlayRegionModel::setStart(double newStart)
{
    if (muse::is_equal(m_start, newStart)) {
        return;
    }
    m_start = newStart;
    emit startChanged();
}

double PlayRegionModel::end() const
{
    return m_end;
}

void PlayRegionModel::setEnd(double newEnd)
{
    if (muse::is_equal(m_end, newEnd)) {
        return;
    }
    m_end = newEnd;
    emit endChanged();
}

bool PlayRegionModel::active() const
{
    return m_active;
}

void PlayRegionModel::setActive(bool newActive)
{
    if (m_active == newActive) {
        return;
    }
    m_active = newActive;
    emit activeChanged();
}

void PlayRegionModel::onLoopRegionChanged()
{
    const auto region = playbackController()->loopRegion();
    setStart(region.start);
    setEnd(region.end);
    setActive(playbackController()->isLoopRegionActive());
}
}
