/*
* Audacity: A Digital Audio Editor
*/
#include "pitchandspeedchangemodel.h"

using namespace au::projectscene;

PitchAndSpeedChangeModel::PitchAndSpeedChangeModel(QObject* parent)
    : QObject(parent)
{
}

void PitchAndSpeedChangeModel::load(const QString& trackIdStr, const QString& clipIdStr)
{
    trackedit::ITrackeditProjectPtr prj = trackeditProject();
    if (!prj) {
        return;
    }

    trackedit::TrackId trackId = trackIdStr.toLong();
    trackedit::ClipId clipId = clipIdStr.toULongLong();

    trackedit::Clip clip = prj->clip({ trackId, clipId });
    if (!clip.isValid()) {
        return;
    }

    prj->clipList(trackId).onItemChanged(this, [this](const trackedit::Clip& clip) {
        if (m_clip.key != clip.key) {
            return;
        }

        setClip(clip);
    });

    prj->clipList(trackId).onItemRemoved(this, [this](const trackedit::Clip& clip) {
        if (m_clip.key != clip.key) {
            return;
        }

        emit closeDialogRequested();
    });

    selectionController()->clipsSelected().onReceive(this, [this](const trackedit::ClipKeyList& clipKeyList) {
        if (clipKeyList.empty()) {
            return;
        }

        trackedit::ClipKey clipKey = clipKeyList.at(0);

        if (m_clip.key == clipKey) {
            return;
        }

        trackedit::ITrackeditProjectPtr prj = trackeditProject();
        if (!prj) {
            return;
        }

        trackedit::Clip clip = clipKey.isValid() ? prj->clip({ clipKey.trackId, clipKey.clipId }) : trackedit::Clip();
        setClip(clip);
    });

    playbackState()->playbackStatusChanged().onReceive(this, [this](playback::PlaybackStatus){
        emit canChangeSpeedChanged();
    });

    setClip(clip);
}

int PitchAndSpeedChangeModel::pitch() const
{
    return m_clip.pitch;
}

void PitchAndSpeedChangeModel::setPitch(int pitch)
{
    if (!m_clip.isValid() || m_clip.pitch == pitch) {
        return;
    }

    trackeditInteraction()->changeClipPitch(m_clip.key, pitch);
}

double PitchAndSpeedChangeModel::speedPercentage() const
{
    return 100.0 / m_clip.speed;
}

void PitchAndSpeedChangeModel::setSpeedPercentage(double speedPercentage)
{
    double speed = 100.0 / speedPercentage;
    if (!m_clip.isValid() || muse::RealIsEqual(m_clip.speed, speed)) {
        return;
    }

    trackeditInteraction()->changeClipSpeed(m_clip.key, speed);
}

bool PitchAndSpeedChangeModel::optimizeForVoice() const
{
    return m_clip.optimizeForVoice;
}

void PitchAndSpeedChangeModel::setOptimizeForVoice(bool optimize)
{
    if (!m_clip.isValid() || m_clip.optimizeForVoice == optimize) {
        return;
    }

    trackeditInteraction()->changeClipOptimizeForVoice(m_clip.key, optimize);
}

au::trackedit::ITrackeditProjectPtr PitchAndSpeedChangeModel::trackeditProject() const
{
    return globalContext()->currentTrackeditProject();
}

au::context::IPlaybackStatePtr PitchAndSpeedChangeModel::playbackState() const
{
    return globalContext()->playbackState();
}

void PitchAndSpeedChangeModel::setClip(const trackedit::Clip& clip)
{
    m_clip = clip;

    emit clipTitleChanged();
    emit pitchChanged();
    emit speedPercentageChanged();
    emit optimizeForVoiceChanged();
}

bool PitchAndSpeedChangeModel::canChangeSpeed() const
{
    context::IPlaybackStatePtr playbackState = this->playbackState();
    if (!playbackState) {
        return false;
    }

    return playbackState->playbackStatus() != playback::PlaybackStatus::Running;
}

QString PitchAndSpeedChangeModel::clipTitle() const
{
    if (!m_clip.isValid()) {
        return QString();
    }

    return m_clip.title;
}
