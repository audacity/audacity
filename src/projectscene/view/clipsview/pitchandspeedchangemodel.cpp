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

    selectionController()->clipSelected().onReceive(this, [this](const trackedit::ClipKey& clipKey) {
        if (!clipKey.isValid() || m_clip.key == clipKey) {
            return;
        }

        trackedit::ITrackeditProjectPtr prj = trackeditProject();
        if (!prj) {
            return;
        }

        trackedit::Clip clip = clipKey.isValid() ? prj->clip({ clipKey.trackId, clipKey.clipId }) : trackedit::Clip();
        setClip(clip);
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

int PitchAndSpeedChangeModel::speedPercentage() const
{
    return std::round(100.0 / m_clip.speed);
}

void PitchAndSpeedChangeModel::setSpeedPercentage(int speedPercentage)
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

void PitchAndSpeedChangeModel::setClip(const trackedit::Clip& clip)
{
    m_clip = clip;

    emit pitchChanged();
    emit speedPercentageChanged();
    emit optimizeForVoiceChanged();
}
