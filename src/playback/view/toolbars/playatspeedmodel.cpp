/*
* Audacity: A Digital Audio Editor
*/
#include "playatspeedmodel.h"

#include <algorithm>

#include "framework/global/realfn.h"

#include "au3-audio-io/ProjectAudioIO.h"
#include "au3wrap/au3types.h"

using namespace au::playback;
using namespace au::au3;

PlayAtSpeedModel::PlayAtSpeedModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void PlayAtSpeedModel::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });

    playbackController()->isPlayAllowedChanged().onNotify(this, [this]() {
        emit isEnabledChanged();
    });

    onProjectChanged();
}

double PlayAtSpeedModel::speed() const
{
    return m_speed;
}

void PlayAtSpeedModel::setSpeed(double speed)
{
    speed = std::clamp(speed, MIN_SPEED, MAX_SPEED);

    if (muse::RealIsEqual(m_speed, speed)) {
        return;
    }

    m_speed = speed;
    emit speedChanged();

    auto project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    auto* au3Project = reinterpret_cast<Au3Project*>(project->au3ProjectPtr());
    if (!au3Project) {
        return;
    }

    ProjectAudioIO::Get(*au3Project).SetPlaySpeed(m_speed);
}

bool PlayAtSpeedModel::isEnabled() const
{
    return globalContext()->currentProject() != nullptr
           && playbackController()->isPlayAllowed();
}

void PlayAtSpeedModel::increaseSpeed()
{
    adjustSpeed(SPEED_STEP);
}

void PlayAtSpeedModel::decreaseSpeed()
{
    adjustSpeed(-SPEED_STEP);
}

void PlayAtSpeedModel::resetSpeed()
{
    setSpeed(DEFAULT_SPEED);
}

void PlayAtSpeedModel::adjustSpeed(double delta)
{
    setSpeed(m_speed + delta);
}

void PlayAtSpeedModel::onProjectChanged()
{
    m_speedSubscription.Reset();

    auto project = globalContext()->currentProject();
    if (!project) {
        m_speed = DEFAULT_SPEED;
        emit speedChanged();
        emit isEnabledChanged();
        return;
    }

    auto* au3Project = reinterpret_cast<Au3Project*>(project->au3ProjectPtr());
    if (!au3Project) {
        return;
    }

    auto& projectAudioIO = ProjectAudioIO::Get(*au3Project);

    // Ensure a sensible default if nothing has set speed yet.
    if (projectAudioIO.GetPlaySpeed() <= 0.0) {
        projectAudioIO.SetPlaySpeed(DEFAULT_SPEED);
    }

    m_speedSubscription = static_cast<Observer::Publisher<SpeedChangeMessage>&>(projectAudioIO)
                          .Subscribe([this](const SpeedChangeMessage&) {
        updateSpeedFromProject();
    });

    updateSpeedFromProject();
    emit isEnabledChanged();
}

void PlayAtSpeedModel::updateSpeedFromProject()
{
    auto project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    auto* au3Project = reinterpret_cast<Au3Project*>(project->au3ProjectPtr());
    if (!au3Project) {
        return;
    }

    const double speed = ProjectAudioIO::Get(*au3Project).GetPlaySpeed();
    if (muse::RealIsEqual(m_speed, speed)) {
        return;
    }

    m_speed = speed;
    emit speedChanged();
}
