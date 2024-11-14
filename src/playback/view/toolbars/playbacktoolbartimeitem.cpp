/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbartimeitem.h"

#include <QVariantMap>

#include "playback/iaudiooutput.h"

using namespace au::playback;
using namespace au::audio;

PlaybackToolBarTimeItem::PlaybackToolBarTimeItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                                 QObject* parent)
    : muse::uicomponents::ToolBarItem(action, type, parent)
{
    playbackState()->playbackPositionChanged().onReceive(this, [this](muse::secs_t) {
        emit currentValueChanged();
    });

    playback()->audioOutput()->sampleRateChanged().onReceive(this, [this](audio::sample_rate_t) {
        emit sampleRateChanged();
    });

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this](){
        auto project = globalContext()->currentTrackeditProject();
        if (!project) {
            return;
        }

        emit timeSignatureChanged();
    });
}

int PlaybackToolBarTimeItem::currentFormat() const
{
    return m_currentFormat; // from settings
}

void PlaybackToolBarTimeItem::setCurrentFormat(int format)
{
    if (m_currentFormat == format) {
        return;
    }

    m_currentFormat = format;
    emit currentFormatChanged();
}

double PlaybackToolBarTimeItem::currentValue() const
{
    return playbackState()->playbackPosition();
}

void PlaybackToolBarTimeItem::setCurrentValue(double value)
{
    if (currentValue() == value) {
        return;
    }

    dispatcher()->dispatch("playback-seek", muse::actions::ActionData::make_arg1(value));
}

au::context::IPlaybackStatePtr PlaybackToolBarTimeItem::playbackState() const
{
    return globalContext()->playbackState();
}

double PlaybackToolBarTimeItem::sampleRate() const
{
    return playback()->audioOutput()->sampleRate();
}

double PlaybackToolBarTimeItem::tempo() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0.0;
    }

    return project->timeSignature().tempo;
}

int PlaybackToolBarTimeItem::upperTimeSignature() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0;
    }

    return project->timeSignature().upper;
}

int PlaybackToolBarTimeItem::lowerTimeSignature() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0.0;
    }

    return project->timeSignature().lower;
}
