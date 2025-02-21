/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbartimeitem.h"

#include <QVariantMap>

#include "playback/iaudiooutput.h"

static const int INVALID_FORMAT = -1;

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

    configuration()->playbackTimeItemFormatChanged().onNotify(this, [this](){
        emit currentFormatChanged();
    });
}

int PlaybackToolBarTimeItem::currentFormat() const
{
    return static_cast<int>(configuration()->playbackTimeItemFormat());
}

void PlaybackToolBarTimeItem::setCurrentFormat(int format)
{
    if (format == INVALID_FORMAT) {
        return;
    }

    if (currentFormat() == format) {
        return;
    }

    configuration()->setPlaybackTimeItemFormat(static_cast<TimecodeFormatType>(format));
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
