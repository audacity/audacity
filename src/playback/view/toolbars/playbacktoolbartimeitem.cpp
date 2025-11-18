/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbartimeitem.h"

#include <QVariantMap>

#include "playback/iaudiooutput.h"

static const int INVALID_FORMAT = -1;

using namespace au::playback;

static const muse::actions::ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");

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

    configuration()->setPlaybackTimeItemFormat(static_cast<au::uicomponents::TimecodeFormatType>(format));
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
    muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
    q.addParam("seekTime", muse::Val(value));
    q.addParam("triggerPlay", muse::Val(false));
    dispatcher()->dispatch(q);
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
