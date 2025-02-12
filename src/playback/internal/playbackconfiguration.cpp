/*
* Audacity: A Digital Audio Editor
*/
#include "playbackconfiguration.h"

static const QString PLAYBACK_TIME_ITEM_FORMAT("playbackToolbar/playbackTimeItemFormat");

using namespace muse;
using namespace au::playback;

draw::Color PlaybackConfiguration::playColor() const
{
    return "#18A999";
}

TimecodeFormatType PlaybackConfiguration::playbackTimeItemFormat() const
{
    TimecodeFormatType result = TimecodeFormatType::HHMMSSHundredths;

    QString formatStr = uiConfiguration()->uiItemState(PLAYBACK_TIME_ITEM_FORMAT);
    if (!formatStr.isEmpty()) {
        result = static_cast<TimecodeFormatType>(formatStr.toInt());
    }

    return result;
}

void PlaybackConfiguration::setPlaybackTimeItemFormat(TimecodeFormatType format)
{
    uiConfiguration()->setUiItemState(PLAYBACK_TIME_ITEM_FORMAT, QString::number(static_cast<int>(format)));
}

async::Notification PlaybackConfiguration::playbackTimeItemFormatChanged() const
{
    return uiConfiguration()->uiItemStateChanged(PLAYBACK_TIME_ITEM_FORMAT);
}
