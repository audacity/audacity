/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/view/abstractmenumodel.h"
#include "playback/playbacktypes.h"

namespace au::playback {
class PlaybackMeterDbRangeModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

public:
    explicit PlaybackMeterDbRangeModel(QObject* parent = nullptr);

    void load() override;
    const char* description(PlaybackMeterDbRange::DbRange range) const;
    const char* action(PlaybackMeterDbRange::DbRange range) const;
    std::optional<PlaybackMeterDbRange::DbRange> rangeFromAction(const std::string& action) const;
};
}
