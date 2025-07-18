/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/view/abstractmenumodel.h"

namespace au::playback {
class PlaybackMeterDbRangeModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

public:
    explicit PlaybackMeterDbRangeModel(QObject* parent = nullptr);

    void load() override;
};
}
