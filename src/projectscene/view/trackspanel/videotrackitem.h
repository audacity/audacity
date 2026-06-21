/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "trackitem.h"

namespace au::projectscene {
class VideoTrackItem : public TrackItem
{
    Q_OBJECT

public:
    explicit VideoTrackItem(QObject* parent = nullptr);

protected:
    bool isAudible() const override;
};
}
