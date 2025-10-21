/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "trackedit/dom/label.h"

#include "viewtrackitem.h"

namespace au::projectscene {
class TrackLabelItem : public ViewTrackItem
{
    Q_OBJECT

public:
    explicit TrackLabelItem(QObject* parent);

    void setLabel(const trackedit::Label& label);
};
}
