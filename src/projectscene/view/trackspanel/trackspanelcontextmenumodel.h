/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

namespace au::projectscene {
//! The context menu of the empty part of the track header panel
class TracksPanelContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

public:
    Q_INVOKABLE void load() override;
};
}
