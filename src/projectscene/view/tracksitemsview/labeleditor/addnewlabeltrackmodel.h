/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <qqmlintegration.h>

#include "modularity/ioc.h"
#include "trackedit/itrackeditinteraction.h"

namespace au::projectscene {
class AddNewLabelTrackModel : public QObject, public muse::Injectable
{
    Q_OBJECT
    QML_ELEMENT;

    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction = { this };

public:
    explicit AddNewLabelTrackModel(QObject* parent = nullptr);

    Q_INVOKABLE trackedit::TrackId createLabelTrack(const QString& trackName);
};
}
