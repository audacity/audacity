/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "iglobalconfiguration.h"

namespace au::projectscene {
class AddNewTrackPopupModel : public QObject
{
    Q_OBJECT

    muse::GlobalInject<muse::IGlobalConfiguration> globalConfiguration;

public:
    explicit AddNewTrackPopupModel(QObject* parent = nullptr);

    Q_INVOKABLE bool isAddLabelAvailable() const;
};
}
