/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QtQml/qqmlregistration.h>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "importexport/export/iexportconfiguration.h"
#include "preferences/types/preferencestypes.h"
#include "project/iprojectconfiguration.h"

namespace au::appshell {
class ExportPreferencesPageModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT

    muse::GlobalInject<au::importexport::IExportConfiguration> exportConfiguration;
    muse::GlobalInject<au::project::IProjectConfiguration> projectConfiguration;

    Q_PROPERTY(
        bool askExportLocationType READ askExportLocationType WRITE setAskExportLocationType NOTIFY askExportLocationTypeChanged)
    Q_PROPERTY(
        au::preferences::SaveBehaviorPref::SaveBehavior saveBehavior READ saveBehavior NOTIFY saveBehaviorChanged)

public:
    explicit ExportPreferencesPageModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    bool askExportLocationType() const;
    void setAskExportLocationType(bool ask);

    preferences::SaveBehaviorPref::SaveBehavior saveBehavior() const;
    Q_INVOKABLE void setSaveBehavior(preferences::SaveBehaviorPref::SaveBehavior behavior);

signals:
    void askExportLocationTypeChanged();
    void saveBehaviorChanged();
};
}
