/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QString>
#include <QVariantList>
#include <QtQml/qqmlregistration.h>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "projectscene/iprojectsceneconfiguration.h"
#include "trackedit/itrackeditconfiguration.h"

namespace au::appshell {
class TracksPreferencesModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT

    muse::GlobalInject<au::projectscene::IProjectSceneConfiguration> projectsceneConfiguration;
    muse::GlobalInject<au::trackedit::ITrackeditConfiguration> trackeditConfiguration;

    Q_PROPERTY(int defaultTrackVisualization READ defaultTrackVisualization NOTIFY defaultTrackVisualizationChanged)
    Q_PROPERTY(int defaultRulerFormat READ defaultRulerFormat NOTIFY defaultRulerFormatChanged)
    Q_PROPERTY(QString defaultTrackName READ defaultTrackName NOTIFY defaultTrackNameChanged)

    Q_PROPERTY(QVariantList trackVisualizations READ trackVisualizations CONSTANT)
    Q_PROPERTY(QVariantList rulerFormats READ rulerFormats CONSTANT)

public:
    explicit TracksPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    int defaultTrackVisualization() const;
    Q_INVOKABLE void setDefaultTrackVisualization(int mode);

    int defaultRulerFormat() const;
    Q_INVOKABLE void setDefaultRulerFormat(int scale);

    QString defaultTrackName() const;
    Q_INVOKABLE void setDefaultTrackName(const QString& name);

    QVariantList trackVisualizations() const;
    QVariantList rulerFormats() const;

signals:
    void defaultTrackVisualizationChanged();
    void defaultRulerFormatChanged();
    void defaultTrackNameChanged();
};
}
