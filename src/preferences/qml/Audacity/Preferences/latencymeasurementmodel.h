/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QString>
#include <qqmlintegration.h>

#include "framework/global/modularity/ioc.h"

#include "audio/driver/ilatencymeasurement.h"

namespace au::appshell {
class LatencyMeasurementModel : public QObject, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(State state READ state NOTIFY changed)
    Q_PROPERTY(QString resultText READ resultText NOTIFY changed)
    Q_PROPERTY(QString reportedText READ reportedText NOTIFY changed)
    Q_PROPERTY(QString detailsText READ detailsText NOTIFY changed)
    Q_PROPERTY(QString warningText READ warningText NOTIFY changed)
    Q_PROPERTY(double compensationMs READ compensationMs NOTIFY changed)

    muse::GlobalInject<audio::ILatencyMeasurement> latencyMeasurement;

public:
    enum class State {
        Idle,
        Measuring,
        Measured,
        Failed,
    };
    Q_ENUM(State)

    explicit LatencyMeasurementModel(QObject* parent = nullptr);

    Q_INVOKABLE void start();

    State state() const;
    QString resultText() const;
    QString reportedText() const;
    QString detailsText() const;
    QString warningText() const;
    double compensationMs() const;

signals:
    void changed();

private:
    State m_state = State::Idle;
    audio::LatencyMeasurement m_result;
};
}
