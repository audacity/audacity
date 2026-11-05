/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QString>
#include <QTimer>
#include <QtQml/qqmlregistration.h>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include "record/itimedrecordingscenario.h"

namespace au::record {
class TimedRecordingProgressModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(bool isRecording READ isRecording NOTIFY changed FINAL)
    Q_PROPERTY(bool isQuitting READ isQuitting NOTIFY changed FINAL)
    Q_PROPERTY(QString statusText READ statusText NOTIFY changed FINAL)
    Q_PROPERTY(QString clockText READ clockText NOTIFY changed FINAL)
    Q_PROPERTY(QString startText READ startText NOTIFY changed FINAL)
    Q_PROPERTY(QString endText READ endText NOTIFY changed FINAL)

    muse::ContextInject<ITimedRecordingScenario> scenario{ this };

public:
    explicit TimedRecordingProgressModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void stop();

    bool isRecording() const;
    bool isQuitting() const;
    QString statusText() const;
    QString clockText() const;
    QString startText() const;
    QString endText() const;

signals:
    void changed();

private:
    void update();

    bool m_isRecording = false;
    bool m_isQuitting = false;
    QString m_statusText;
    QString m_clockText;
    QString m_startText;
    QString m_endText;

    QTimer m_refresher;
};
}
