/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "processing/iselectioncontroller.h"
#include "playback/iplayback.h"

namespace au::projectscene {
class SelectionStatusModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(double startTime READ startTime WRITE setStartTime NOTIFY startTimeChanged FINAL)
    Q_PROPERTY(double endTime READ endTime WRITE setEndTime NOTIFY endTimeChanged FINAL)

    Q_PROPERTY(int currentFormat READ currentFormat WRITE setCurrentFormat NOTIFY currentFormatChanged FINAL)
    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged FINAL)

    muse::Inject<processing::ISelectionController> selectionController;
    muse::Inject<playback::IPlayback> playback;

public:
    Q_INVOKABLE void init();

    double startTime() const;
    void setStartTime(double time);

    double endTime() const;
    void setEndTime(double time);

    int currentFormat() const;
    void setCurrentFormat(int format);

    double sampleRate() const;

signals:
    void startTimeChanged();
    void endTimeChanged();

    void currentFormatChanged();
    void sampleRateChanged();

private:

    double m_startTime = 0.0;
    double m_endTime = 0.0;

    int m_currentFormat = 0;
};
}
