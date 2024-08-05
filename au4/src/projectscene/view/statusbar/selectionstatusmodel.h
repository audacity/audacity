/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iselectioncontroller.h"
#include "playback/iplayback.h"

namespace au::projectscene {
class SelectionStatusModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(double startTime READ startTime WRITE setStartTime NOTIFY startTimeChanged FINAL)
    Q_PROPERTY(double endTime READ endTime WRITE setEndTime NOTIFY endTimeChanged FINAL)

    Q_PROPERTY(int currentFormat READ currentFormat WRITE setCurrentFormat NOTIFY currentFormatChanged FINAL)

    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged FINAL)
    Q_PROPERTY(double tempo READ tempo NOTIFY timeSignatureChanged FINAL)
    Q_PROPERTY(int upperTimeSignature READ upperTimeSignature NOTIFY timeSignatureChanged FINAL)
    Q_PROPERTY(int lowerTimeSignature READ lowerTimeSignature NOTIFY timeSignatureChanged FINAL)

    Q_PROPERTY(bool isEnabled READ isEnabled NOTIFY isEnabledChanged)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<trackedit::ISelectionController> selectionController;
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
    double tempo() const;
    int upperTimeSignature() const;
    int lowerTimeSignature() const;

    bool isEnabled() const;

signals:
    void startTimeChanged();
    void endTimeChanged();

    void currentFormatChanged();
    void sampleRateChanged();

    void timeSignatureChanged();

    void isEnabledChanged();

private:

    double m_startTime = 0.0;
    double m_endTime = 0.0;

    int m_currentFormat = 0;
};
}
