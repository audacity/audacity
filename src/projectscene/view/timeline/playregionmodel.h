/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>

#include "global/async/asyncable.h"
#include "modularity/ioc.h"

#include "playback/iplaybackcontroller.h"

namespace au {
class PlayRegionModel : public QObject, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(double start READ start WRITE setStart NOTIFY startChanged FINAL)
    Q_PROPERTY(double end READ end WRITE setEnd NOTIFY endChanged FINAL)
    Q_PROPERTY(bool active READ active WRITE setActive NOTIFY activeChanged FINAL)

    muse::Inject<playback::IPlaybackController> playbackController{ this };

public:
    explicit PlayRegionModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    double start() const;
    void setStart(double newStart);

    double end() const;
    void setEnd(double newEnd);

    bool active() const;
    void setActive(bool newActive);

signals:
    void startChanged();
    void endChanged();

    void activeChanged();

private:
    void onLoopRegionChanged();

    double m_start;
    double m_end;
    bool m_active;
};
}
