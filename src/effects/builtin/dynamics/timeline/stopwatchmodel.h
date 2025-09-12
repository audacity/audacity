/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "stopwatch.h"
#include "global/modularity/ioc.h"

#include <QObject>

namespace au::effects {
class StopwatchModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(au::effects::Stopwatch::PlayState playState READ playState NOTIFY playStateChanged)
public:

    Stopwatch::PlayState playState() const { return Stopwatch::PlayState::Playing; }

signals:
    void playStateChanged();
};
}
