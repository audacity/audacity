#pragma once

#include <QObject>

namespace au::playback {
class TimecodeModeSelector : public QObject
{
    Q_OBJECT
public:
    explicit TimecodeModeSelector(QObject* parent = nullptr);

    enum Mode {
        TimePoint,
        Duration
    };
    Q_ENUM(Mode)
};

typedef TimecodeModeSelector::Mode TimecodeMode;
}
