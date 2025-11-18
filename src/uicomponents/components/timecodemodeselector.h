#pragma once

#include <QObject>

namespace au::uicomponents {
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
