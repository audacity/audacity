#pragma once

#include <QObject>
#include <QtQml/qqmlregistration.h>

namespace au::uicomponents {
class TimecodeModeSelector : public QObject
{
    Q_OBJECT
    QML_ELEMENT
    QML_UNCREATABLE("TimecodeModeSelector is a simple enum")
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
