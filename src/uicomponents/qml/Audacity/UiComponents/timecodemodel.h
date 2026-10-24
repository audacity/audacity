/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QtQml/qqmlregistration.h>

#include "timecodemodeselector.h"
#include "numericviewmodel.h"

namespace au::uicomponents {
class TimecodeModel : public NumericViewModel
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(TimecodeMode mode READ mode WRITE setMode FINAL)

public:
    explicit TimecodeModel(QObject* parent = nullptr);

    TimecodeMode mode() const;
    void setMode(TimecodeMode mode);

private:
    void reloadFormatter() override;

    TimecodeMode m_mode = TimecodeMode::TimePoint;
};
}
