/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QtQml/qqmlregistration.h>

#include "numericviewmodel.h"

namespace au::uicomponents {
class FrequencyModel : public NumericViewModel
{
    Q_OBJECT
    QML_ELEMENT

public:
    explicit FrequencyModel(QObject* parent = nullptr);

private:
    void reloadFormatter() override;
};
}
