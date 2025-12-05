/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "numericviewmodel.h"

namespace au::uicomponents {
class BPMModel : public NumericViewModel
{
    Q_OBJECT

public:
    explicit BPMModel(QObject* parent = nullptr);

    Q_INVOKABLE void upValue();
    Q_INVOKABLE void downValue();

    void setValue(double value) override;

private:
    void reloadFormatter() override;
};
}
