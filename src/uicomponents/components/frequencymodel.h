/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "numericviewmodel.h"

namespace au::uicomponents {
class FrequencyModel : public NumericViewModel
{
    Q_OBJECT

public:
    explicit FrequencyModel(QObject* parent = nullptr);

private:
    void reloadFormatter() override;
};
}
