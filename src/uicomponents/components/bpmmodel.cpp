/*
* Audacity: A Digital Audio Editor
*/
#include "bpmmodel.h"

#include "internal/numeric/numericformatter.h"

static constexpr double BPM_MAX = 999.0;
static constexpr double BPM_MIN = 1.0;

using namespace au::uicomponents;

BPMModel::BPMModel(QObject* parent)
    : NumericViewModel(parent)
{
    initFieldInteractionController();

    reloadFormatter();

    setValue(0.0);
}

void BPMModel::upValue()
{
    setValue(std::min(m_value + 1, BPM_MAX));
}

void BPMModel::downValue()
{
    setValue(std::max(m_value - 1, BPM_MIN));
}

void BPMModel::setValue(double value)
{
    if (value < BPM_MIN || value > BPM_MAX) {
        return;
    }

    NumericViewModel::setValue(value);
}

void BPMModel::reloadFormatter()
{
    static const QString FORMAT = "1000bpm"; // translate

    m_formatter = std::make_shared<NumericFormatter>(FORMAT);

    initFormatter();

    m_fieldsInteractionController->setFormatter(m_formatter);
}
