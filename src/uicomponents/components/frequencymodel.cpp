/*
* Audacity: A Digital Audio Editor
*/
#include "frequencymodel.h"

#include "numericformatter.h"

#include "translation.h"

using namespace au::uicomponents;

FrequencyModel::FrequencyModel(QObject* parent)
    : NumericViewModel(parent)
{
    // translate all
    m_availableViewFormats = {
        { static_cast<NumericViewFormatType>(FrequencyFormatType::Centihertz), muse::qtrc("uicomponents", "Hz"),
          "010,01000>0100 Hz" },
        { static_cast<NumericViewFormatType>(FrequencyFormatType::Hertz), muse::qtrc("uicomponents", "kHz"), "01000>01000 kHz|0.001" },
    };

    m_currentFormat = static_cast<NumericViewFormatType>(FrequencyFormatType::Centihertz);

    initFieldInteractionController();

    reloadFormatter();

    setValue(0.0);
}

void FrequencyModel::reloadFormatter()
{
    NumericViewFormat currentFormat = currentViewFormat();
    if (!currentFormat.isValid()) {
        return;
    }

    m_formatter = std::make_shared<NumericFormatter>(currentFormat.formatStr);

    initFormatter();

    m_fieldsInteractionController->setFormatter(m_formatter);
}
