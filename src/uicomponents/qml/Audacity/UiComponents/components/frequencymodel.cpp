/*
* Audacity: A Digital Audio Editor
*/
#include "frequencymodel.h"

#include "internal/numeric/numericformatter.h"

#include "translation.h"

using namespace au::uicomponents;

namespace {
QList<NumericViewFormat> makeFrequencyFormats()
{
    // translate all
    return { NumericViewFormat { static_cast<NumericViewFormatType>(FrequencyFormatType::Centihertz),
                                 muse::qtrc("uicomponents", "Hz"), "010,01000>0100 Hz" },
             NumericViewFormat { static_cast<NumericViewFormatType>(FrequencyFormatType::Hertz),
                                 muse::qtrc("uicomponents", "kHz"), "01000>01000 kHz|0.001" },
    };
}
}

FrequencyModel::FrequencyModel(QObject* parent)
    : NumericViewModel(parent, makeFrequencyFormats())
{
    setCurrentFormat(static_cast<int>(FrequencyFormatType::Centihertz));

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
