/*
 * Audacity: A Digital Audio Editor
 */
#include "colorsectionparameterlistmodel.h"

#include "abstractspectrogramsettingsmodel.h"
#include "./spectrogramviewutils.h"

#include "framework/global/log.h"
#include "framework/global/translation.h"

namespace au::spectrogram {
ColorSectionParameterListModel::ColorSectionParameterListModel(QObject* parent)
    : QAbstractListModel(parent) {}

void ColorSectionParameterListModel::setSettingsModel(AbstractSpectrogramSettingsModel* model)
{
    if (m_settingsModel == model) {
        return;
    }
    m_settingsModel = model;

    if (m_settingsModel) {
        const QList<int> roles { ControlCurrentValueRole };
        CONNECT_SETTING_CHANGED(colorGainDbChanged_2, Control::ColorGain, roles);
        CONNECT_SETTING_CHANGED(colorRangeDbChanged_3, Control::ColorRange, roles);
        CONNECT_SETTING_CHANGED(colorHighBoostDbPerDecChanged_4, Control::ColorHighBoost, roles);
    }

    emit settingsModelChanged();
}

QVariant ColorSectionParameterListModel::data(const QModelIndex& index, int role) const
{
    if (!m_settingsModel) {
        return QVariant{};
    }

    const auto control = static_cast<Control>(index.row());
    switch (role) {
    case ControlLabelRole: return controlLabel(control);
    case ControlUnitsRole: return controlUnits(control);
    case ControlMinValueRole: return controlMinValue(control);
    case ControlMaxValueRole: return controlMaxValue(control);
    case ControlCurrentValueRole: return controlCurrentValue(control);
    default:
        assert(false);
        return QVariant{};
    }
}

int ColorSectionParameterListModel::controlCurrentValue(Control control) const
{
    switch (control) {
    case ColorGain:
        return m_settingsModel->colorGainDb_2();
    case ColorRange:
        return m_settingsModel->colorRangeDb_3();
    case ColorHighBoost:
        return m_settingsModel->colorHighBoostDbPerDec_4();
    default:
        assert(false);
        return 0;
    }
}

int ColorSectionParameterListModel::controlMinValue(Control control) const
{
    switch (control) {
    case ColorGain:
        return std::numeric_limits<int>::min();
    case ColorRange:
        return m_settingsModel->colorRangeDbMin();
    case ColorHighBoost:
        return m_settingsModel->colorHighBoostDbPerDecMin();
    default:
        assert(false);
        return 0;
    }
}

int ColorSectionParameterListModel::controlMaxValue(Control control) const
{
    switch (control) {
    case ColorGain:
        return std::numeric_limits<int>::max();
    case ColorRange:
        return std::numeric_limits<int>::max();
    case ColorHighBoost:
        return m_settingsModel->colorHighBoostDbPerDecMax();
    default:
        assert(false);
        return 0;
    }
}

QString ColorSectionParameterListModel::controlLabel(Control control) const
{
    switch (control) {
    case ColorGain:
        return muse::qtrc("spectrogram/preferences", "Gain");
    case ColorRange:
        return muse::qtrc("spectrogram/preferences", "Range");
    case ColorHighBoost:
        return muse::qtrc("spectrogram/preferences", "High Boost");
    default:
        assert(false);
        return QString{};
    }
}

QString ColorSectionParameterListModel::controlUnits(Control control) const
{
    switch (control) {
    case ColorGain:
        return { "dB" };
    case ColorRange:
        return { "dB" };
    case ColorHighBoost:
        return { "dB/dec" };
    default:
        assert(false);
        return {};
    }
}

QHash<int, QByteArray> ColorSectionParameterListModel::roleNames() const
{
    return {
        { ControlLabelRole, "colorControlLabel" },
        { ControlUnitsRole, "colorControlUnits" },
        { ControlMinValueRole, "colorControlMinValue" },
        { ControlMaxValueRole, "colorControlMaxValue" },
        { ControlCurrentValueRole, "colorControlCurrentValue" }
    };
}

bool ColorSectionParameterListModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (role != ControlCurrentValueRole) {
        return false;
    }

    IF_ASSERT_FAILED(m_settingsModel) {
        return false;
    }

    const auto control = static_cast<Control>(index.row());
    const auto currentValue = value.toInt();

    switch (control) {
    case ColorGain:
        m_settingsModel->setColorGainDb_2(currentValue);
        break;
    case ColorRange:
        m_settingsModel->setColorRangeDb_3(currentValue);
        break;
    case ColorHighBoost:
        m_settingsModel->setColorHighBoostDbPerDec_4(currentValue);
        break;
    default:
        assert(false);
        return false;
    }

    emit dataChanged(this->index(control), this->index(control), { ControlCurrentValueRole });

    return true;
}
}
