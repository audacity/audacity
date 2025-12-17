/*
 * Audacity: A Digital Audio Editor
 */
#include "scalesectionparameterlistmodel.h"

#include "abstractspectrogramsettingsmodel.h"
#include "./spectrogramviewutils.h"

#include "framework/global/translation.h"
#include "framework/global/log.h"

namespace au::spectrogram {
ScaleSectionParameterListModel::ScaleSectionParameterListModel(QObject* parent)
    : AbstractSectionParametersListModel(parent) {}

void ScaleSectionParameterListModel::onSettingsModelSet(AbstractSpectrogramSettingsModel&)
{
    const QList<int> roles { ControlCurrentValueRole };
    CONNECT_SETTING_CHANGED(maxFreqChanged, Control::MaxFreq, roles);
    CONNECT_SETTING_CHANGED(minFreqChanged, Control::MinFreq, roles);
}

QVariant ScaleSectionParameterListModel::data(const QModelIndex& index, int role) const
{
    if (!m_settingsModel) {
        return QVariant{};
    }

    const auto control = static_cast<Control>(index.row());
    switch (role) {
    case ControlLabelRole: return controlLabel(control);
    case ControlUnitsRole: return muse::qtrc("spectrogram/preferences", "Hz");
    case ControlMinValueRole: return controlMinValue(control);
    case ControlMaxValueRole: return controlMaxValue(control);
    case ControlCurrentValueRole: return controlCurrentValue(control);
    default:
        assert(false);
        return QVariant{};
    }
}

QHash<int, QByteArray> ScaleSectionParameterListModel::roleNames() const
{
    return {
        { ControlLabelRole, "controlLabel" },
        { ControlUnitsRole, "controlUnits" },
        { ControlMinValueRole, "controlMinValue" },
        { ControlMaxValueRole, "controlMaxValue" },
        { ControlCurrentValueRole, "controlCurrentValue" },
    };
}

QString ScaleSectionParameterListModel::controlLabel(Control control) const
{
    switch (control) {
    case MaxFreq:
        return muse::qtrc("spectrogram/preferences", "Max frequency");
    case MinFreq:
        return muse::qtrc("spectrogram/preferences", "Min frequency");
    default:
        assert(false);
        return QString{};
    }
}

int ScaleSectionParameterListModel::controlMinValue(Control) const
{
    return m_settingsModel ? m_settingsModel->frequencyHardMinimum() : 0;
}

int ScaleSectionParameterListModel::controlMaxValue(Control) const
{
    return m_settingsModel ? m_settingsModel->frequencyHardMaximum() : 0;
}

int ScaleSectionParameterListModel::controlCurrentValue(Control control) const
{
    switch (control) {
    case MaxFreq:
        return m_settingsModel->maxFreq();
    case MinFreq:
        return m_settingsModel->minFreq();
    default:
        assert(false);
        return {};
    }
}

bool ScaleSectionParameterListModel::setData(const QModelIndex& index, const QVariant& valueVar, int)
{
    const auto control = static_cast<Control>(index.row());
    const auto value = valueVar.toInt();

    switch (control) {
    case MaxFreq:
        m_settingsModel->setMaxFreq(value);
        break;
    case MinFreq:
        m_settingsModel->setMinFreq(value);
        break;
    default:
        assert(false);
        return false;
    }

    emit dataChanged(this->index(control), this->index(control), { ControlCurrentValueRole, ControlMinValueRole, ControlMaxValueRole });

    return true;
}
}
