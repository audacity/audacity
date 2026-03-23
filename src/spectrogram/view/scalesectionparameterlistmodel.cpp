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
    : AbstractSectionParametersListModel(parent), muse::Injectable(muse::iocCtxForQmlObject(this)) {}

void ScaleSectionParameterListModel::componentComplete()
{
    spectrogramService()->trackSpectrogramConfigurationChanged().onReceive(this, [this](int trackId){
        if (trackId == m_trackId) {
            emit dataChanged(index(0), index(rowCount(index(0)) - 1), { ControlCurrentValueRole });
        }
    });
}

void ScaleSectionParameterListModel::setTrackId(int trackId)
{
    if (trackId == m_trackId) {
        return;
    }
    m_trackId = trackId;
    emit trackIdChanged();
}

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
    case ShortControlLabelRole: return shortControlLabel(control);
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
        { ShortControlLabelRole, "shortControlLabel" },
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

QString ScaleSectionParameterListModel::shortControlLabel(Control control) const
{
    switch (control) {
    case MaxFreq:
        return muse::qtrc("spectrogram/preferences", "Max");
    case MinFreq:
        return muse::qtrc("spectrogram/preferences", "Min");
    default:
        assert(false);
        return QString{};
    }
}

int ScaleSectionParameterListModel::controlMinValue(Control) const
{
    return 0;
}

int ScaleSectionParameterListModel::controlMaxValue(Control) const
{
    return m_trackId == -1 ? 1e6 : spectrogramService()->frequencyHardMaximum(m_trackId);
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

    return true;
}
}
