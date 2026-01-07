/*
 * Audacity: A Digital Audio Editor
 */
#include "generatedeffectviewermodel.h"

#include "framework/global/log.h"

using namespace au::effects;
using namespace muse;

GeneratedEffectViewerModel::GeneratedEffectViewerModel(QObject* parent)
    : QAbstractListModel(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

int GeneratedEffectViewerModel::rowCount(const QModelIndex& parent) const
{
    if (parent.isValid()) {
        return 0;
    }
    return static_cast<int>(m_parameters.size());
}

QVariant GeneratedEffectViewerModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() < 0 || index.row() >= static_cast<int>(m_parameters.size())) {
        return QVariant();
    }

    const ParameterInfo& param = m_parameters[index.row()];

    switch (role) {
    case IdRole:
        return param.id.toQString();
    case NameRole:
        return param.name.toQString();
    case UnitsRole:
        return param.units.toQString();
    case TypeRole:
        switch (param.type) {
        case ParameterType::Toggle: return QStringLiteral("toggle");
        case ParameterType::Dropdown: return QStringLiteral("dropdown");
        case ParameterType::Slider: return QStringLiteral("slider");
        case ParameterType::Numeric: return QStringLiteral("numeric");
        case ParameterType::ReadOnly: return QStringLiteral("readonly");
        default: return QStringLiteral("unknown");
        }
    case MinValueRole:
        return param.minValue;
    case MaxValueRole:
        return param.maxValue;
    case DefaultValueRole:
        return param.defaultValue;
    case CurrentValueRole:
        return param.currentValue;
    case StepSizeRole:
        return param.stepSize;
    case StepCountRole:
        return param.stepCount;
    case CurrentValueStringRole:
        return param.currentValueString.toQString();
    case EnumValuesRole:
    {
        QVariantList list;
        for (const auto& val : param.enumValues) {
            list.append(val.toQString());
        }
        return list;
    }
    case EnumIndicesRole:
        return QVariant::fromValue(param.enumIndices);
    case IsReadOnlyRole:
        return param.isReadOnly;
    case IsHiddenRole:
        return param.isHidden;
    case IsLogarithmicRole:
        return param.isLogarithmic;
    case IsIntegerRole:
        return param.isInteger;
    case CanAutomateRole:
        return param.canAutomate;
    default:
        return QVariant();
    }
}

bool GeneratedEffectViewerModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || index.row() < 0 || index.row() >= static_cast<int>(m_parameters.size())) {
        return false;
    }

    if (role != CurrentValueRole) {
        return false;
    }

    setParameterValue(index.row(), value.toDouble());
    return true;
}

QHash<int, QByteArray> GeneratedEffectViewerModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { IdRole, "id" },
        { NameRole, "name" },
        { UnitsRole, "units" },
        { TypeRole, "type" },
        { MinValueRole, "minValue" },
        { MaxValueRole, "maxValue" },
        { DefaultValueRole, "defaultValue" },
        { CurrentValueRole, "currentValue" },
        { StepSizeRole, "stepSize" },
        { StepCountRole, "stepCount" },
        { CurrentValueStringRole, "currentValueString" },
        { EnumValuesRole, "enumValues" },
        { EnumIndicesRole, "enumIndices" },
        { IsReadOnlyRole, "isReadOnly" },
        { IsHiddenRole, "isHidden" },
        { IsLogarithmicRole, "isLogarithmic" },
        { IsIntegerRole, "isInteger" },
        { CanAutomateRole, "canAutomate" },
    };
    return roles;
}

void GeneratedEffectViewerModel::load()
{
    LOGI() << "instanceId=" << m_instanceId;

    updateEffectName();
    reloadParameters();

    // Listen for parameter value changes
    parametersProvider()->parameterValuesChanged().onNotify(this, [this]() {
        reloadParameters();
    });
}

int GeneratedEffectViewerModel::instanceId() const
{
    return m_instanceId;
}

void GeneratedEffectViewerModel::setInstanceId(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }

    m_instanceId = newInstanceId;
    emit instanceIdChanged();

    updateEffectName();
    reloadParameters();
}

QString GeneratedEffectViewerModel::effectName() const
{
    return m_effectName;
}

QString GeneratedEffectViewerModel::title() const
{
    if (m_effectName.isEmpty()) {
        return QObject::tr("Auto-Generated UI");
    }
    return QObject::tr("%1 Fallback UI").arg(m_effectName);
}

QString GeneratedEffectViewerModel::noParametersMessage() const
{
    return QObject::tr("No parameters available for this effect");
}

bool GeneratedEffectViewerModel::hasParameters() const
{
    return !m_parameters.empty();
}

void GeneratedEffectViewerModel::setParameterValue(int index, double plainValue)
{
    if (m_instanceId < 0) {
        LOGE() << "Invalid instance ID";
        return;
    }

    if (index < 0 || index >= static_cast<int>(m_parameters.size())) {
        LOGE() << "Invalid parameter index: " << index;
        return;
    }

    ParameterInfo& param = m_parameters[index];
    LOGI() << "index=" << index << " id=" << param.id << " plainValue=" << plainValue;

    // Convert plain value to normalized [0,1] for the plugin API
    const double normalizedValue = param.toNormalized(plainValue);

    bool success = parametersProvider()->setParameterValue(m_instanceId, param.id, normalizedValue);
    if (!success) {
        LOGW() << "Failed to set parameter value";
        return;
    }

    // Update local cache and emit dataChanged for just this row
    param.currentValue = plainValue;
    // Also update the formatted string
    param.currentValueString = parametersProvider()->parameterValueString(m_instanceId, param.id, normalizedValue);

    QModelIndex modelIndex = createIndex(index, 0);
    emit dataChanged(modelIndex, modelIndex, { CurrentValueRole, CurrentValueStringRole });
}

QString GeneratedEffectViewerModel::getParameterValueString(int index, double normalizedValue) const
{
    if (m_instanceId < 0 || index < 0 || index >= static_cast<int>(m_parameters.size())) {
        return QString::number(normalizedValue);
    }

    const ParameterInfo& param = m_parameters[index];
    String result = parametersProvider()->parameterValueString(m_instanceId, param.id, normalizedValue);
    return result.toQString();
}

void GeneratedEffectViewerModel::updateEffectName()
{
    QString oldEffectName = m_effectName;

    if (m_instanceId < 0) {
        m_effectName.clear();
    } else {
        EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
        EffectMeta meta = effectsProvider()->meta(effectId);

        if (meta.isValid()) {
            m_effectName = meta.title.toQString();
        } else {
            m_effectName = QString("Unknown Effect");
        }
    }

    if (m_effectName != oldEffectName) {
        emit effectNameChanged();
        emit titleChanged();
    }
}

void GeneratedEffectViewerModel::reloadParameters()
{
    const bool hadParameters = !m_parameters.empty();

    beginResetModel();

    m_parameters.clear();

    if (m_instanceId >= 0) {
        ParameterInfoList paramList = parametersProvider()->parameters(m_instanceId);

        LOGI() << "found " << paramList.size() << " parameters";

        // Filter out hidden parameters
        for (const auto& param : paramList) {
            if (!param.isHidden) {
                m_parameters.push_back(param);
            }
        }
    }

    endResetModel();

    if (hadParameters != !m_parameters.empty()) {
        emit hasParametersChanged();
    }
}

int GeneratedEffectViewerModel::findParameterIndex(const muse::String& parameterId) const
{
    for (size_t i = 0; i < m_parameters.size(); ++i) {
        if (m_parameters[i].id == parameterId) {
            return static_cast<int>(i);
        }
    }
    return -1;
}
