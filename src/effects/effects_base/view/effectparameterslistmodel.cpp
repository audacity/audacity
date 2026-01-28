/*
 * Audacity: A Digital Audio Editor
 */
#include "effectparameterslistmodel.h"

#include <cmath>

#include "framework/global/log.h"

using namespace au::effects;
using namespace muse;

EffectParametersListModel::EffectParametersListModel(QObject* parent, EffectInstanceId instanceId)
    : QAbstractListModel(parent)
    , muse::Injectable(muse::iocCtxForQmlObject(this))
    , m_instanceId(instanceId)
{
    IF_ASSERT_FAILED(m_instanceId >= 0) {
        LOGE() << "Invalid instance ID: " << m_instanceId;
    }
}

int EffectParametersListModel::rowCount(const QModelIndex& parent) const
{
    if (parent.isValid()) {
        return 0;
    }
    return static_cast<int>(m_parameters.size());
}

QVariant EffectParametersListModel::data(const QModelIndex& index, int role) const
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
        case ParameterType::Time: return QStringLiteral("time");
        case ParameterType::File: return QStringLiteral("file");
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
    case FormattedValueRole:
    {
        QString displayText = param.currentValueString.toQString();
        QString units = param.units.toQString();
        // Add units if available and not already in the formatted string
        if (!units.isEmpty() && !displayText.contains(units)) {
            displayText = displayText + " " + units;
        }
        return displayText;
    }
    case IsToggleCheckedRole:
    {
        // For toggle controls: check if currentValue is above the midpoint
        const double midpoint = (param.minValue + param.maxValue) / 2.0;
        return param.currentValue > midpoint;
    }
    case CurrentEnumIndexRole:
    {
        // For dropdown controls: find the index that matches the current value
        for (size_t i = 0; i < param.enumIndices.size(); ++i) {
            if (std::abs(param.enumIndices[i] - param.currentValue) < 0.001) {
                return static_cast<int>(i);
            }
        }
        return 0; // Default to first item if no match found
    }
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

bool EffectParametersListModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || index.row() < 0 || index.row() >= static_cast<int>(m_parameters.size())) {
        return false;
    }

    if (role != CurrentValueRole) {
        return false;
    }

    // Get parameter ID from the index and use ID-based method
    const ParameterInfo& param = m_parameters[index.row()];
    setParameterValue(param.id.toQString(), value.toDouble());
    return true;
}

QHash<int, QByteArray> EffectParametersListModel::roleNames() const
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
        { FormattedValueRole, "formattedValue" },
        { IsToggleCheckedRole, "isToggleChecked" },
        { CurrentEnumIndexRole, "currentEnumIndex" },
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

void EffectParametersListModel::load()
{
    LOGI() << "instanceId=" << m_instanceId;

    reloadParameters();

    // Listen for individual parameter value changes
    parametersProvider()->parameterChanged().onReceive(this, [this](const ParameterChangedData& data) {
        // Only handle changes for our instance
        if (data.instanceId != m_instanceId) {
            return;
        }

        // Find the parameter in our list and update it
        int idx = findParameterIndex(data.parameterId);
        if (idx < 0) {
            return;
        }

        // Update the cached values
        m_parameters[idx].currentValue = data.newFullRangeValue;
        m_parameters[idx].currentValueString = data.newValueString;

        // Emit dataChanged for just this row
        // Include all derived roles that depend on currentValue or currentValueString
        QModelIndex modelIndex = createIndex(idx, 0);
        emit dataChanged(modelIndex, modelIndex, {
            CurrentValueRole,
            CurrentValueStringRole,
            FormattedValueRole,
            IsToggleCheckedRole,
            CurrentEnumIndexRole
        });
    });
}

void EffectParametersListModel::setParameterValue(const QString& parameterId, double fullRangeValue)
{
    // Update the parameter value through the provider
    parametersProvider()->setParameterValue(m_instanceId, String::fromQString(parameterId), fullRangeValue);

    // The actual model update will happen via the parameterChanged signal
}

QString EffectParametersListModel::getParameterValueString(int index, double normalizedValue) const
{
    if (index < 0 || index >= static_cast<int>(m_parameters.size())) {
        return QString();
    }

    const ParameterInfo& param = m_parameters[index];
    return parametersProvider()->parameterValueString(m_instanceId, param.id, normalizedValue).toQString();
}

void EffectParametersListModel::beginGesture(const QString& parameterId)
{
    parametersProvider()->beginParameterGesture(m_instanceId, String::fromQString(parameterId));
}

void EffectParametersListModel::endGesture(const QString& parameterId)
{
    parametersProvider()->endParameterGesture(m_instanceId, String::fromQString(parameterId));
}

bool EffectParametersListModel::hasParameters() const
{
    return !m_parameters.empty();
}

void EffectParametersListModel::reloadParameters()
{
    beginResetModel();

    m_parameters = parametersProvider()->parameters(m_instanceId);

    endResetModel();

    emit hasParametersChanged();
}

int EffectParametersListModel::findParameterIndex(const muse::String& parameterId) const
{
    for (size_t i = 0; i < m_parameters.size(); ++i) {
        if (m_parameters[i].id == parameterId) {
            return static_cast<int>(i);
        }
    }
    return -1;
}
