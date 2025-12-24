/*
 * Audacity: A Digital Audio Editor
 */
#include "generatedeffectviewermodel.h"

#include "framework/global/log.h"

using namespace au::effects;
using namespace muse;

GeneratedEffectViewerModel::GeneratedEffectViewerModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void GeneratedEffectViewerModel::load()
{
    LOGI() << "GeneratedEffectViewerModel::load() instanceId=" << m_instanceId;

    updateEffectName();
    updateParameters();

    // Listen for parameter value changes
    parametersProvider()->parameterValuesChanged().onNotify(this, [this]() {
        updateParameters();
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
    updateParameters();
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

QVariantList GeneratedEffectViewerModel::parameters() const
{
    return m_parameters;
}

bool GeneratedEffectViewerModel::hasParameters() const
{
    return !m_parameters.isEmpty();
}

void GeneratedEffectViewerModel::setParameterValue(const QString& parameterId, double value)
{
    LOGI() << "GeneratedEffectViewerModel::setParameterValue() parameterId=" << parameterId << " value=" << value;

    if (m_instanceId < 0) {
        LOGE() << "Invalid instance ID";
        return;
    }

    bool success = parametersProvider()->setParameterValue(m_instanceId, String::fromQString(parameterId), value);
    if (!success) {
        LOGW() << "Failed to set parameter value";
    }
}

QString GeneratedEffectViewerModel::getParameterValueString(const QString& parameterId, double value) const
{
    if (m_instanceId < 0) {
        return QString::number(value);
    }

    String result = parametersProvider()->parameterValueString(m_instanceId, String::fromQString(parameterId), value);
    return result.toQString();
}

QVariantMap GeneratedEffectViewerModel::parameterInfoToVariant(const ParameterInfo& info) const
{
    QVariantMap map;

    map["id"] = info.id.toQString();
    map["name"] = info.name.toQString();
    map["units"] = info.units.toQString();

    // Convert ParameterType enum to string for QML
    QString typeStr;
    switch (info.type) {
    case ParameterType::Toggle:
        typeStr = "toggle";
        break;
    case ParameterType::Dropdown:
        typeStr = "dropdown";
        break;
    case ParameterType::Slider:
        typeStr = "slider";
        break;
    case ParameterType::Numeric:
        typeStr = "numeric";
        break;
    case ParameterType::ReadOnly:
        typeStr = "readonly";
        break;
    default:
        typeStr = "unknown";
        break;
    }
    map["type"] = typeStr;

    // Value range (normalized)
    map["minValue"] = info.minValue;
    map["maxValue"] = info.maxValue;
    map["defaultValue"] = info.defaultValue;
    map["currentValue"] = info.currentValue;
    map["stepSize"] = info.stepSize;
    map["stepCount"] = static_cast<int>(info.stepCount);

    // Formatted value string from plugin (e.g., "440 Hz", "3.5 dB")
    map["currentValueString"] = info.currentValueString.toQString();

    // Plain value range (actual display values for discrete parameters only)
    map["plainMinValue"] = info.plainMinValue;
    map["plainMaxValue"] = info.plainMaxValue;
    map["plainDefaultValue"] = info.plainDefaultValue;
    map["plainCurrentValue"] = info.plainCurrentValue;
    map["hasPlainRange"] = info.hasPlainRange;

    // Enum values for dropdown
    if (info.type == ParameterType::Dropdown) {
        QVariantList enumValues;
        QVariantList enumIndices;

        // Make sure both vectors have the same size
        size_t count = std::min(info.enumValues.size(), info.enumIndices.size());

        for (size_t i = 0; i < count; ++i) {
            enumValues.append(info.enumValues[i].toQString());
            enumIndices.append(info.enumIndices[i]);
        }

        map["enumValues"] = enumValues;
        map["enumIndices"] = enumIndices;

        LOGI() << "Dropdown parameter " << info.name << " has " << count << " enum values";
    }

    // Flags
    map["isReadOnly"] = info.isReadOnly;
    map["isHidden"] = info.isHidden;
    map["isLogarithmic"] = info.isLogarithmic;
    map["isInteger"] = info.isInteger;
    map["canAutomate"] = info.canAutomate;

    return map;
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

void GeneratedEffectViewerModel::updateParameters()
{
    if (m_instanceId < 0) {
        m_parameters.clear();
        emit parametersChanged();
        return;
    }

    ParameterInfoList paramList = parametersProvider()->parameters(m_instanceId);

    LOGI() << "GeneratedEffectViewerModel::updateParameters() found " << paramList.size() << " parameters";

    QVariantList newParameters;
    for (const auto& param : paramList) {
        // Skip hidden parameters
        if (param.isHidden) {
            continue;
        }

        newParameters.append(parameterInfoToVariant(param));
    }

    m_parameters = newParameters;
    emit parametersChanged();
}
