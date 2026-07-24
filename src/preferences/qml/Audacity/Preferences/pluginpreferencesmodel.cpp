/*
 * Audacity: A Digital Audio Editor
 */
#include "pluginpreferencesmodel.h"

#include "io/dir.h"

#include <cmath>
#include <limits>

#include <QVariantMap>

namespace au::appshell {
namespace {
static QStringList toQStringList(const muse::io::paths_t& paths)
{
    QStringList result;
    result.reserve(static_cast<int>(paths.size()));
    for (const auto& p : paths) {
        result.push_back(p.toQString());
    }
    return result;
}

static QVariant valueToVariant(const audacityplugin::Value& value)
{
    if (const auto* result = std::get_if<bool>(&value)) {
        return *result;
    }
    // QML numbers cannot exactly represent all int64_t values.
    if (const auto* result = std::get_if<int64_t>(&value)) {
        return QString::number(*result);
    }
    if (const auto* result = std::get_if<double>(&value)) {
        return *result;
    }
    if (const auto* result = std::get_if<std::string>(&value)) {
        return QString::fromStdString(*result);
    }
    return {};
}

static QString valueTypeName(audacityplugin::ParameterType type)
{
    switch (type) {
    case audacityplugin::ParameterType::Boolean: return QStringLiteral("bool");
    case audacityplugin::ParameterType::Int64: return QStringLiteral("int64");
    case audacityplugin::ParameterType::Double: return QStringLiteral("double");
    case audacityplugin::ParameterType::String: return QStringLiteral("string");
    case audacityplugin::ParameterType::Enumeration: return QStringLiteral("enum");
    case audacityplugin::ParameterType::File: return QStringLiteral("file");
    case audacityplugin::ParameterType::Directory: return QStringLiteral("directory");
    }
    return {};
}

static bool variantToValue(const QVariant& source, audacityplugin::ParameterType type,
                           audacityplugin::Value& result, QString& error)
{
    switch (type) {
    case audacityplugin::ParameterType::Boolean:
        result = source.toBool();
        return true;
    case audacityplugin::ParameterType::Int64: {
        bool ok = false;
        const qlonglong value = source.toString().trimmed().toLongLong(&ok);
        if (!ok) {
            error = QStringLiteral("Enter a whole number between %1 and %2.")
                    .arg(std::numeric_limits<qint64>::min())
                    .arg(std::numeric_limits<qint64>::max());
            return false;
        }
        result = static_cast<int64_t>(value);
        return true;
    }
    case audacityplugin::ParameterType::Double: {
        bool ok = false;
        const double value = source.toString().trimmed().toDouble(&ok);
        if (!ok || !std::isfinite(value)) {
            error = QStringLiteral("Enter a finite number.");
            return false;
        }
        result = value;
        return true;
    }
    case audacityplugin::ParameterType::String:
    case audacityplugin::ParameterType::Enumeration:
    case audacityplugin::ParameterType::File:
    case audacityplugin::ParameterType::Directory:
        result = source.toString().toStdString();
        return true;
    }
    error = QStringLiteral("This preference uses an unsupported value type.");
    return false;
}

static QString resultText(const audacityplugin::PluginPreferences& plugin,
                          const QString& fallback)
{
    const QString name = QString::fromStdString(plugin.pluginName).isEmpty()
                         ? QString::fromStdString(plugin.pluginId)
                         : QString::fromStdString(plugin.pluginName);
    return QStringLiteral("%1: %2").arg(name, fallback);
}
}

static muse::io::paths_t toPathsT(const QStringList& paths)
{
    muse::io::paths_t result;
    result.reserve(static_cast<size_t>(paths.size()));
    for (const auto& p : paths) {
        result.push_back(muse::io::path_t(p));
    }
    return result;
}

PluginPreferencesModel::PluginPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void PluginPreferencesModel::init()
{
    effectsConfiguration()->effectMenuOrganizationChanged().onNotify(this, [this] {
        emit effectMenuOrganizationChanged();
    });

    effectsConfiguration()->lv2CustomPathsChanged().onNotify(this, [this] {
        emit lv2CustomPathsChanged();
    });

    effectsConfiguration()->vst3CustomPathsChanged().onNotify(this, [this] {
        emit vst3CustomPathsChanged();
    });

    m_audacityPluginPreferences.clear();
    m_audacityPluginInputErrors.clear();
    setAudacityPluginError({});
    if (audacityPluginHost()) {
        m_audacityPluginPreferences = audacityPluginHost()->preferences();
    }
    validateAudacityPluginPreferences();
    emit audacityPluginsChanged();
}

au::effects::EffectMenuOrganization PluginPreferencesModel::effectMenuOrganization() const
{
    return effectsConfiguration()->effectMenuOrganization();
}

void PluginPreferencesModel::setEffectMenuOrganization(effects::EffectMenuOrganization organization)
{
    if (effectMenuOrganization() == organization) {
        return;
    }

    effectsConfiguration()->setEffectMenuOrganization(organization);
}

QStringList PluginPreferencesModel::lv2CustomPaths() const
{
    return toQStringList(effectsConfiguration()->lv2CustomPaths());
}

QStringList PluginPreferencesModel::vst3CustomPaths() const
{
    return toQStringList(effectsConfiguration()->vst3CustomPaths());
}

QVariantList PluginPreferencesModel::audacityPlugins() const
{
    QVariantList plugins;
    plugins.reserve(static_cast<qsizetype>(m_audacityPluginPreferences.size()));
    for (const auto& plugin : m_audacityPluginPreferences) {
        QVariantMap pluginObject;
        pluginObject[QStringLiteral("id")] = QString::fromStdString(plugin.pluginId);
        pluginObject[QStringLiteral("name")] = QString::fromStdString(plugin.pluginName);
        pluginObject[QStringLiteral("vendor")] = QString::fromStdString(plugin.vendor);
        pluginObject[QStringLiteral("version")] = QString::fromStdString(plugin.version);

        QVariantList items;
        items.reserve(static_cast<qsizetype>(plugin.items.size()));
        for (const auto& preference : plugin.items) {
            const auto& value = preference.parameter;
            QVariantMap item;
            item[QStringLiteral("key")] = QString::fromStdString(value.key);
            item[QStringLiteral("name")] = QString::fromStdString(value.name);
            item[QStringLiteral("description")] = QString::fromStdString(value.description);
            item[QStringLiteral("unit")] = QString::fromStdString(value.unit);
            item[QStringLiteral("type")] = valueTypeName(value.type);
            item[QStringLiteral("value")] = valueToVariant(preference.value);
            item[QStringLiteral("hasMinimum")] = value.minimum.has_value();
            item[QStringLiteral("hasMaximum")] = value.maximum.has_value();
            if (value.minimum) {
                item[QStringLiteral("minimum")] = valueToVariant(*value.minimum);
            }
            if (value.maximum) {
                item[QStringLiteral("maximum")] = valueToVariant(*value.maximum);
            }
            QVariantList choices;
            choices.reserve(static_cast<qsizetype>(value.enumChoices.size()));
            for (const auto& choice : value.enumChoices) {
                QVariantMap choiceObject;
                // StyledDropdown uses auto text detection internally.
                choiceObject[QStringLiteral("text")] = QString::fromStdString(choice.name).toHtmlEscaped();
                choiceObject[QStringLiteral("value")] = QString::fromStdString(choice.token);
                choices.push_back(choiceObject);
            }
            item[QStringLiteral("choices")] = choices;
            items.push_back(item);
        }
        pluginObject[QStringLiteral("preferences")] = items;
        plugins.push_back(pluginObject);
    }
    return plugins;
}

QString PluginPreferencesModel::audacityPluginError() const
{
    return m_audacityPluginError;
}

void PluginPreferencesModel::addLv2Path()
{
    QStringList paths = lv2CustomPaths();
    paths.append(QString());
    effectsConfiguration()->setLv2CustomPaths(toPathsT(paths));
    emit lv2CustomPathsChanged();
}

void PluginPreferencesModel::setLv2Path(int index, const QString& path)
{
    QStringList paths = lv2CustomPaths();
    if (index < 0 || index >= paths.size()) {
        return;
    }
    paths[index] = path;
    effectsConfiguration()->setLv2CustomPaths(toPathsT(paths));
}

void PluginPreferencesModel::removeLv2Path(int index)
{
    QStringList paths = lv2CustomPaths();
    if (index < 0 || index >= paths.size()) {
        return;
    }
    paths.removeAt(index);
    effectsConfiguration()->setLv2CustomPaths(toPathsT(paths));
    emit lv2CustomPathsChanged();
}

void PluginPreferencesModel::addVst3Path()
{
    QStringList paths = vst3CustomPaths();
    paths.append(QString());
    effectsConfiguration()->setVst3CustomPaths(toPathsT(paths));
    emit vst3CustomPathsChanged();
}

void PluginPreferencesModel::setVst3Path(int index, const QString& path)
{
    QStringList paths = vst3CustomPaths();
    if (index < 0 || index >= paths.size()) {
        return;
    }
    paths[index] = path;
    effectsConfiguration()->setVst3CustomPaths(toPathsT(paths));
}

void PluginPreferencesModel::removeVst3Path(int index)
{
    QStringList paths = vst3CustomPaths();
    if (index < 0 || index >= paths.size()) {
        return;
    }
    paths.removeAt(index);
    effectsConfiguration()->setVst3CustomPaths(toPathsT(paths));
    emit vst3CustomPathsChanged();
}

bool PluginPreferencesModel::pathExists(const QString& path) const
{
    if (path.isEmpty()) {
        return true; // empty path represents an unfilled, freshly-added row
    }
    return muse::io::Dir(muse::io::path_t(path)).exists();
}

bool PluginPreferencesModel::setAudacityPluginPreference(
    const QString& pluginId, const QString& key, const QVariant& value)
{
    for (auto& plugin : m_audacityPluginPreferences) {
        if (QString::fromStdString(plugin.pluginId) != pluginId) {
            continue;
        }
        for (auto& preference : plugin.items) {
            const auto& descriptor = preference.parameter;
            if (QString::fromStdString(descriptor.key) != key) {
                continue;
            }

            audacityplugin::Value converted;
            QString error;
            if (!variantToValue(value, descriptor.type, converted, error)) {
                updateInputError(pluginId, key,
                                 QStringLiteral("%1: %2").arg(QString::fromStdString(descriptor.name), error));
                return validateAudacityPluginPreferences();
            }

            preference.value = std::move(converted);
            updateInputError(pluginId, key, {});
            return validateAudacityPluginPreferences();
        }
    }

    updateInputError(pluginId, key, QStringLiteral("The plug-in preference no longer exists."));
    return validateAudacityPluginPreferences();
}

bool PluginPreferencesModel::validateAudacityPluginPreferences()
{
    if (!m_audacityPluginInputErrors.isEmpty()) {
        setAudacityPluginError(inputErrorsText());
        return false;
    }
    if (m_audacityPluginPreferences.empty()) {
        setAudacityPluginError({});
        return true;
    }
    if (!audacityPluginHost()) {
        setAudacityPluginError(QStringLiteral("The Plugin API host is not available."));
        return false;
    }

    QStringList errors;
    for (const auto& plugin : m_audacityPluginPreferences) {
        std::vector<audacityplugin::Value> values;
        values.reserve(plugin.items.size());
        for (const auto& item : plugin.items) {
            values.push_back(item.value);
        }
        const auto result = audacityPluginHost()->validatePreferences(plugin.pluginId, values);
        if (result != audacityplugin::Status::Ok) {
            errors.push_back(resultText(plugin,
                                        QStringLiteral("The plug-in rejected these preferences.")));
        }
    }
    setAudacityPluginError(errors.join(QStringLiteral("\n")));
    return errors.isEmpty();
}

bool PluginPreferencesModel::applyAudacityPluginPreferences()
{
    if (!validateAudacityPluginPreferences()) {
        return false;
    }

    for (auto& plugin : m_audacityPluginPreferences) {
        std::vector<audacityplugin::Value> values;
        values.reserve(plugin.items.size());
        for (const auto& item : plugin.items) {
            values.push_back(item.value);
        }
        const auto result = audacityPluginHost()->applyPreferences(
            plugin.pluginId, values);
        if (result == audacityplugin::Status::Ok) {
            continue;
        }

        setAudacityPluginError(resultText(
                                   plugin, QStringLiteral("The plug-in could not apply these preferences.")));
        return false;
    }

    setAudacityPluginError({});
    return true;
}

void PluginPreferencesModel::resetAudacityPluginPreferences()
{
    for (auto& plugin : m_audacityPluginPreferences) {
        for (auto& item : plugin.items) {
            item.value = item.parameter.defaultValue;
        }
    }
    m_audacityPluginInputErrors.clear();
    validateAudacityPluginPreferences();
    emit audacityPluginsChanged();
}

void PluginPreferencesModel::setAudacityPluginError(const QString& error)
{
    if (m_audacityPluginError == error) {
        return;
    }
    m_audacityPluginError = error;
    emit audacityPluginErrorChanged();
}

void PluginPreferencesModel::updateInputError(const QString& pluginId, const QString& key,
                                              const QString& error)
{
    const QString mapKey = pluginId + QChar(0x1f) + key;
    if (error.isEmpty()) {
        m_audacityPluginInputErrors.remove(mapKey);
    } else {
        m_audacityPluginInputErrors[mapKey] = error;
    }
}

QString PluginPreferencesModel::inputErrorsText() const
{
    return m_audacityPluginInputErrors.values().join(QStringLiteral("\n"));
}

bool PluginPreferencesModel::lv2Supported() const
{
    // `EffectFamily::LV2` is only declared in the platform-gated enum on
    // Linux (see `effects::EffectFamilies::EffectFamily`). On other
    // platforms there is nothing to ask the provider about.
#ifdef Q_OS_LINUX
    return effectsProvider()->hasEffectFamily(effects::EffectFamily::LV2);
#else
    return false;
#endif
}

bool PluginPreferencesModel::vst3Supported() const
{
    return effectsProvider()->hasEffectFamily(effects::EffectFamily::VST3);
}
}
