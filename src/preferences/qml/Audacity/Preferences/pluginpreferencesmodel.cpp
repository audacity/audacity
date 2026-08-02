/*
 * Audacity: A Digital Audio Editor
 */
#include "pluginpreferencesmodel.h"

#include <QTimer>

#include "io/dir.h"
#include "extensions/extensionpreferences.h"
#include "framework/global/containers.h"

namespace au::appshell {
static QVariant preferenceValue(const muse::Val& value)
{
    switch (value.type()) {
    case muse::Val::Type::Bool:
    case muse::Val::Type::Int:
    case muse::Val::Type::Int64:
    case muse::Val::Type::Double:
    case muse::Val::Type::String:
        return value.toQVariant();
    default:
        return {};
    }
}

static QStringList toQStringList(const muse::io::paths_t& paths)
{
    QStringList result;
    result.reserve(static_cast<int>(paths.size()));
    for (const auto& p : paths) {
        result.push_back(p.toQString());
    }
    return result;
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
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
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

    extensionsProvider()->reloadExtensions();
    reloadExtensionPreferences();
    extensionsProvider()->manifestListChanged().onNotify(this, [this] {
        reloadExtensionPreferences();
    });

    appshellConfiguration()->aboutToRevertToFactorySettings().onNotify(this, [this] {
        QTimer::singleShot(0, this, [this] {
            reloadExtensionPreferences();
        });
    });
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

QVariantList PluginPreferencesModel::extensionPreferences() const
{
    return m_extensionPreferences;
}

bool PluginPreferencesModel::setExtensionPreference(const QString& extensionId, const QString& key, const QVariant& value)
{
    const muse::Uri uri(extensionId.toStdString());
    if (!uri.isValid() || key.isEmpty() || !value.isValid()) {
        return false;
    }

    const auto setting = au::extensions::extensionPreferenceKey(uri, key.toStdString());
    QVariant converted = value;
    bool ok = true;
    switch (muse::settings()->defaultValue(setting).type()) {
    case muse::Val::Type::Int:
        converted = value.toInt(&ok);
        break;
    case muse::Val::Type::Int64:
        converted = value.toLongLong(&ok);
        break;
    case muse::Val::Type::Double:
        converted = value.toDouble(&ok);
        break;
    default:
        break;
    }
    if (!ok) {
        return false;
    }
    muse::settings()->setSharedValue(setting, muse::Val::fromQVariant(converted));
    return true;
}

void PluginPreferencesModel::reloadExtensionPreferences()
{
    QVariantList groups;
    for (const auto& manifest : extensionsProvider()->manifestList()) {
        const auto found = manifest.contributes.find("audacity.preferences");
        if (found == manifest.contributes.end() || found->second.empty()) {
            continue;
        }

        QVariantList items;
        for (const muse::ValMap& item : found->second) {
            const muse::Val idValue = muse::value(item, "id");
            const muse::Val titleValue = muse::value(item, "title");
            if (idValue.type() != muse::Val::Type::String || titleValue.type() != muse::Val::Type::String) {
                continue;
            }

            const QString key = QString::fromStdString(idValue.toString());
            const QString title = QString::fromStdString(titleValue.toString());
            const muse::Val typeValue = muse::value(item, "type");
            const QString type = typeValue.type()
                                 == muse::Val::Type::String ? QString::fromStdString(typeValue.toString()) : QStringLiteral("string");
            if (key.isEmpty() || title.isEmpty()) {
                continue;
            }

            const auto setting = au::extensions::extensionPreferenceKey(manifest.uri, key.toStdString());

            const muse::Val descriptionValue = muse::value(item, "description");
            QVariantMap mapped{
                { QStringLiteral("id"), key }, { QStringLiteral("title"), title },
                { QStringLiteral("description"), descriptionValue.type() == muse::Val::Type::String ? QString::fromStdString(
                      descriptionValue.toString()) : QString() }, { QStringLiteral("type"), type },
                { QStringLiteral("value"), muse::settings()->value(setting).toQVariant() },
            };
            QVariantList choices;
            const muse::Val choiceValue = muse::value(item, "choices");
            if (choiceValue.type() == muse::Val::Type::List) {
                for (const muse::Val& declaredChoice : choiceValue.toList()) {
                    if (declaredChoice.type() != muse::Val::Type::Map) {
                        continue;
                    }

                    const muse::ValMap entry = declaredChoice.toMap();
                    const muse::Val choiceTitle = muse::value(entry, "title");
                    const QVariant value = preferenceValue(muse::value(entry, "value"));
                    if (choiceTitle.type() != muse::Val::Type::String || !value.isValid()) {
                        continue;
                    }
                    choices.push_back(QVariantMap {
                            { QStringLiteral("value"), value },
                            { QStringLiteral("text"), QString::fromStdString(choiceTitle.toString()) },
                        });
                }
            }
            mapped.insert(QStringLiteral("choices"), choices);
            items.push_back(mapped);
        }
        if (items.empty()) {
            continue;
        }
        groups.push_back(QVariantMap {
                { QStringLiteral("extensionId"), QString::fromStdString(manifest.uri.toString()) },
                { QStringLiteral("title"), manifest.title.toQString() },
                { QStringLiteral("items"), items },
            });
    }
    m_extensionPreferences = std::move(groups);
    emit extensionPreferencesChanged();
}
}
