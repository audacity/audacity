/*
 * Audacity: A Digital Audio Editor
 */
#include "extensionpreferences.h"

#include <QByteArray>
#include <QDir>

#include "framework/global/iglobalconfiguration.h"
#include "framework/global/modularity/ioc.h"
#include "framework/extensions/extensionstypes.h"

#include "framework/global/containers.h"

namespace au::extensions {
muse::Settings::Key extensionPreferenceKey(const muse::Uri& uri, const std::string& key)
{
    const std::string id = QByteArray::fromStdString(uri.toString()).toHex().toStdString();
    return { "extensions", "preferences/" + id + "/" + key };
}

QVariant extensionPreferenceDefault(const muse::ValMap& preference)
{
    const muse::Val value = muse::value(preference, "default");
    switch (value.type()) {
    case muse::Val::Type::Bool:
    case muse::Val::Type::Int:
    case muse::Val::Type::Int64:
    case muse::Val::Type::Double:
        return value.toQVariant();
    case muse::Val::Type::String:
        break;
    default:
        return {};
    }

    QString result = value.toQVariant().toString();
    static const QString genericDataToken = QStringLiteral("${GENERIC_DATA}");
    if (!result.startsWith(genericDataToken)) {
        return result;
    }

    static muse::GlobalInject<muse::IGlobalConfiguration> configuration;
    if (!configuration()) {
        return {};
    }
    result.replace(0, genericDataToken.size(), configuration()->genericDataPath().toQString());
    return QDir::cleanPath(result);
}

void registerExtensionPreferenceDefaults(const muse::extensions::Manifest& manifest)
{
    const auto contribution = manifest.contributes.find("audacity.preferences");
    if (contribution == manifest.contributes.end()) {
        return;
    }

    for (const muse::ValMap& preference : contribution->second) {
        const muse::Val id = muse::value(preference, "id");
        const QVariant value = extensionPreferenceDefault(preference);
        if (id.type() != muse::Val::Type::String || !value.isValid()) {
            continue;
        }
        muse::settings()->setDefaultValue(extensionPreferenceKey(manifest.uri, id.toString()), muse::Val::fromQVariant(value));
    }
}
} // namespace au::extensions
