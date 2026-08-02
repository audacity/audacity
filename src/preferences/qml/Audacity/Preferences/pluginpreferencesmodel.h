/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QtQml/qqmlregistration.h>
#include <QStringList>

#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/ieffectsconfiguration.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "appshell/iappshellconfiguration.h"
#include "framework/extensions/iextensionsprovider.h"
#include "async/asyncable.h"
#include "modularity/ioc.h"

#include <QObject>

namespace au::appshell {
class PluginPreferencesModel : public QObject, public muse::Contextable, public muse::async::Asyncable
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(effects::EffectMenuOrganization effectMenuOrganization READ effectMenuOrganization NOTIFY effectMenuOrganizationChanged)

    Q_PROPERTY(QStringList lv2CustomPaths READ lv2CustomPaths NOTIFY lv2CustomPathsChanged)
    Q_PROPERTY(QStringList vst3CustomPaths READ vst3CustomPaths NOTIFY vst3CustomPathsChanged)

    Q_PROPERTY(bool lv2Supported READ lv2Supported CONSTANT)
    Q_PROPERTY(bool vst3Supported READ vst3Supported CONSTANT)
    Q_PROPERTY(QVariantList extensionPreferences READ extensionPreferences NOTIFY extensionPreferencesChanged)

    muse::GlobalInject<effects::IEffectsConfiguration> effectsConfiguration;
    muse::GlobalInject<effects::IEffectsProvider> effectsProvider;
    muse::GlobalInject<IAppShellConfiguration> appshellConfiguration;

    muse::ContextInject<muse::extensions::IExtensionsProvider> extensionsProvider{ this };

public:
    explicit PluginPreferencesModel(QObject* parent = nullptr);

    effects::EffectMenuOrganization effectMenuOrganization() const;
    Q_INVOKABLE void setEffectMenuOrganization(effects::EffectMenuOrganization);

    QStringList lv2CustomPaths() const;
    QStringList vst3CustomPaths() const;

    bool lv2Supported() const;
    bool vst3Supported() const;
    QVariantList extensionPreferences() const;
    Q_INVOKABLE bool setExtensionPreference(const QString& extensionId, const QString& key, const QVariant& value);

    Q_INVOKABLE void addLv2Path();
    Q_INVOKABLE void setLv2Path(int index, const QString& path);
    Q_INVOKABLE void removeLv2Path(int index);

    Q_INVOKABLE void addVst3Path();
    Q_INVOKABLE void setVst3Path(int index, const QString& path);
    Q_INVOKABLE void removeVst3Path(int index);

    Q_INVOKABLE bool pathExists(const QString& path) const;

    Q_INVOKABLE void init();

signals:
    void effectMenuOrganizationChanged();
    void lv2CustomPathsChanged();
    void vst3CustomPathsChanged();
    void extensionPreferencesChanged();

private:
    void reloadExtensionPreferences();

    QVariantList m_extensionPreferences;
};
}
