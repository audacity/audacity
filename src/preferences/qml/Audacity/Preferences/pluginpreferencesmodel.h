/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QtQml/qqmlregistration.h>
#include <QMap>
#include <QVariantList>
#include <QStringList>

#include <vector>

#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/ieffectsconfiguration.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "audacityplugin/iaudacitypluginhost.h"
#include "async/asyncable.h"
#include "modularity/ioc.h"

#include <QObject>

namespace au::appshell {
class PluginPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(effects::EffectMenuOrganization effectMenuOrganization READ effectMenuOrganization NOTIFY effectMenuOrganizationChanged)

    Q_PROPERTY(QStringList lv2CustomPaths READ lv2CustomPaths NOTIFY lv2CustomPathsChanged)
    Q_PROPERTY(QStringList vst3CustomPaths READ vst3CustomPaths NOTIFY vst3CustomPathsChanged)

    Q_PROPERTY(QVariantList audacityPlugins READ audacityPlugins NOTIFY audacityPluginsChanged)
    Q_PROPERTY(QString audacityPluginError READ audacityPluginError NOTIFY audacityPluginErrorChanged)

    Q_PROPERTY(bool lv2Supported READ lv2Supported CONSTANT)
    Q_PROPERTY(bool vst3Supported READ vst3Supported CONSTANT)

    muse::GlobalInject<effects::IEffectsConfiguration> effectsConfiguration;
    muse::GlobalInject<effects::IEffectsProvider> effectsProvider;
    muse::GlobalInject<audacityplugin::IAudacityPluginHost> audacityPluginHost;

public:
    explicit PluginPreferencesModel(QObject* parent = nullptr);

    effects::EffectMenuOrganization effectMenuOrganization() const;
    Q_INVOKABLE void setEffectMenuOrganization(effects::EffectMenuOrganization);

    QStringList lv2CustomPaths() const;
    QStringList vst3CustomPaths() const;
    QVariantList audacityPlugins() const;
    QString audacityPluginError() const;

    bool lv2Supported() const;
    bool vst3Supported() const;

    Q_INVOKABLE void addLv2Path();
    Q_INVOKABLE void setLv2Path(int index, const QString& path);
    Q_INVOKABLE void removeLv2Path(int index);

    Q_INVOKABLE void addVst3Path();
    Q_INVOKABLE void setVst3Path(int index, const QString& path);
    Q_INVOKABLE void removeVst3Path(int index);

    Q_INVOKABLE bool pathExists(const QString& path) const;

    Q_INVOKABLE bool setAudacityPluginPreference(
        const QString& pluginId, const QString& key, const QVariant& value);
    Q_INVOKABLE bool applyAudacityPluginPreferences();
    Q_INVOKABLE void resetAudacityPluginPreferences();

    Q_INVOKABLE void init();

signals:
    void effectMenuOrganizationChanged();
    void lv2CustomPathsChanged();
    void vst3CustomPathsChanged();
    void audacityPluginsChanged();
    void audacityPluginErrorChanged();

private:
    bool validateAudacityPluginPreferences();
    void setAudacityPluginError(const QString& error);
    void updateInputError(const QString& pluginId, const QString& key, const QString& error);
    QString inputErrorsText() const;

    std::vector<audacityplugin::PluginPreferences> m_audacityPluginPreferences;
    QMap<QString, QString> m_audacityPluginInputErrors;
    QString m_audacityPluginError;
};
}
