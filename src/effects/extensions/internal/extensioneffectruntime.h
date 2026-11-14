/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>
#include <string>
#include <vector>

#include <QJSValue>
#include <QVariantMap>

#include "framework/extensions/iextensionsession.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/types/ret.h"

#include "extensioneffecttypes.h"

namespace au::trackedit::api {
class ProjectEditSession;
}

namespace au::effects::extensions {
class ExtensionEffectRuntime final
{
public:
    ExtensionEffectRuntime(const EffectDescriptor& descriptor, const muse::modularity::ContextPtr& context);
    ~ExtensionEffectRuntime();

    muse::Ret initialize();
    muse::RetVal<std::vector<ParameterDescriptor> > parameters();
    muse::RetVal<std::string> validate(const Settings& settings);
    muse::RetVal<QJSValue> process(const Settings& settings, trackedit::api::ProjectEditSession& projectSession, QObject* task);
    muse::RetVal<bool> observePromise(const QJSValue& value, QObject* observer);
    bool hasProgressCallback() const;
    muse::Ret updateProgress(trackedit::api::ProjectEditSession& projectSession, QObject* task);

private:
    QJSValue call(const char* method, const QJSValueList& arguments = {});
    QVariantMap settingsMap(const Settings& settings) const;

    EffectDescriptor m_descriptor;
    std::unique_ptr<muse::extensions::IExtensionSession> m_session;
    QJSValue m_effect;
};
} // namespace au::effects::extensions
