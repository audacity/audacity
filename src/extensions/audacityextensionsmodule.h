/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "framework/extensions/iextensionsprovider.h"
#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/imodulesetup.h"

namespace au::extensions {
class AudacityExtensionsModule final : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerApi() override;
    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& context) const override;
};

class AudacityExtensionsContext final : public muse::modularity::IContextSetup, public muse::async::Asyncable
{
public:
    explicit AudacityExtensionsContext(const muse::modularity::ContextPtr& context);

    void onInit(const muse::IApplication::RunMode& mode) override;

private:
    void enableExtensionsByDefault() const;
    void registerPreferenceDefaults() const;

    std::shared_ptr<muse::extensions::IExtensionsProvider> m_extensions;
};
} // namespace au::extensions
