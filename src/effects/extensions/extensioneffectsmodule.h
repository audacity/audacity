/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/imodulesetup.h"

namespace muse::extensions {
class IExtensionsProvider;
}

namespace au::effects::extensions {
class ExtensionEffectLoader;
class ExtensionEffectsScanner;
class ExtensionEffectsRepository;

class ExtensionEffectsModule final : public muse::modularity::IModuleSetup
{
public:
    ExtensionEffectsModule();

    std::string moduleName() const override;
    void resolveImports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;
    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& context) const override;

private:
    std::shared_ptr<muse::extensions::IExtensionsProvider> m_extensionsProvider;
    std::shared_ptr<ExtensionEffectsRepository> m_repository;
    std::shared_ptr<ExtensionEffectLoader> m_effectLoader;
    std::shared_ptr<ExtensionEffectsScanner> m_scanner;
};

class ExtensionEffectsContext final : public muse::modularity::IContextSetup, public muse::async::Asyncable
{
public:
    ExtensionEffectsContext(const muse::modularity::ContextPtr& context, std::shared_ptr<ExtensionEffectsScanner> scanner)
        : IContextSetup(context), m_scanner(std::move(scanner)) {}

    void resolveImports() override;
    void onInit(const muse::IApplication::RunMode&) override;

private:
    void refreshPlugins() const;

    std::shared_ptr<ExtensionEffectsScanner> m_scanner;
};
} // namespace au::effects::extensions
