/*
 * Audacity: A Digital Audio Editor
 */
#include "pluginregistrationapp.h"

#include <QCoreApplication>

#include "modularity/ioc.h"
#include "types/ret.h"

#include "log.h"

using namespace muse;
using namespace au::app;

PluginRegistrationApp::PluginRegistrationApp(const CommandLineParser::AudioPluginRegistration& task,
                                             const muse::modularity::ContextPtr& ctx)
    : BaseApplication(ctx), m_task(task)
{
}

void PluginRegistrationApp::addModule(modularity::IModuleSetup* module)
{
    m_modules.push_back(module);
}

void PluginRegistrationApp::setup()
{
    const IApplication::RunMode runMode = IApplication::RunMode::AudioPluginRegistration;

    m_globalModule.setApplication(shared_from_this());
    m_globalModule.registerResources();
    m_globalModule.registerExports();
    m_globalModule.registerUiTypes();

    for (modularity::IModuleSetup* m : m_modules) {
        m->setApplication(shared_from_this());
        m->registerResources();
    }

    for (modularity::IModuleSetup* m : m_modules) {
        m->registerExports();
    }

    // Create single context (id=0)
    modularity::ContextPtr ctx = std::make_shared<modularity::Context>();
    ctx->id = 0;
    std::vector<muse::modularity::IContextSetup*>& csetups = contextSetups(ctx);
    for (modularity::IContextSetup* s : csetups) {
        s->registerExports();
    }

    m_globalModule.resolveImports();
    m_globalModule.registerApi();
    for (modularity::IModuleSetup* m : m_modules) {
        m->registerUiTypes();
        m->resolveImports();
        m->registerApi();
    }

    for (modularity::IContextSetup* s : csetups) {
        s->resolveImports();
    }

    setRunMode(runMode);

    m_globalModule.onPreInit(runMode);
    for (modularity::IModuleSetup* m : m_modules) {
        m->onPreInit(runMode);
    }

    for (modularity::IContextSetup* s : csetups) {
        s->onPreInit(runMode);
    }

    m_globalModule.onInit(runMode);
    for (modularity::IModuleSetup* m : m_modules) {
        m->onInit(runMode);
    }

    for (modularity::IContextSetup* s : csetups) {
        s->onInit(runMode);
    }

    m_globalModule.onAllInited(runMode);
    for (modularity::IModuleSetup* m : m_modules) {
        m->onAllInited(runMode);
    }

    for (modularity::IContextSetup* s : csetups) {
        s->onAllInited(runMode);
    }

    QMetaObject::invokeMethod(qApp, [this]() {
        m_globalModule.onStartApp();
        for (modularity::IModuleSetup* m : m_modules) {
            m->onStartApp();
        }
    }, Qt::QueuedConnection);

    QMetaObject::invokeMethod(qApp, [this]() {
        int code = processAudioPluginRegistration();
        qApp->exit(code);
    }, Qt::QueuedConnection);
}

void PluginRegistrationApp::finish()
{
    for (modularity::IModuleSetup* m : m_modules) {
        m->onDeinit();
    }

    m_globalModule.onDeinit();

    for (modularity::IModuleSetup* m : m_modules) {
        m->onDestroy();
    }

    m_globalModule.onDestroy();

    // Delete contexts
    for (auto& c : m_contexts) {
        qDeleteAll(c.setups);
    }
    m_contexts.clear();

    qDeleteAll(m_modules);
    m_modules.clear();

    removeIoC();
}

std::vector<muse::modularity::IContextSetup*>& PluginRegistrationApp::contextSetups(const muse::modularity::ContextPtr& ctx)
{
    for (Context& c : m_contexts) {
        if (c.ctx->id == ctx->id) {
            return c.setups;
        }
    }

    m_contexts.emplace_back();

    Context& ref = m_contexts.back();
    ref.ctx = ctx;

    modularity::IContextSetup* global = m_globalModule.newContext(ctx);
    if (global) {
        ref.setups.push_back(global);
    }

    for (modularity::IModuleSetup* m : m_modules) {
        modularity::IContextSetup* s = m->newContext(ctx);
        if (s) {
            ref.setups.push_back(s);
        }
    }

    return ref.setups;
}

modularity::ContextPtr PluginRegistrationApp::setupNewContext()
{
    // Single context mode: only allow one context
    static bool once = false;
    IF_ASSERT_FAILED(!once) {
        return nullptr;
    }
    once = true;

    // Context was already created in setup()
    if (!m_contexts.empty()) {
        return m_contexts.front().ctx;
    }

    return nullptr;
}

int PluginRegistrationApp::contextCount() const
{
    return static_cast<int>(m_contexts.size());
}

std::vector<modularity::ContextPtr> PluginRegistrationApp::contexts() const
{
    std::vector<modularity::ContextPtr> ctxs;
    ctxs.reserve(m_contexts.size());
    for (const Context& c : m_contexts) {
        ctxs.push_back(c.ctx);
    }
    return ctxs;
}

int PluginRegistrationApp::processAudioPluginRegistration()
{
    Ret ret = make_ret(Ret::Code::Ok);

    if (m_task.failedPlugin) {
        ret = registerAudioPluginsScenario()->registerFailedPlugin(m_task.pluginPath, m_task.failCode);
    } else {
        ret = registerAudioPluginsScenario()->registerPlugin(m_task.pluginPath);
    }

    if (!ret) {
        LOGE() << ret.toString();
    }

    return ret.code();
}
