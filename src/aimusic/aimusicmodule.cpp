/*
 * Audacity: A Digital Audio Editor
 */
#include "aimusicmodule.h"

#include "modularity/ioc.h"
#include "ui/iuiactionsregister.h"

#include "internal/aimusiccontroller.h"
#include "internal/aimusicuiactions.h"

using namespace au::aimusic;
using namespace muse;
using namespace muse::modularity;
using namespace muse::ui;

static const std::string mname("aimusic");

std::string AiMusicModule::moduleName() const
{
    return mname;
}

IContextSetup* AiMusicModule::newContext(const ContextPtr& ctx) const
{
    return new AiMusicContext(ctx);
}

void AiMusicContext::registerExports()
{
    m_controller = std::make_shared<AiMusicController>(iocContext());
    m_uiActions = std::make_shared<AiMusicUiActions>(iocContext(), m_controller);
}

void AiMusicContext::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_controller->init();

    auto actionsRegister = ioc()->resolve<IUiActionsRegister>(mname);
    if (actionsRegister) {
        actionsRegister->reg(m_uiActions);
    }
}
