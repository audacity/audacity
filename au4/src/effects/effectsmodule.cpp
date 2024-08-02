/*
* Audacity: A Digital Audio Editor
*/
#include "effectsmodule.h"

#include "ui/iuiactionsregister.h"
#include "ui/iinteractiveuriregister.h"

#include "internal/effectsprovider.h"
#include "internal/effectsconfiguration.h"
#include "internal/effectsactionscontroller.h"
#include "internal/effectsuiengine.h"
#include "internal/effectsuiactions.h"
#include "internal/effectsparametersservice.h"

#include "view/effectbuilder.h"

#include "builtineffects/amplify/amplify.h"

using namespace au::effects;

static void effects_init_qrc()
{
    Q_INIT_RESOURCE(effects);
}

std::string EffectsModule::moduleName() const
{
    return "effects";
}

void EffectsModule::registerExports()
{
    m_provider = std::make_shared<EffectsProvider>();
    m_configuration = std::make_shared<EffectsConfiguration>();
    m_actionsController = std::make_shared<EffectsActionsController>();
    m_effectsParametersService = std::make_shared<EffectsParametersService>();

    ioc()->registerExport<IEffectsProvider>(moduleName(), m_provider);
    ioc()->registerExport<IEffectsConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IEffectsUiEngine>(moduleName(), new EffectsUiEngine());
    ioc()->registerExport<EffectsParametersService>(moduleName(), m_effectsParametersService);
}

void EffectsModule::resolveImports()
{
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(std::make_shared<EffectsUiActions>());
    }
    auto ir = ioc()->resolve<muse::ui::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("muse://effects/viewer"), "Audacity/Effects/EffectsViewerDialog.qml");
    }
}

void EffectsModule::registerResources()
{
    effects_init_qrc();
}

void EffectsModule::registerUiTypes()
{
    qmlRegisterType<EffectBuilder>("Audacity.Effects", 1, 0, "EffectBuilder");

    qmlRegisterType<EffectAmplify>("Audacity.Effects", 1, 0, "EffectAmplify"); // todo
}

void EffectsModule::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode != muse::IApplication::RunMode::GuiApp) {
        return;
    }

    m_actionsController->init();
}

void EffectsModule::onDelayedInit()
{
    m_provider->reloadEffects();
}

void EffectsModule::onDeinit()
{
}
