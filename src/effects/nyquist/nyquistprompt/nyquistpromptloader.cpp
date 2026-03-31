/*
 * Audacity: A Digital Audio Editor
 */
#include "nyquistpromptloader.h"

#include "effects/effects_base/internal/au3/au3effectsutils.h"

#include "au3-module-manager/PluginManager.h"
#include "au3-effects/LoadEffects.h"

#include "framework/global/translation.h"

namespace au::effects {
void NyquistPromptLoader::preInit()
{
    //! NOTE preInit() only creates static Registration objects (doesn't use `this`).
    //! Must run at module level before Au3WrapModule::onInit() sets sInitialized = true.
    static BuiltinEffectsModule::Registration< NyquistPromptEffect > regNyquistPrompt;
}

void NyquistPromptLoader::init()
{
    qmlRegisterSingletonType<NyquistPromptViewModelFactory>("Audacity.Nyquist", 1, 0, "NyquistPromptViewModelFactory",
                                                            [] (QQmlEngine*, QJSEngine*) -> QObject* {
        return new NyquistPromptViewModelFactory();
    });
    const auto effectName = au3::wxToString(NyquistPromptEffect::Symbol.Internal());
    builtinEffectsViewRegister()->regUrl(effectName, u"qrc:/nyquistprompt/NyquistPromptView.qml");

    effectsProvider()->initialized().onNotify(this, [this]() {
        registerNyquistPromptEffect();
    });
}

void NyquistPromptLoader::registerNyquistPromptEffect()
{
    for (const PluginDescriptor& desc : PluginManager::Get().PluginsOfType(PluginTypeEffect)) {
        const auto& symbol = desc.GetSymbol();
        if (symbol != NyquistPromptEffect::Symbol) {
            continue;
        }
        qmlRegisterSingletonType<NyquistPromptViewModelFactory>("Audacity.Nyquist", 1, 0, "NyquistPromptViewModelFactory",
                                                                [] (QQmlEngine*, QJSEngine*) -> QObject* {
            return new NyquistPromptViewModelFactory();
        });
        const auto effectName = au3::wxToString(NyquistPromptEffect::Symbol.Internal());
        builtinEffectsViewRegister()->regUrl(effectName, u"qrc:/nyquistprompt/NyquistPromptView.qml");

        // With the nyquist prompt, one can do the same thing to multiple clips at once (right?)
        constexpr auto supportsMultipleClipSelection = true;
        const auto title = muse::mtrc("effects", "Nyquist prompt");
        const auto description = muse::mtrc("effects", "Nyquist prompt effect");
        const EffectMeta meta = toEffectMeta(desc, EffectFamily::Builtin, title, description, supportsMultipleClipSelection);

        builtinEffectsRepository()->registerMeta(meta);
        break;
    }
}
}
