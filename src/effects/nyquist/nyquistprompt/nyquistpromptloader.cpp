/*
 * Audacity: A Digital Audio Editor
 */
#include "nyquistpromptloader.h"

#include "au3-module-manager/PluginManager.h"
#include "au3-effects/LoadEffects.h"

namespace au::effects {
void NyquistPromptLoader::preInit()
{
    //! NOTE preInit() only creates static Registration objects (doesn't use `this`).
    //! Must run at module level before Au3WrapModule::onInit() sets sInitialized = true.
    static BuiltinEffectsModule::Registration< NyquistPromptEffect > regNyquistPrompt;
}

void NyquistPromptLoader::init()
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

        EffectMeta meta;
        meta.id = au3::wxToString(desc.GetID());
        meta.family = EffectFamily::Builtin;
        meta.category = utils::builtinEffectCategoryIdString(toAu4EffectCategory(desc.GetEffectGroup()));
        meta.title = muse::mtrc("effects", "Nyquist prompt");
        meta.description = muse::mtrc("effects", "Nyquist prompt effect");
        meta.isRealtimeCapable = desc.IsEffectRealtime();
        meta.supportsMultipleClipSelection = false;
        meta.vendor = "Audacity";
        meta.path = desc.GetPath();
        meta.type = EffectType::Tool;

        builtinEffectsRepository()->registerMeta(meta);
    }
}
}
