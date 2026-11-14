/*
 * Audacity: A Digital Audio Editor
 */
#include "nyquistpromptloader.h"
#include "nyquistprompteffect.h"
#include "nyquistpromptviewmodel.h"

#include "au3-effects/LoadEffects.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include <QtQml/qqml.h>

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
}
}
