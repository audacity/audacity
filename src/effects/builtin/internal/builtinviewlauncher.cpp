/*
* Audacity: A Digital Audio Editor
*/
#include "builtinviewlauncher.h"

#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-effects/Effect.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace au::effects;

static const char16_t* BUILTIN_VIEWER_URI = u"audacity://effects/builtin_viewer?type=%1&instanceId=%2";

muse::Ret BuiltinViewLauncher::showEffect(const EffectId& effectId, const EffectInstanceId& instanceId)
{
    PluginID pluginID = effectId.toStdString();
    Effect* effect = dynamic_cast<Effect*>(EffectManager::Get().GetEffect(pluginID));
    IF_ASSERT_FAILED(effect) {
        LOGE() << "effect not available, effectId: " << effectId;
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    muse::String type = au3::wxToString(effect->GetSymbol().Internal());
    muse::Ret ret = interactive()->open(muse::String(BUILTIN_VIEWER_URI)
                                        .arg(type)
                                        .arg(size_t(instanceId)).toStdString()
                                        ).ret;

    return ret;
}
