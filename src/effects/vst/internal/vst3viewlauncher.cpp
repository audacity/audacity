/*
* Audacity: A Digital Audio Editor
*/
#include "vst3viewlauncher.h"

#include "libraries/lib-vst3/VST3Instance.h"

#include "musevstplugininstance.h"

#include "log.h"

using namespace au::effects;

static const char16_t* VST_VIEWER_URI = u"audacity://effects/vst_viewer?instanceId=%1&modal=false&floating=true";

muse::Ret Vst3ViewLauncher::showEffect(const EffectId& effectId, const EffectInstanceId& instanceId)
{
    std::shared_ptr<EffectInstance> instance = instancesRegister()->instanceById(instanceId);
    IF_ASSERT_FAILED(instance) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    std::shared_ptr<VST3Instance> auVst3Instance = std::dynamic_pointer_cast<VST3Instance>(instance);
    IF_ASSERT_FAILED(auVst3Instance) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    std::shared_ptr<MuseVstPluginInstance> museVstInstance = std::make_shared<MuseVstPluginInstance>(effectId, instanceId, auVst3Instance);
    museInstancesRegister()->registerFxPlugin(0, 0, museVstInstance);

    muse::Ret ret = interactive()->open(muse::String(VST_VIEWER_URI)
                                        .arg(size_t(museVstInstance->id())).toStdString()
                                        ).ret;

    return ret;
}
