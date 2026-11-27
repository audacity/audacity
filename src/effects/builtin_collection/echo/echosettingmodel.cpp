/*
 * Audacity: A Digital Audio Editor
 */
#include "echosettingmodel.h"

#include "../common/measureunits.h"

#include "framework/global/translation.h"

namespace au::effects {
EchoSettingModel::EchoSettingModel(QObject* parent, int instanceId)
    : EffectSettingModelImpl<EchoEffect>(parent, instanceId, {
        { "Delay", {
              //: Label of the Echo effect control that sets the time between the original sound and each echo, in seconds
              muse::qtrc("effects/echo", "Delay time"),
              units::seconds().m_symbol,
              makeSettingAccess(EchoEffect::Delay) } },
        { "Decay", {
              //: Label of the Echo effect control that sets how much quieter each echo is than the previous one. A factor, not a time.
              muse::qtrc("effects/echo", "Decay factor"),
              units::none().m_symbol,
              makeSettingAccess(EchoEffect::Decay) } },
    })
{
}
}
