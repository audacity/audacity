/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "global/types/string.h"
#include "global/types/ratio.h"
#include "actions/actiontypes.h"

class Effect;
class EffectInstanceEx;
class RealtimeEffectState;
class EffectSettingsAccess;
struct EffectSettings;
class wxString;
namespace au::effects {
using secs_t = muse::number_t<double>;
using percent_t = muse::number_t<float>;
using ratio_t = muse::ratio_t;
using db_t = muse::db_t;
using rms_t = float;

using EffectId = muse::String;              // PluginID from AU3
using Effect = ::Effect;                    // Effect from AU3
using EffectInstanceId = int;
using EffectInstance = ::EffectInstanceEx;  // EffectInstanceEx from AU3
using EffectSettings = ::EffectSettings;
using EffectSettingsAccess = ::EffectSettingsAccess;
using EffectSettingsAccessPtr = std::shared_ptr<EffectSettingsAccess>;
using RealtimeEffectState = ::RealtimeEffectState;
using RealtimeEffectStatePtr = std::shared_ptr<RealtimeEffectState>;
using RealtimeEffectStateId = int;
using TrackId = long;
using EffectChainLinkIndex = int;

enum class EffectFamily {
    Unknown = -1,
    Builtin,
    VST3,
};

struct EffectMeta {
    EffectId id;
    EffectFamily family = EffectFamily::Unknown;
    muse::String title;
    muse::String description;
    muse::String version;
    muse::String vendor;

    muse::String categoryId;

    bool isRealtimeCapable = false;
    bool supportsMultipleClipSelection = true;

    bool isValid() const { return !id.empty(); }
};

using EffectMetaList = std::vector<EffectMeta>;

constexpr const char16_t* EFFECT_OPEN_ACTION = u"action://effects/open?effectId=%1";
constexpr const char16_t* REALTIME_EFFECT_ADD_ACTION = u"action://effects/realtime-add?effectId=%1";
constexpr const char16_t* REALTIME_EFFECT_REPLACE_ACTION = u"action://effects/realtime-replace?effectId=%1";

inline muse::actions::ActionQuery makeEffectAction(const char16_t* action, const EffectId id)
{
    return muse::actions::ActionQuery(muse::String(action).arg(id));
}

inline EffectId effectIdFromAction(const muse::actions::ActionQuery& action)
{
    return EffectId::fromStdString(action.param("effectId").toString());
}

inline EffectId effectIdFromAction(const QString& action)
{
    return effectIdFromAction(muse::actions::ActionQuery { action });
}

struct EffectCategory {
    muse::String id;
    muse::String title;

    bool isValid() const { return !id.empty(); }
};

using EffectCategoryList = std::vector<EffectCategory>;

constexpr const char16_t* VST_CATEGORY_ID = u"vst";
constexpr const char16_t* BUILTIN_CATEGORY_ID = u"builtin";

using PresetId = wxString;
using PresetIdList = std::vector<PresetId>;
}
