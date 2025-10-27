/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "framework/global/types/string.h"
#include "framework/global/types/ratio.h"
#include "framework/actions/actiontypes.h"

class Effect;
class EffectInstanceEx;
class RealtimeEffectState;
class EffectSettingsAccess;
struct EffectSettings;
class wxString;

namespace muse {
class String;
namespace uicomponents {
class MenuItem;
using MenuItemList = QList<MenuItem*>;
}
}

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
using TrackId = long;
using EffectChainLinkIndex = int;

enum class EffectMenuOrganization {
    Grouped = 0,
    Flat = 1,
};

class EffectFamilies
{
    Q_GADGET
public:
    enum class EffectFamily {
        Unknown = -1,
        Builtin,
        VST3,
        LV2,
        AudioUnit,
    };
    Q_ENUM(EffectFamily)
};

using EffectFamily = EffectFamilies::EffectFamily;

enum class BuiltinEffectCategoryId {
    None,
    VolumeAndCompression,
    Fading,
    PitchAndTempo,
    EqAndFilters,
    NoiseRemovalAndRepair,
    DelayAndReverb,
    DistortionAndModulation,
    Special,
    Legacy,
};

enum class EffectType {
    Unknown = -1,
    Analyzer,
    Generator,
    Processor,
};

struct EffectMeta {
    EffectId id;
    EffectFamily family = EffectFamily::Unknown;
    EffectType type = EffectType::Unknown;
    muse::String title;
    muse::String description;
    muse::String vendor;
    muse::io::path_t path;

    muse::String category;

    bool isRealtimeCapable = false;
    bool supportsMultipleClipSelection = true;

    bool isValid() const { return !id.empty(); }
};

using EffectMetaList = std::vector<EffectMeta>;

const std::string EFFECT_OPEN_ACTION = "action://effects/open?effectId=%1";
const std::string REALTIME_EFFECT_ADD_ACTION = "action://effects/realtime-add?effectId=%1";
const std::string REALTIME_EFFECT_REPLACE_ACTION = "action://effects/realtime-replace?effectId=%1";

const std::string EFFECT_VIEWER_URI = "audacity://effects/effect_viewer?effectFamily=%1";

inline std::string makeEffectAction(const std::string& action, const EffectId& id)
{
    return QString::fromStdString(action).arg(id).toStdString();
}

inline EffectId effectIdFromAction(const muse::actions::ActionQuery& action)
{
    return EffectId::fromStdString(action.param("effectId").toString());
}

inline EffectId effectIdFromAction(const QString& action)
{
    return effectIdFromAction(muse::actions::ActionQuery { action });
}

using PresetId = wxString;
using PresetIdList = std::vector<PresetId>;

class IEffectMenuItemFactory
{
public:
    virtual ~IEffectMenuItemFactory() = default;
    virtual muse::uicomponents::MenuItem* makeMenuSeparator() = 0;
    virtual muse::uicomponents::MenuItem* makeMenuEffectItem(const EffectId& effectId) = 0;
    virtual muse::uicomponents::MenuItem* makeMenuEffect(const muse::String& title, const muse::uicomponents::MenuItemList& items) = 0;
};
}
