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

enum class EffectUIMode {
    VendorUI = 0,      // Use plugin's native/graphical UI
    FallbackUI = 1,   // Use Audacity's fallback UI
};

// Parameter types for auto-generated UI
enum class ParameterType {
    Unknown = -1,
    Toggle,        // Boolean on/off
    Dropdown,      // Enumerated list of choices
    Slider,        // Continuous value with range
    Numeric,       // Numeric input field
    ReadOnly,      // Display-only (meter, status)
};

// Parameter metadata for auto-generated UI
// Values are stored in "Full Range" (display) representation, e.g., -60 to +6 for dB.
// Use getNormalizedValue() to convert to normalized [0,1] for plugin API calls.
struct ParameterInfo {
    muse::String id;              // Unique parameter identifier
    muse::String name;            // Display name
    muse::String description;     // Optional description
    muse::String units;           // Unit string (dB, Hz, %, etc.)
    muse::String group;           // Parameter group/category

    ParameterType type = ParameterType::Unknown;

    // Value range in "Full Range" (display) values, e.g., -60 to +6 for dB, 20 to 20000 for Hz
    // For plugins that don't implement proper conversions, these will be 0.0 to 1.0.
    double minValue = 0.0;
    double maxValue = 0.0;
    double defaultValue = 0.0;
    double currentValue = 0.0;

    // Formatted value string from plugin (e.g., "440 Hz", "3.5 dB", "-12.0 dB")
    muse::String currentValueString;

    // For discrete parameters
    int stepCount = 0;            // 0=continuous, 1=toggle, >1=discrete steps
    double stepSize = 0.0;        // Step increment for discrete values

    // For dropdown/enumeration parameters
    std::vector<muse::String> enumValues;  // List of choice labels
    std::vector<double> enumIndices;       // Corresponding normalized values

    // Flags
    bool isReadOnly = false;
    bool isHidden = false;
    bool isLogarithmic = false;
    bool isInteger = false;
    bool canAutomate = true;

    bool isValid() const { return !id.empty(); }

    //! Convert current "Full Range" value to normalized [0,1] for plugin API calls
    double getNormalizedValue() const
    {
        if (maxValue == minValue) {
            return 0.0; // Avoid division by zero
        }
        return (currentValue - minValue) / (maxValue - minValue);
    }

    //! Convert a "Full Range" value to normalized [0,1]
    double toNormalized(double fullRangeValue) const
    {
        if (maxValue == minValue) {
            return 0.0;
        }
        return (fullRangeValue - minValue) / (maxValue - minValue);
    }

    //! Convert a normalized [0,1] value to "Full Range"
    double toFullRange(double normalizedValue) const
    {
        return minValue + normalizedValue * (maxValue - minValue);
    }
};

using ParameterInfoList = std::vector<ParameterInfo>;

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

const std::string DESTRUCTIVE_EFFECT_VIEWER_URI = "audacity://effects/destructive_viewer?instanceId=%1&effectFamily=%2";

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
