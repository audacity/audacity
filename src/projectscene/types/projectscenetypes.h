/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_TRACKTYPES_H
#define AU_PROJECTSCENE_TRACKTYPES_H

#include <qobjectdefs.h>

#include "actions/actiontypes.h"
#include "global/types/number.h"
#include "trackedit/trackedittypes.h"

namespace au::projectscene {
class TrackTypes
{
    Q_GADGET

public:
    //! NOTE: must be in sync with au::trackedit::TrackType
    enum class Type {
        UNDEFINED = 0,
        MONO,
        STEREO,
        LABEL
    };
    Q_ENUM(Type)
};

class ClipKey
{
    Q_GADGET

public:

    ClipKey() = default;
    ClipKey(const trackedit::ClipKey& k)
        : key(k) {}

    trackedit::ClipKey key;
};

class ClipTime
{
    Q_GADGET

public:

    double clipStartTime = 0.0;
    double clipEndTime = 0.0;

    double itemStartTime = 0.0;
    double itemEndTime = 0.0;

    double selectionStartTime = 0.0;
    double selectionEndTime = 0.0;

    inline bool operator==(const ClipTime& other) const
    {
        return muse::is_equal(clipStartTime, other.clipStartTime)
               && muse::is_equal(clipEndTime, other.clipEndTime)
               && muse::is_equal(itemStartTime, other.itemStartTime)
               && muse::is_equal(itemEndTime, other.itemEndTime)
               && muse::is_equal(selectionStartTime, other.selectionStartTime)
               && muse::is_equal(selectionEndTime, other.selectionEndTime);
    }

    inline bool operator!=(const ClipTime& other) const { return !this->operator==(other); }
};

class VerticalRulerTypes
{
    Q_GADGET
public:
    enum class Type {
        LINEAR_AMP = 0,
        LOGARITHMIC_DB,
        LINEAR_DB
    };
    Q_ENUM(Type)
};

enum class SnapType {
    Bar,

    Half,
    Quarter,
    Eighth,
    Sixteenth,
    ThirtySecond,
    SixtyFourth,
    HundredTwentyEighth,

    Seconds,
    Deciseconds,
    Centiseconds,
    Milliseconds,
    Samples,

    FilmFrames,
    NTSCFrames,
    NTSCFramesDrop,
    PALFrames,

    CDDAFrames
};

enum class TimelineRulerMode {
    MINUTES_AND_SECONDS = 0,
    BEATS_AND_MEASURES
};

enum class Direction {
    Left = 0,
    Right
};

constexpr const char16_t* COLOR_CHANGE_ACTION = u"action://trackedit/clip/change-color?color=%1";
inline muse::actions::ActionQuery makeColorChangeAction(const std::string& colorHex)
{
    return muse::actions::ActionQuery(muse::String(COLOR_CHANGE_ACTION).arg(muse::String::fromStdString(
                                                                                colorHex)));
}
}

#endif // AU_PROJECTSCENE_TRACKTYPES_H
