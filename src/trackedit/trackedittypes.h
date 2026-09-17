/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <variant>

#include "framework/global/types/number.h"
#include "framework/global/logstream.h"

namespace au::trackedit {
using msecs_t = int64_t;
using secs_t = muse::number_t<double>;
using samples_t = uint64_t;
using sample_rate_t = uint64_t;
using audioch_t = uint8_t;
using volume_db_t = float;
using volume_dbfs_t = float;
using gain_t = float;
using pan_t = float;

using TrackId = int64_t;
using LabelTrackId = int64_t;
constexpr TrackId INVALID_TRACK = -1;

using TrackItemId = int64_t;
using ClipId = TrackItemId;
using LabelId = TrackItemId;
constexpr TrackItemId INVALID_TRACK_ITEM = -1;

using TrackIdList = std::vector<TrackId>;

struct TrackItemKey
{
    TrackId trackId = INVALID_TRACK;
    TrackItemId itemId = INVALID_TRACK_ITEM;

    TrackItemKey() = default;
    TrackItemKey(const TrackId t, const TrackItemId o)
        : trackId(t), itemId(o) {}

    inline bool isValid() const { return trackId != INVALID_TRACK && itemId != INVALID_TRACK_ITEM; }

    inline bool operator==(const TrackItemKey& k) const { return trackId == k.trackId && itemId == k.itemId; }
    inline bool operator!=(const TrackItemKey& k) const { return !this->operator==(k); }
    inline bool operator<(const TrackItemKey& k) const { return trackId < k.trackId || (trackId == k.trackId && itemId < k.itemId); }
};

using TrackItemKeyList = std::vector<TrackItemKey>;

using ClipKey = TrackItemKey;
using ClipKeyList = TrackItemKeyList;

using LabelKey = TrackItemKey;
using LabelKeyList = TrackItemKeyList;

//! NOTE: what the keyboard focus of the track view is on: a track itself, an item (clip/label)
//! of a track or the vertical ruler of a track
struct TrackFocus
{
    struct TrackTarget
    {
        bool operator==(const TrackTarget&) const = default;
    };

    struct RulerTarget
    {
        bool operator==(const RulerTarget&) const = default;
    };

    using Target = std::variant<TrackTarget, TrackItemId, RulerTarget>;

    TrackId trackId = INVALID_TRACK;
    Target target = TrackTarget {};

    static TrackFocus track(const TrackId& trackId) { return { trackId, TrackTarget {} }; }
    static TrackFocus item(const TrackItemKey& key) { return { key.trackId, key.itemId }; }
    static TrackFocus ruler(const TrackId& trackId) { return { trackId, RulerTarget {} }; }

    bool isTrack() const { return std::holds_alternative<TrackTarget>(target); }
    bool isItem() const { return std::holds_alternative<TrackItemId>(target); }
    bool isRuler() const { return std::holds_alternative<RulerTarget>(target); }

    std::optional<TrackItemKey> itemKey() const
    {
        const TrackItemId* itemId = std::get_if<TrackItemId>(&target);
        return itemId ? std::optional<TrackItemKey>(TrackItemKey { trackId, *itemId }) : std::nullopt;
    }

    bool operator==(const TrackFocus&) const = default;
};

struct TimeSignature
{
    double tempo = 0;

    int upper = 0;
    int lower = 0;
};

enum class UndoPushType : unsigned char {
    NONE = 0,
    CONSOLIDATE = 1 << 0,
    NOAUTOSAVE = 1 << 1
};

enum class TrackMoveDirection {
    Up,
    Down,
    Top,
    Bottom
};

enum class DeleteBehavior {
    NotSet = -1,
    CloseGap,
    LeaveGap,
};

enum class CloseGapBehavior {
    ClipRipple,
    TrackRipple,
    AllTracksRipple,
};

enum class PasteBehavior {
    PasteOverlap,
    PasteInsert,
};

enum class PasteInsertBehavior {
    PasteInsert,
    PasteInsertRipple,
};

enum class HistoryEvent {
    RestoredState,
    NewState,
};

enum class SelectionMode {
    Replace,
    Toggle,
    Range,
};

// 1-based index into theme clip_color_N / clip_selected_color_N slots.
// 0 means "no custom color" (clip inherits from track).
using ClipColorIndex = int;
static constexpr ClipColorIndex CLIP_COLOR_INDEX_NONE = 0;
static constexpr int CLIP_COLOR_COUNT = 9;
}

inline muse::logger::Stream& operator<<(muse::logger::Stream& s, const au::trackedit::TrackItemKey& k)
{
    s << "{trackId: " << k.trackId << ", itemId: " << k.itemId << "}";
    return s;
}

inline muse::logger::Stream& operator<<(muse::logger::Stream& s, au::trackedit::UndoPushType t)
{
    switch (t) {
    case au::trackedit::UndoPushType::NONE: s << "UndoPushType::NONE";
        break;
    case au::trackedit::UndoPushType::CONSOLIDATE: s << "UndoPushType::CONSOLIDATE";
        break;
    case au::trackedit::UndoPushType::NOAUTOSAVE: s << "UndoPushType::NOAUTOSAVE";
        break;
    }
    return s;
}
