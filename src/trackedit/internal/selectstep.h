/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <variant>

#include "framework/global/types/retval.h"
#include "framework/global/types/string.h"
#include "framework/global/types/val.h"

#include "../trackeditcommands.h"

namespace au::trackedit {
enum class SelectAnchor {
    ProjectStart,
    ProjectEnd,
    SelectionStart,
    SelectionEnd
};

struct SelectAnchorContract {
    static constexpr std::string_view key = "anchor";
    static constexpr SelectAnchor def = SelectAnchor::ProjectStart;
    static constexpr muse::rcommand::EnumTable<SelectAnchor, 4> table = { {
        { "ProjectStart", SelectAnchor::ProjectStart, QT_TRANSLATE_NOOP("trackedit", "Project start") },
        { "ProjectEnd", SelectAnchor::ProjectEnd, QT_TRANSLATE_NOOP("trackedit", "Project end") },
        { "SelectionStart", SelectAnchor::SelectionStart, QT_TRANSLATE_NOOP("trackedit", "Selection start") },
        { "SelectionEnd", SelectAnchor::SelectionEnd, QT_TRANSLATE_NOOP("trackedit", "Selection end") },
    } };
};

enum class SelectTimeKind {
    WholeProject,
    Range
};

struct SelectTimeKindContract {
    static constexpr std::string_view key = "time";
    static constexpr SelectTimeKind def = SelectTimeKind::WholeProject;
    static constexpr muse::rcommand::EnumTable<SelectTimeKind, 2> table = { {
        { "WholeProject", SelectTimeKind::WholeProject, QT_TRANSLATE_NOOP("trackedit", "Whole project") },
        { "Range", SelectTimeKind::Range, QT_TRANSLATE_NOOP("trackedit", "Custom range") },
    } };
};

enum class SelectTracksKind {
    All,
    Range
};

struct SelectTracksKindContract {
    static constexpr std::string_view key = "tracks";
    static constexpr SelectTracksKind def = SelectTracksKind::All;
    static constexpr muse::rcommand::EnumTable<SelectTracksKind, 2> table = { {
        { "All", SelectTracksKind::All, QT_TRANSLATE_NOOP("trackedit", "All tracks") },
        { "Range", SelectTracksKind::Range, QT_TRANSLATE_NOOP("trackedit", "Track range") },
    } };
};
}

namespace muse::rcommand {
DECLARE_ENUM_CONTRACT(au::trackedit::SelectAnchorContract)
DECLARE_ENUM_CONTRACT(au::trackedit::SelectTimeKindContract)
DECLARE_ENUM_CONTRACT(au::trackedit::SelectTracksKindContract)
}

namespace au::trackedit {
inline static const std::string SELECT_STEP_FIRST_TRACK_PARAM("firstTrack");
inline static const std::string SELECT_STEP_TRACK_COUNT_PARAM("trackCount");

struct WholeProjectTime {};
struct SelectTimeRange {
    SelectTimeRelativeTo relativeTo = SelectTimeRelativeToContract::def;
    double startOffset = 0.0;
    double endOffset = 0.0;
};

struct AllTracks {};
struct SelectTrackRange {
    int firstTrack = 1;
    int trackCount = 1;
    SelectTracksMode mode = SelectTracksModeContract::def;
};

using SelectTimeSpec = std::variant<WholeProjectTime, SelectTimeRange>;
using SelectTrackSpec = std::variant<AllTracks, SelectTrackRange>;

struct SelectEnhancedStep {
    SelectTimeSpec time = WholeProjectTime {};
    SelectTrackSpec tracks = AllTracks {};
};

SelectAnchor startAnchorOf(SelectTimeRelativeTo relativeTo);
SelectAnchor endAnchorOf(SelectTimeRelativeTo relativeTo);
std::optional<SelectTimeRelativeTo> relativeToOf(SelectAnchor start, SelectAnchor end);

SelectTimeKind kindOf(const SelectTimeSpec& spec);
SelectTracksKind kindOf(const SelectTrackSpec& spec);

muse::Val toVal(const SelectEnhancedStep& step);
muse::RetVal<SelectEnhancedStep> selectStepFromVal(const muse::Val& val);

SelectCommand selectStepCommand(const SelectEnhancedStep& step, int projectTrackCount);
muse::String selectStepSummary(const SelectEnhancedStep& step);
}
