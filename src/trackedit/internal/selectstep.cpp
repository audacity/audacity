/*
* Audacity: A Digital Audio Editor
*/
#include "selectstep.h"

#include <algorithm>
#include <cmath>
#include <string>

using namespace au::trackedit;
using namespace muse;
using namespace muse::rcommand;

namespace {
template<class ... Ts>
struct Overloaded : Ts ... {
    using Ts::operator() ...;
};

struct TimeArgs {
    SelectTimeRelativeTo relativeTo = SelectTimeRelativeTo::Project;
    double start = 0.0;
    double end = 0.0;
};

struct TrackArgs {
    int index = 0;
    int count = 0;
    SelectTracksMode mode = SelectTracksMode::Set;
};

bool isEndAnchor(SelectAnchor anchor)
{
    return anchor == SelectAnchor::ProjectEnd || anchor == SelectAnchor::SelectionEnd;
}

std::string describe(SelectAnchor anchor)
{
    const auto& table = SelectAnchorContract::table;
    const auto it = std::ranges::find(table, anchor, [](const auto& entry) { return entry.eval; });
    return it != table.end() ? std::string(it->description) : std::string();
}

Ret badArgs(const std::string& text)
{
    return make_ret(Ret::Code::BadArgs, text);
}

template<typename E>
RetVal<E> enumParam(const Params& params, std::string_view key)
{
    const std::string value = params.at(key).toString();
    if (const std::optional<E> parsed = EnumContract<E>::enum_val(value)) {
        return RetVal<E>::make_ok(*parsed);
    }

    return RetVal<E>::make_ret(badArgs("Unknown value of '" + std::string(key) + "': " + value));
}

RetVal<SelectTimeSpec> timeSpecFromParams(const Params& params)
{
    const RetVal<SelectTimeKind> kind = enumParam<SelectTimeKind>(params, SelectTimeKindContract::key);
    if (!kind.ret) {
        return RetVal<SelectTimeSpec>::make_ret(kind.ret);
    }

    if (kind.val == SelectTimeKind::WholeProject) {
        return RetVal<SelectTimeSpec>::make_ok(WholeProjectTime {});
    }

    const RetVal<SelectTimeRelativeTo> relativeTo = enumParam<SelectTimeRelativeTo>(params, SelectTimeRelativeToContract::key);
    if (!relativeTo.ret) {
        return RetVal<SelectTimeSpec>::make_ret(relativeTo.ret);
    }

    return RetVal<SelectTimeSpec>::make_ok(SelectTimeRange {
            relativeTo.val,
            params.at(TRACKEDIT_START_PARAM).toDouble(),
            params.at(TRACKEDIT_END_PARAM).toDouble(),
        });
}

RetVal<SelectTrackSpec> trackSpecFromParams(const Params& params)
{
    const RetVal<SelectTracksKind> kind = enumParam<SelectTracksKind>(params, SelectTracksKindContract::key);
    if (!kind.ret) {
        return RetVal<SelectTrackSpec>::make_ret(kind.ret);
    }

    if (kind.val == SelectTracksKind::All) {
        return RetVal<SelectTrackSpec>::make_ok(AllTracks {});
    }

    const RetVal<SelectTracksMode> mode = enumParam<SelectTracksMode>(params, SelectTracksModeContract::key);
    if (!mode.ret) {
        return RetVal<SelectTrackSpec>::make_ret(mode.ret);
    }

    const int firstTrack = params.at(SELECT_STEP_FIRST_TRACK_PARAM, Val(1)).toInt();
    const int trackCount = params.at(SELECT_STEP_TRACK_COUNT_PARAM, Val(1)).toInt();
    if (firstTrack < 1 || trackCount < 1) {
        return RetVal<SelectTrackSpec>::make_ret(badArgs("The first track and the track count must be 1 or more"));
    }

    return RetVal<SelectTrackSpec>::make_ok(SelectTrackRange { firstTrack, trackCount, mode.val });
}

TimeArgs timeArgs(const SelectTimeSpec& spec)
{
    return std::visit(Overloaded {
            [](const WholeProjectTime&) { return TimeArgs { SelectTimeRelativeTo::Project, 0.0, 0.0 }; },
            [](const SelectTimeRange& range) {
            const double sign = isEndAnchor(startAnchorOf(range.relativeTo)) ? -1.0 : 1.0;
            return TimeArgs { range.relativeTo, sign * range.startOffset, sign * range.endOffset };
        },
        }, spec);
}

TrackArgs trackArgs(const SelectTrackSpec& spec, int projectTrackCount)
{
    return std::visit(Overloaded {
            [projectTrackCount](const AllTracks&) { return TrackArgs { 0, projectTrackCount, SelectTracksMode::Set }; },
            [projectTrackCount](const SelectTrackRange& range) {
            const int index = std::clamp(range.firstTrack - 1, 0, std::max(projectTrackCount - 1, 0));
            return TrackArgs { index, range.trackCount, range.mode };
        },
        }, spec);
}

std::string edgeText(SelectAnchor anchor, double offset)
{
    const std::string anchorText = describe(anchor);
    if (offset == 0.0) {
        return anchorText;
    }

    const std::string sign = offset > 0.0 ? " + " : " - ";
    return anchorText + sign + String::number(std::abs(offset), 3).toStdString() + " s";
}

std::string timeText(const SelectTimeSpec& spec)
{
    return std::visit(Overloaded {
            [](const WholeProjectTime&) { return std::string("Whole project"); },
            [](const SelectTimeRange& range) {
            return edgeText(startAnchorOf(range.relativeTo), range.startOffset) + " to "
                   + edgeText(endAnchorOf(range.relativeTo), range.endOffset);
        },
        }, spec);
}

std::string trackText(const SelectTrackSpec& spec)
{
    return std::visit(Overloaded {
            [](const AllTracks&) { return std::string("all tracks"); },
            [](const SelectTrackRange& range) {
            const std::string first = std::to_string(range.firstTrack);
            const std::string tracks = range.trackCount == 1
                                       ? "track " + first
                                       : "tracks " + first + "-" + std::to_string(range.firstTrack + range.trackCount - 1);
            switch (range.mode) {
                case SelectTracksMode::Add: return "add " + tracks;
                case SelectTracksMode::Remove: return "remove " + tracks;
                case SelectTracksMode::Set: break;
            }
            return tracks;
        },
        }, spec);
}
}

SelectAnchor au::trackedit::startAnchorOf(SelectTimeRelativeTo relativeTo)
{
    switch (relativeTo) {
    case SelectTimeRelativeTo::ProjectStart:
    case SelectTimeRelativeTo::Project: return SelectAnchor::ProjectStart;
    case SelectTimeRelativeTo::ProjectEnd: return SelectAnchor::ProjectEnd;
    case SelectTimeRelativeTo::SelectionStart:
    case SelectTimeRelativeTo::Selection: return SelectAnchor::SelectionStart;
    case SelectTimeRelativeTo::SelectionEnd: return SelectAnchor::SelectionEnd;
    }
    return SelectAnchor::ProjectStart;
}

SelectAnchor au::trackedit::endAnchorOf(SelectTimeRelativeTo relativeTo)
{
    switch (relativeTo) {
    case SelectTimeRelativeTo::ProjectStart: return SelectAnchor::ProjectStart;
    case SelectTimeRelativeTo::Project:
    case SelectTimeRelativeTo::ProjectEnd: return SelectAnchor::ProjectEnd;
    case SelectTimeRelativeTo::SelectionStart: return SelectAnchor::SelectionStart;
    case SelectTimeRelativeTo::Selection:
    case SelectTimeRelativeTo::SelectionEnd: return SelectAnchor::SelectionEnd;
    }
    return SelectAnchor::ProjectStart;
}

std::optional<SelectTimeRelativeTo> au::trackedit::relativeToOf(SelectAnchor start, SelectAnchor end)
{
    const auto& table = SelectTimeRelativeToContract::table;
    const auto it = std::ranges::find_if(table, [start, end](const auto& entry) {
        return startAnchorOf(entry.eval) == start && endAnchorOf(entry.eval) == end;
    });
    return it != table.end() ? std::optional(it->eval) : std::nullopt;
}

SelectTimeKind au::trackedit::kindOf(const SelectTimeSpec& spec)
{
    return std::visit(Overloaded {
        [](const WholeProjectTime&) { return SelectTimeKind::WholeProject; },
        [](const SelectTimeRange&) { return SelectTimeKind::Range; },
    }, spec);
}

SelectTracksKind au::trackedit::kindOf(const SelectTrackSpec& spec)
{
    return std::visit(Overloaded {
        [](const AllTracks&) { return SelectTracksKind::All; },
        [](const SelectTrackRange&) { return SelectTracksKind::Range; },
    }, spec);
}

Val au::trackedit::toVal(const SelectEnhancedStep& step)
{
    Params params;
    EnumContract<SelectTimeKind>::set(params, kindOf(step.time));
    EnumContract<SelectTracksKind>::set(params, kindOf(step.tracks));

    if (const auto* range = std::get_if<SelectTimeRange>(&step.time)) {
        EnumContract<SelectTimeRelativeTo>::set(params, range->relativeTo);
        params.set(TRACKEDIT_START_PARAM, Val(range->startOffset));
        params.set(TRACKEDIT_END_PARAM, Val(range->endOffset));
    }

    if (const auto* range = std::get_if<SelectTrackRange>(&step.tracks)) {
        params.set(SELECT_STEP_FIRST_TRACK_PARAM, Val(range->firstTrack));
        params.set(SELECT_STEP_TRACK_COUNT_PARAM, Val(range->trackCount));
        EnumContract<SelectTracksMode>::set(params, range->mode);
    }

    return Val(ValMap(params));
}

RetVal<SelectEnhancedStep> au::trackedit::selectStepFromVal(const Val& val)
{
    const Params params(val.toMap());

    const RetVal<SelectTimeSpec> time = timeSpecFromParams(params);
    if (!time.ret) {
        return RetVal<SelectEnhancedStep>::make_ret(time.ret);
    }

    const RetVal<SelectTrackSpec> tracks = trackSpecFromParams(params);
    if (!tracks.ret) {
        return RetVal<SelectEnhancedStep>::make_ret(tracks.ret);
    }

    return RetVal<SelectEnhancedStep>::make_ok(SelectEnhancedStep { time.val, tracks.val });
}

SelectCommand au::trackedit::selectStepCommand(const SelectEnhancedStep& step, int projectTrackCount)
{
    const TimeArgs time = timeArgs(step.time);
    const TrackArgs tracks = trackArgs(step.tracks, projectTrackCount);

    SelectCommand command;
    command.start = time.start;
    command.end = time.end;
    command.relativeTo = time.relativeTo;
    command.trackIndex = tracks.index;
    command.trackCount = tracks.count;
    command.mode = tracks.mode;
    return command;
}

String au::trackedit::selectStepSummary(const SelectEnhancedStep& step)
{
    return String::fromStdString(timeText(step.time) + ", " + trackText(step.tracks));
}
