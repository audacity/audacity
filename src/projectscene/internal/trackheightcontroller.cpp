/*
* Audacity: A Digital Audio Editor
*/
#include "trackheightcontroller.h"

#include <algorithm>

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "project/iaudacityproject.h"

#include "../projectscenecommands.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::rcommand;

namespace {
constexpr int TRACK_HEIGHT_STEP = 24;

std::optional<au::trackedit::TrackId> trackOf(const Params& params)
{
    if (!params.contains(PROJECTSCENE_TRACK_HEIGHT_TRACK_ID_PARAM)) {
        return std::nullopt;
    }
    return params.at(PROJECTSCENE_TRACK_HEIGHT_TRACK_ID_PARAM).toInt64();
}
}

void TrackHeightController::init()
{
    auto cd = commandDispatcher();
    cd->onRequest(this, PROJECTSCENE_INCREASE_TRACK_HEIGHT_COMMAND, [this](const Params& params) {
        changeHeight(targetTracks(trackOf(params)), TRACK_HEIGHT_STEP);
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_DECREASE_TRACK_HEIGHT_COMMAND, [this](const Params& params) {
        changeHeight(targetTracks(trackOf(params)), -TRACK_HEIGHT_STEP);
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_INCREASE_ALL_TRACK_HEIGHTS_COMMAND, [this]() {
        changeHeight(allTracks(), TRACK_HEIGHT_STEP);
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_DECREASE_ALL_TRACK_HEIGHTS_COMMAND, [this]() {
        changeHeight(allTracks(), -TRACK_HEIGHT_STEP);
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_COLLAPSE_TRACKS_COMMAND, [this](const Params& params) {
        collapse(targetTracks(trackOf(params)));
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_EXPAND_TRACKS_COMMAND, [this](const Params& params) {
        expand(targetTracks(trackOf(params)));
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_COLLAPSE_ALL_TRACKS_COMMAND, [this]() {
        collapse(allTracks());
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_EXPAND_ALL_TRACKS_COMMAND, [this]() {
        expand(allTracks());
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_FIT_TRACKS_TO_HEIGHT_COMMAND, [this](const Params& params) {
        fitToHeight(targetTracks(trackOf(params)));
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_FIT_ALL_TRACKS_TO_HEIGHT_COMMAND, [this]() {
        fitToHeight(allTracks());
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_RESET_TRACK_HEIGHT_COMMAND, [this](const Params& params) {
        reset(targetTracks(trackOf(params)));
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_RESET_ALL_TRACK_HEIGHTS_COMMAND, [this]() {
        reset(allTracks());
        return make_ok();
    });
}

au::trackedit::TrackIdList TrackHeightController::targetTracks(std::optional<trackedit::TrackId> track) const
{
    if (track && !muse::contains(allTracks(), *track)) {
        LOGW() << "track " << *track << " is not in the project, using the focused track";
        track.reset();
    }

    const trackedit::TrackIdList selected = selectionController()->selectedTracks();
    const trackedit::TrackId anchor = track.value_or(trackNavigationController()->focusedTrack());
    if (anchor == trackedit::INVALID_TRACK) {
        return selected;
    }

    return muse::contains(selected, anchor) ? selected : trackedit::TrackIdList { anchor };
}

au::trackedit::TrackIdList TrackHeightController::allTracks() const
{
    const auto project = globalContext()->currentTrackeditProject();
    return project ? project->trackIdList() : trackedit::TrackIdList {};
}

void TrackHeightController::changeHeight(const trackedit::TrackIdList& tracks, int delta)
{
    const IProjectViewStatePtr state = viewState();
    if (!state) {
        return;
    }

    for (const trackedit::TrackId trackId : tracks) {
        state->changeTrackHeight(trackId, delta);
    }
}

void TrackHeightController::collapse(const trackedit::TrackIdList& tracks)
{
    const IProjectViewStatePtr state = viewState();
    if (!state) {
        return;
    }

    for (const trackedit::TrackId trackId : tracks) {
        state->collapseTrack(trackId);
    }
}

void TrackHeightController::expand(const trackedit::TrackIdList& tracks)
{
    const IProjectViewStatePtr state = viewState();
    if (!state) {
        return;
    }

    for (const trackedit::TrackId trackId : tracks) {
        state->expandTrack(trackId);
    }
}

void TrackHeightController::reset(const trackedit::TrackIdList& tracks)
{
    const IProjectViewStatePtr state = viewState();
    if (!state) {
        return;
    }

    for (const trackedit::TrackId trackId : tracks) {
        state->setTrackHeight(trackId, state->trackDefaultHeight(trackId));
    }
}

void TrackHeightController::fitToHeight(const trackedit::TrackIdList& tracks)
{
    const IProjectViewStatePtr state = viewState();
    const int viewportHeight = state ? state->tracksViewportHeight() : 0;
    if (tracks.empty() || viewportHeight <= 0) {
        return;
    }

    int othersHeight = 0;
    for (const trackedit::TrackId trackId : allTracks()) {
        if (!muse::contains(tracks, trackId)) {
            state->collapseTrack(trackId);
            othersHeight += state->trackHeight(trackId).val;
        }
    }

    // The pixels left over by the division go to the first tracks, so that the tracks fill the view exactly
    const int count = static_cast<int>(tracks.size());
    const int availableHeight = std::max(viewportHeight - othersHeight, 0);
    const int height = availableHeight / count;
    const int remainder = availableHeight % count;
    for (int i = 0; i < count; ++i) {
        state->setTrackHeight(tracks.at(i), std::max(height + (i < remainder ? 1 : 0), state->trackMinHeight()));
    }

    if (availableHeight >= count * state->trackMinHeight()) {
        state->changeTracksVerticalOffset(0);
        return;
    }

    for (const trackedit::TrackId trackId : allTracks()) {
        if (muse::contains(tracks, trackId)) {
            state->changeTracksVerticalOffset(state->tracksVerticalOffset().val + state->trackVerticalPosition(trackId));
            break;
        }
    }
}

IProjectViewStatePtr TrackHeightController::viewState() const
{
    const project::IAudacityProjectPtr project = globalContext()->currentProject();
    return project ? project->viewState() : nullptr;
}
