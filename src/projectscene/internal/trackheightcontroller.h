/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/commandable.h"
#include "framework/rcommand/icommanddispatcher.h"

#include "context/iglobalcontext.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/internal/itracknavigationcontroller.h"

#include "../iprojectviewstate.h"

namespace au::projectscene {
//! Collapses, expands, fits, resets and resizes tracks: the focused and selected ones, or all of them
class TrackHeightController : public muse::rcommand::Commandable, public muse::async::Asyncable, public muse::Contextable
{
public:
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher { this };
    muse::ContextInject<context::IGlobalContext> globalContext { this };
    muse::ContextInject<trackedit::ISelectionController> selectionController { this };
    muse::ContextInject<trackedit::ITrackNavigationController> trackNavigationController { this };

    TrackHeightController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    //! The selected tracks if the given track, or by default the focused one, is among them; otherwise that track
    trackedit::TrackIdList targetTracks(std::optional<trackedit::TrackId> track = std::nullopt) const;
    trackedit::TrackIdList allTracks() const;

    void changeHeight(const trackedit::TrackIdList& tracks, int delta);
    void collapse(const trackedit::TrackIdList& tracks);
    void expand(const trackedit::TrackIdList& tracks);
    void reset(const trackedit::TrackIdList& tracks);
    //! Shares the height of the view between the tracks and collapses the others
    void fitToHeight(const trackedit::TrackIdList& tracks);

private:
    IProjectViewStatePtr viewState() const;
};
}
