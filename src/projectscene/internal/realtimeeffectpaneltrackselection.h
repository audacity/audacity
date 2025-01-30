/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "irealtimeeffectpaneltrackselection.h"
#include "trackedit/iselectioncontroller.h"
#include "context/iglobalcontext.h"
#include "modularity/ioc.h"
#include "async/asyncable.h"

namespace au::projectscene {
class RealtimeEffectPanelTrackSelection : public IRealtimeEffectPanelTrackSelection, muse::async::Asyncable
{
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<context::IGlobalContext> globalContext;

public:
    void init();

    std::optional<au::trackedit::TrackId> selectedTrackId() const override;
    muse::async::Notification selectedTrackIdChanged() const override;

private:
    void setTrackId(std::optional<au::trackedit::TrackId>);
    void setupCallbacks(trackedit::ITrackeditProject&);
    void onTrackAdded(const trackedit::Track&);
    void onTrackRemoved(const trackedit::TrackId&);
    void onTracksChanged(const std::vector<trackedit::Track>&);
    std::optional<au::trackedit::TrackId> m_trackId;
    muse::async::Notification m_selectedTrackIdChanged;
};
}
