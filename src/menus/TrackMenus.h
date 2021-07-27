#pragma once

#include "commands/CommandFunctors.h"

namespace TrackActions {

// Menu handler functions
struct Handler : CommandHandlerObject {

    void OnNewWaveTrack(const CommandContext &context);
    void OnNewStereoTrack(const CommandContext &context);
    void OnNewLabelTrack(const CommandContext &context);
    void OnNewTimeTrack(const CommandContext &context);
    void OnStereoToMono(const CommandContext &context);
    void OnMixAndRender(const CommandContext &context);
    void OnMixAndRenderToNewTrack(const CommandContext &context);
    void OnResample(const CommandContext &context);
    void OnRemoveTracks(const CommandContext &context);
    static void MuteTracks(const CommandContext &context, bool mute, bool selected);
    void OnMuteAllTracks(const CommandContext &context);
    void OnUnmuteAllTracks(const CommandContext &context);
    void OnMuteSelectedTracks(const CommandContext &context);
    void OnUnmuteSelectedTracks(const CommandContext &context);
    void OnPanLeft(const CommandContext &context);
    void OnPanRight(const CommandContext &context);
    void OnPanCenter(const CommandContext &context);
    void OnAlignNoSync(const CommandContext &context);
    void OnAlign(const CommandContext &context);
    void OnMoveSelectionWithTracks(const CommandContext &WXUNUSED(context) );
#ifdef EXPERIMENTAL_SCOREALIGN
    void OnScoreAlign(const CommandContext &context);
#endif /* EXPERIMENTAL_SCOREALIGN */
    void OnSortTime(const CommandContext &context);
    void OnSortName(const CommandContext &context);
    void OnSyncLock(const CommandContext &context);
    void OnTrackPan(const CommandContext &context);
    void OnTrackPanLeft(const CommandContext &context);
    void OnTrackPanRight(const CommandContext &context);
    void OnTrackGain(const CommandContext &context);
    void OnTrackGainInc(const CommandContext &context);
    void OnTrackGainDec(const CommandContext &context);
    void OnTrackMenu(const CommandContext &context);
    void OnTrackMute(const CommandContext &context);
    void OnTrackSolo(const CommandContext &context);
    void OnTrackClose(const CommandContext &context);
    void OnTrackMoveUp(const CommandContext &context);
    void OnTrackMoveDown(const CommandContext &context);
    void OnTrackMoveTop(const CommandContext &context);
    void OnTrackMoveBottom(const CommandContext &context);

};
}
