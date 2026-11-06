/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramactionscontroller.h"
#include "spectrogramtypes.h"

#include "trackedit/dom/track.h"

#include "../spectrogramcommands.h"

namespace au::spectrogram {
void SpectrogramActionsController::init()
{
    commandDispatcher()->onRequest<TrackSpectrogramSettingsCommand>(this, [this](const TrackSpectrogramSettingsCommand& command) {
        return openTrackSpectrogramSettings(command.trackId);
    });
}

muse::Ret SpectrogramActionsController::openTrackSpectrogramSettings(int trackId)
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    const auto track = project->trackeditProject()->track(trackId);
    if (!track) {
        return muse::make_ret(muse::Ret::Code::BadArgs);
    }

    muse::UriQuery uriQuery{ TRACK_SPECTROGRAM_SETTINGS_URI };
    uriQuery.addParam("trackId", muse::Val(trackId));
    uriQuery.addParam("trackTitle", muse::Val(track->title.toStdString()));
    interactive()->open(uriQuery);

    return muse::make_ok();
}
}
