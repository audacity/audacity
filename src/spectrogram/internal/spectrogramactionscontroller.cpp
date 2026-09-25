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
    commandDispatcher()->onRequest(this, TRACK_SPECTROGRAM_SETTINGS_COMMAND, [this](const muse::rcommand::Params& params) {
        return openTrackSpectrogramSettings(params);
    });
}

muse::Ret SpectrogramActionsController::openTrackSpectrogramSettings(const muse::rcommand::Params& params)
{
    if (!params.contains("trackId")) {
        return muse::make_ret(muse::Ret::Code::BadArgs);
    }

    const auto project = globalContext()->currentProject();
    if (!project) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    const int trackId = params.at("trackId").toInt();
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
