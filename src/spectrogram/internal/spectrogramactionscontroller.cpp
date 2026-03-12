/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramactionscontroller.h"
#include "spectrogramtypes.h"

namespace au::spectrogram {
void SpectrogramActionsController::init()
{
    dispatcher()->reg(this, TRACK_SPECTROGRAM_SETTINGS_ACTION, this, &SpectrogramActionsController::openTrackSpectrogramSettings);
}

void SpectrogramActionsController::openTrackSpectrogramSettings(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 2) {
        return;
    }

    const auto trackId = args.arg<int>(0);
    const auto trackTitle = args.arg<muse::String>(1);

    muse::UriQuery uriQuery{ TRACK_SPECTROGRAM_SETTINGS_ACTION };
    uriQuery.addParam("trackId", muse::Val(trackId));
    uriQuery.addParam("trackTitle", muse::Val(trackTitle.toStdString()));
    interactive()->open(uriQuery);
}
}
