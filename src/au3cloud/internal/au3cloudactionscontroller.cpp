/*
* Audacity: A Digital Audio Editor
*/
#include "au3cloudactionscontroller.h"

using namespace au::au3cloud;

namespace {
const muse::Uri SIGNIN_URI("audacity://signin/audiocom");
const char* TOUR_PAGE_URL { "https://audio.com/tour?mtm_campaign=audacitydesktop&mtm_content=app_launch_reg" };

const muse::actions::ActionQuery OPEN_SIGNIN_DIALOG_ACTION("audacity://cloud/open-signin-dialog");
const muse::actions::ActionQuery OPEN_CLOUD_PROJECT_PAGE_ACTION("audacity://cloud/open-project-page");
const muse::actions::ActionQuery OPEN_CLOUD_AUDIO_PAGE_ACTION("audacity://cloud/open-audio-page");
}

Au3CloudActionsController::Au3CloudActionsController(muse::modularity::ContextPtr ctx)
    : muse::Contextable(ctx)
{
}

void Au3CloudActionsController::init()
{
    dispatcher()->reg(this, OPEN_SIGNIN_DIALOG_ACTION, this, &Au3CloudActionsController::openSignInDialog);
    dispatcher()->reg(this, OPEN_CLOUD_PROJECT_PAGE_ACTION, this, &Au3CloudActionsController::openCloudProjectPage);
    dispatcher()->reg(this, OPEN_CLOUD_AUDIO_PAGE_ACTION, this, &Au3CloudActionsController::openCloudAudioPage);
}

bool Au3CloudActionsController::canReceiveAction(const muse::actions::ActionCode&) const
{
    return true;
}

void Au3CloudActionsController::openSignInDialog(const muse::actions::ActionQuery& query)
{
    if (authorization()->isAuthorized()) {
        return;
    }

    const bool showTourPage = query.param("showTourPage").toBool();
    const bool sync = query.param("sync").toBool();

    if (sync) {
        muse::RetVal<muse::Val> rv = interactive()->openSync(SIGNIN_URI);
        if (!rv.ret) {
            return;
        }
    } else {
        interactive()->open(SIGNIN_URI);
    }

    if (showTourPage) {
        platformInteractive()->openUrl(TOUR_PAGE_URL);
    }
}

void Au3CloudActionsController::openCloudProjectPage(const muse::actions::ActionQuery& query)
{
    const auto slug = query.param("slug").toString();

    const auto url = audioComService()->getCloudProjectPage(slug);
    if (!url.empty()) {
        platformInteractive()->openUrl(url);
    }
}

void Au3CloudActionsController::openCloudAudioPage(const muse::actions::ActionQuery& query)
{
    const auto slug = query.param("slug").toString();

    const auto url = audioComService()->getCloudAudioPage(slug);
    if (!url.empty()) {
        platformInteractive()->openUrl(url);
    }
}
