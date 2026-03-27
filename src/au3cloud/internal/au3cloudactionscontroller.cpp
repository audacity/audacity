/*
* Audacity: A Digital Audio Editor
*/
#include "au3cloudactionscontroller.h"

using namespace au::au3cloud;

namespace {
const char* TOUR_PAGE_URL { "https://audio.com/tour?mtm_campaign=audacitydesktop&mtm_content=app_launch_reg" };

const muse::actions::ActionQuery OPEN_SIGNIN_DIALOG_ACTION("audacity://cloud/open-signin-dialog");
}

Au3CloudActionsController::Au3CloudActionsController(muse::modularity::ContextPtr ctx)
    : muse::Contextable(ctx)
{
}

void Au3CloudActionsController::init()
{
    dispatcher()->reg(this, OPEN_SIGNIN_DIALOG_ACTION, this, &Au3CloudActionsController::openSignInDialog);
}

bool Au3CloudActionsController::canReceiveAction(const muse::actions::ActionCode&) const
{
    return true;
}

void Au3CloudActionsController::openSignInDialog(const muse::actions::ActionQuery& query)
{
    const auto ret = authorization()->ensureAuthorization();
    if (!ret) {
        return;
    }

    const bool showTourPage = query.param("showTourPage").toBool();

    if (showTourPage) {
        platformInteractive()->openUrl(TOUR_PAGE_URL);
    }
}
