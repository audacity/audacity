/*
* Audacity: A Digital Audio Editor
*/
#include "au3cloudactionscontroller.h"

using namespace au::au3cloud;

namespace {
const muse::Uri SIGNIN_URI("audacity://signin/audiocom");
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
