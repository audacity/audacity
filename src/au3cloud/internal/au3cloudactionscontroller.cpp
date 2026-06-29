/*
* Audacity: A Digital Audio Editor
*/
#include "au3cloudactionscontroller.h"

#include "framework/actions/actiontypes.h"
#include "framework/global/types/uri.h"

#include "cloudurlhandler.h"

using namespace au::au3cloud;

namespace {
const muse::Uri SIGNIN_URI("audacity://signin/audiocom");
const char* TOUR_PAGE_URL { "https://audio.com/tour?mtm_campaign=audacitydesktop&mtm_content=app_launch_reg" };

const muse::actions::ActionQuery SHOW_TOUR_PAGE_ACTION("audacity://cloud/show-tour-page");
const muse::actions::ActionQuery OPEN_SIGNIN_DIALOG_ACTION("audacity://cloud/open-signin-dialog");
const muse::actions::ActionQuery OPEN_CREATE_ACCOUNT_DIALOG_ACTION("audacity://cloud/open-create-account-dialog");
const muse::actions::ActionQuery OPEN_CLOUD_PROJECT_PAGE_ACTION("audacity://cloud/open-project-page");
const muse::actions::ActionQuery OPEN_CLOUD_AUDIO_PAGE_ACTION("audacity://cloud/open-audio-page");

constexpr const char* createAccountModeParam = "isCreateAccountMode";
}

Au3CloudActionsController::Au3CloudActionsController(muse::modularity::ContextPtr ctx)
    : muse::Contextable(ctx)
{
}

Au3CloudActionsController::~Au3CloudActionsController() = default;

void Au3CloudActionsController::init()
{
    m_urlHandler = std::make_unique<CloudUrlHandler>(iocContext());

    dispatcher()->reg(this, SHOW_TOUR_PAGE_ACTION, this, &Au3CloudActionsController::showTourPage);
    dispatcher()->reg(this, OPEN_SIGNIN_DIALOG_ACTION, this, &Au3CloudActionsController::openSignInDialog);
    dispatcher()->reg(this, OPEN_CREATE_ACCOUNT_DIALOG_ACTION, this, &Au3CloudActionsController::openCreateAccountDialog);
    dispatcher()->reg(this, OPEN_CLOUD_PROJECT_PAGE_ACTION, this, &Au3CloudActionsController::openCloudProjectPage);
    dispatcher()->reg(this, OPEN_CLOUD_AUDIO_PAGE_ACTION, this, &Au3CloudActionsController::openCloudAudioPage);
    dispatcher()->reg(this, "open-url", this, &Au3CloudActionsController::openUrl);
}

void Au3CloudActionsController::openUrl(const muse::actions::ActionData& args)
{
    if (args.empty()) {
        return;
    }
    m_urlHandler->handle(args.arg<QString>(0));
}

bool Au3CloudActionsController::canReceiveAction(const muse::actions::ActionCode&) const
{
    return true;
}

void Au3CloudActionsController::openCreateAccountDialog(const muse::actions::ActionQuery& query)
{
    auto newQuery = query;
    newQuery.addParam(createAccountModeParam, muse::Val(true));

    openSignInDialog(newQuery);
}

void Au3CloudActionsController::showTourPage()
{
    if (!authorization()->isAuthorized()) {
        const muse::UriQuery uri(SIGNIN_URI);
        const muse::RetVal<muse::Val> rv = interactive()->openSync(uri);
        if (!rv.ret) {
            return;
        }
    }

    platformInteractive()->openUrl(TOUR_PAGE_URL);
}

void Au3CloudActionsController::openSignInDialog(const muse::actions::ActionQuery& query)
{
    if (authorization()->isAuthorized()) {
        return;
    }

    const bool sync = query.param("sync").toBool();
    const bool isCreateAccountMode = query.param(createAccountModeParam, muse::Val(false)).toBool();

    muse::UriQuery uri(SIGNIN_URI);
    uri.addParam(createAccountModeParam, muse::Val(isCreateAccountMode));

    if (sync) {
        const muse::RetVal<muse::Val> rv = interactive()->openSync(uri);
        if (!rv.ret) {
            return;
        }
    } else {
        interactive()->open(uri);
    }
}

void Au3CloudActionsController::openCloudProjectPage(const muse::actions::ActionQuery& query)
{
    std::string url {};
    if (!query.param("id").isNull()) {
        const auto id = query.param("id").toString();
        if (id.empty()) {
            LOGE() << "Cannot open cloud project page: empty id";
            return;
        }

        url = audioComService()->getCloudProjectPage(id);
    } else if (!query.param("path").isNull()) {
        const auto path = query.param("path").toPath();
        if (path.empty()) {
            LOGE() << "Cannot open cloud project page: empty path";
            return;
        }

        url = audioComService()->getCloudProjectPage(path);
    }

    if (url.empty()) {
        LOGE() << "Cannot open cloud project page: empty URL";
        return;
    }

    platformInteractive()->openUrl(url);
}

void Au3CloudActionsController::openCloudAudioPage(const muse::actions::ActionQuery& query)
{
    const auto slug = query.param("slug").toString();
    if (slug.empty()) {
        LOGE() << "Cannot open cloud audio page: empty slug";
        return;
    }

    const auto url = audioComService()->getCloudAudioPage(slug);
    if (url.empty()) {
        LOGE() << "Cannot open cloud audio page: empty URL";
        return;
    }

    platformInteractive()->openUrl(url);
}
