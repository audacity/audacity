/*
* Audacity: A Digital Audio Editor
*/
#include "au3cloudactionscontroller.h"

#include <variant>

#include "../cloudcommands.h"
#include "cloudurlhandler.h"

using namespace au::au3cloud;
using namespace muse::rcommand;

Au3CloudActionsController::Au3CloudActionsController(muse::modularity::ContextPtr ctx)
    : muse::Contextable(ctx)
{
}

Au3CloudActionsController::~Au3CloudActionsController() = default;

void Au3CloudActionsController::init()
{
    m_urlHandler = std::make_unique<CloudUrlHandler>(iocContext());

    auto cd = commandDispatcher();
    cd->onRequest(this, CLOUD_SHOW_TOUR_PAGE_COMMAND, [this]() { return showTourPage(); });
    cd->onRequest(this, CLOUD_OPEN_PROJECT_PAGE_COMMAND, [this](const Params& params) { return openCloudProjectPage(params); });
    cd->onRequest(this, CLOUD_OPEN_AUDIO_PAGE_COMMAND, [this](const Params& params) { return openCloudAudioPage(params); });
    cd->onRequest(this, CLOUD_OPEN_PROFILE_PAGE_COMMAND, [this]() { return openCloudProfilePage(); });
    cd->onRequest(this, CLOUD_OPEN_URL_COMMAND, [this](const Params& params) { return openUrl(params); });

    authorization()->authState().ch.onReceive(this, [this](const AuthState& state) {
        if (std::holds_alternative<Authorizing>(state)) {
            return;
        }

        for (const std::string& url : std::exchange(m_pendingUrls, {})) {
            m_urlHandler->handle(QString::fromStdString(url));
        }
    });
}

muse::Ret Au3CloudActionsController::openUrl(const Params& params)
{
    const QString url = params.at(CLOUD_OPEN_URL_URL_PARAM).toQString();
    if (url.isEmpty()) {
        return muse::make_ret(muse::Ret::Code::BadArgs);
    }

    if (std::holds_alternative<Authorizing>(authorization()->authState().val)) {
        m_pendingUrls.push_back(url.toStdString());
        return muse::make_ok();
    }

    m_urlHandler->handle(url);
    return muse::make_ok();
}

muse::Ret Au3CloudActionsController::showTourPage()
{
    const muse::Ret ret = authorization()->ensureAuthorized(iocContext(), true);
    if (!ret) {
        LOGW() << "Sign in cancelled: " << ret.toString();
        return ret;
    }

    platformInteractive()->openUrl(audioComService()->getTourPage());
    return muse::make_ok();
}

muse::Ret Au3CloudActionsController::openCloudProjectPage(const Params& params)
{
    const auto id = params.at(CLOUD_OPEN_PROJECT_PAGE_ID_PARAM).toString();
    if (id.empty()) {
        LOGE() << "Cannot open cloud project page: empty id";
        return muse::make_ret(muse::Ret::Code::BadArgs);
    }

    const std::string url = audioComService()->getCloudProjectPage(id);
    if (url.empty()) {
        LOGE() << "Cannot open cloud project page: empty URL";
        return muse::make_ret(muse::Ret::Code::BadArgs);
    }

    platformInteractive()->openUrl(url);
    return muse::make_ok();
}

muse::Ret Au3CloudActionsController::openCloudAudioPage(const Params& params)
{
    const auto slug = params.at(CLOUD_OPEN_AUDIO_PAGE_SLUG_PARAM).toString();
    if (slug.empty()) {
        LOGE() << "Cannot open cloud audio page: empty slug";
        return muse::make_ret(muse::Ret::Code::BadArgs);
    }

    const auto url = audioComService()->getCloudAudioPage(slug);
    if (url.empty()) {
        LOGE() << "Cannot open cloud audio page: empty URL";
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    platformInteractive()->openUrl(url);
    return muse::make_ok();
}

muse::Ret Au3CloudActionsController::openCloudProfilePage()
{
    if (!authorization()->isAuthorized()) {
        LOGE() << "Cannot open cloud profile page: not signed in";
        return muse::make_ret(muse::Ret::Code::NotSupported);
    }

    const auto url = audioComService()->getCloudProfilePage();
    if (url.empty()) {
        LOGE() << "Cannot open cloud profile page: empty URL";
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    platformInteractive()->openUrl(url);
    return muse::make_ok();
}
