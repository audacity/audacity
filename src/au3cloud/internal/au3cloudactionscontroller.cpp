/*
* Audacity: A Digital Audio Editor
*/
#include "au3cloudactionscontroller.h"

#include <variant>

#include "framework/actions/actiontypes.h"
#include "framework/rcommand/actiontocommand.h"

#include "../cloudcommands.h"
#include "cloudurlhandler.h"

using namespace au::au3cloud;
using namespace muse::actions;
using namespace muse::rcommand;

namespace {
const ActionCode OPEN_URL_ACTION("open-url");
}

Au3CloudActionsController::Au3CloudActionsController(muse::modularity::ContextPtr ctx)
    : muse::Contextable(ctx)
{
}

Au3CloudActionsController::~Au3CloudActionsController() = default;

void Au3CloudActionsController::init()
{
    m_urlHandler = std::make_unique<CloudUrlHandler>(iocContext());

    auto cd = commandDispatcher();
    cd->onRequest<ShowTourPageCommand>(this, [this](const ShowTourPageCommand&) { return showTourPage(); });
    cd->onRequest<OpenProjectPageCommand>(this, [this](const OpenProjectPageCommand& command) {
        return openCloudProjectPage(command.projectId);
    });
    cd->onRequest<OpenAudioPageCommand>(this, [this](const OpenAudioPageCommand& command) {
        return openCloudAudioPage(command.slug);
    });
    cd->onRequest<OpenProfilePageCommand>(this, [this](const OpenProfilePageCommand&) { return openCloudProfilePage(); });
    cd->onRequest<OpenUrlCommand>(this, [this](const OpenUrlCommand& command) {
        return openUrl(QString::fromStdString(command.url));
    });

    static const std::vector<ActionToCommand> actionToCommand = {
        { OPEN_URL_ACTION, OpenUrlCommand::id, make_conv({ { "url", param<QString> } }) },
    };
    registerActionToCommand(this, actionToCommand, commandDispatcher(), dispatcher());

    authorization()->authState().ch.onReceive(this, [this](const AuthState& state) {
        if (std::holds_alternative<Authorizing>(state)) {
            return;
        }

        for (const std::string& url : std::exchange(m_pendingUrls, {})) {
            m_urlHandler->handle(QString::fromStdString(url));
        }
    });
}

muse::Ret Au3CloudActionsController::openUrl(const QString& url)
{
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

bool Au3CloudActionsController::canReceiveAction(const ActionCode&) const
{
    return true;
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

muse::Ret Au3CloudActionsController::openCloudProjectPage(const std::string& id)
{
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

muse::Ret Au3CloudActionsController::openCloudAudioPage(const std::string& slug)
{
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
