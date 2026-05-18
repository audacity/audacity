/*
 * Audacity: A Digital Audio Editor
 */
#include "cloudurlhandler.h"

#include <QString>
#include <QUrl>
#include <QUrlQuery>

#include "framework/actions/actiontypes.h"
#include "framework/global/log.h"

#include "au3-cloud-audiocom/sync/CloudProjectsDatabase.h"

using namespace au::au3cloud;

CloudUrlHandler::CloudUrlHandler(muse::modularity::ContextPtr ctx)
    : muse::Contextable(ctx)
{
}

void CloudUrlHandler::handle(const QString& url)
{
    const QUrl parsed(url, QUrl::StrictMode);
    if (!parsed.isValid() || parsed.scheme().compare(QStringLiteral("audacity"), Qt::CaseInsensitive) != 0) {
        return;
    }

    if (tryHandleProjectLink(parsed)) {
        return;
    }

    if (tryHandleAudioLink(parsed)) {
        return;
    }

    LOGW() << "Unhandled audacity:// URL: " << url;
}

bool CloudUrlHandler::tryHandleProjectLink(const QUrl& parsed)
{
    if (parsed.host() != QStringLiteral("open")) {
        return false;
    }

    const QUrlQuery query(parsed);
    const QString projectId = query.queryItemValue(QStringLiteral("projectId"));
    if (projectId.isEmpty()) {
        return false;
    }

    //! When the project is in the local cloud DB we pass its path directly for
    //! an immediate open; otherwise an empty URL routes openCloudProject into
    //! the standard download/auth/sync flow.
    using namespace audacity::cloud::audiocom::sync;
    const auto local = CloudProjectsDatabase::Get().GetProjectData(projectId.toStdString());

    QUrl localUrl;
    if (local.has_value() && !local->LocalPath.empty()) {
        localUrl = QUrl::fromLocalFile(QString::fromStdString(local->LocalPath));
    }

    using namespace muse::actions;
    dispatcher()->dispatch(
        "cloud-file-open",
        ActionData::make_arg3<QString, QUrl, QString>(projectId, localUrl, QString()));

    return true;
}

bool CloudUrlHandler::tryHandleAudioLink(const QUrl& parsed)
{
    if (parsed.host() != QStringLiteral("open")) {
        return false;
    }

    const QUrlQuery query(parsed);
    const QString audioId = query.queryItemValue(QStringLiteral("audioId"));
    if (audioId.isEmpty()) {
        return false;
    }

    muse::actions::ActionQuery action("audacity://cloud/open-audio-file");
    action.addParam("audioId", muse::Val(audioId.toStdString()));

    dispatcher()->dispatch(action);

    return true;
}
