/*
 * Audacity: A Digital Audio Editor
 */
#include "cloudurlhandler.h"

#include <QString>
#include <QUrl>
#include <QUrlQuery>

#include "framework/actions/actiontypes.h"
#include "framework/global/log.h"

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

    const QString snapshotId = query.queryItemValue(QStringLiteral("snapshotId"));

    using namespace muse::actions;
    ActionData data;
    data.setArg<QString>(0, projectId);
    data.setArg<QString>(1, snapshotId);
    dispatcher()->dispatch("cloud-file-open", data);

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
