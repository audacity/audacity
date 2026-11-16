/*
 * Audacity: A Digital Audio Editor
 */
#include "cloudurlhandler.h"

#include <QString>
#include <QUrl>
#include <QUrlQuery>

#include "framework/global/log.h"

#include "project/projectcommands.h"

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

    if (tryHandleGenerateAudioLink(parsed)) {
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

    commandDispatcher()->dispatch(project::PROJECT_OPEN_CLOUD_COMMAND, {
        { project::PROJECT_ID_PARAM, muse::Val(projectId) },
        { project::PROJECT_SNAPSHOT_ID_PARAM, muse::Val(snapshotId) },
    });

    return true;
}

bool CloudUrlHandler::tryHandleGenerateAudioLink(const QUrl& parsed)
{
    if (parsed.host() != QStringLiteral("generate-audio")) {
        return false;
    }

    const QUrlQuery query(parsed);
    const QString projectId = query.queryItemValue(QStringLiteral("projectId"));
    if (projectId.isEmpty()) {
        return false;
    }

    commandDispatcher()->dispatch(project::PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_FOR_PROJECT_COMMAND, {
        { project::PROJECT_ID_PARAM, muse::Val(projectId) },
    });

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

    commandDispatcher()->dispatch(project::PROJECT_OPEN_CLOUD_AUDIO_FILE_COMMAND, {
        { project::PROJECT_AUDIO_ID_PARAM, muse::Val(audioId.toStdString()) },
    });

    return true;
}
