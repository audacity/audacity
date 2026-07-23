/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/actions/iactionsdispatcher.h"

class QUrl;

namespace au::au3cloud {
//! Handles audacity:// deep links.
//! Supported urls:
//! Audio.com:
//! "audacity://open?projectId=<id>[&snapshotId=<id>]"
//! "audacity://open?audioId=<id>"
//! "audacity://generate-audio?projectId=<id>"
class CloudUrlHandler : public muse::Contextable
{
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };

public:
    explicit CloudUrlHandler(muse::modularity::ContextPtr ctx);

    void handle(const QString& url);

private:
    bool tryHandleProjectLink(const QUrl& parsedUrl);
    bool tryHandleAudioLink(const QUrl& parsedUrl);
    bool tryHandleGenerateAudioLink(const QUrl& parsedUrl);
};
}
