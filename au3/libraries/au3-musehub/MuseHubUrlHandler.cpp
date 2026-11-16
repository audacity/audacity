/*
 * Audacity: A Digital Audio Editor
 */
#include "MuseHubUrlHandler.h"

#include <QDesktopServices>
#include <QUrlQuery>

#include "MuseHubService.h"

namespace audacity::musehub {
MuseHubUrlHandler::MuseHubUrlHandler(QObject* parent)
    : QObject(parent)
{
    // Set http and https url handlers, to prevent musehub leaking into the main app
    // and keep the integration code contained

    // This will conflict if in future other modules will set their handlers
    // Then we would need a centralaized uri dispatcher
    QDesktopServices::setUrlHandler("http", this, "openUrl");
    QDesktopServices::setUrlHandler("https", this, "openUrl");
}

MuseHubUrlHandler::~MuseHubUrlHandler()
{
    QDesktopServices::unsetUrlHandler("http");
    QDesktopServices::unsetUrlHandler("https");
}

void MuseHubUrlHandler::openUrl(const QUrl& url)
{
    QUrl targetUrl = url;
    const QString host = url.host();
    if (host == "www.musehub.com" || host == "musehub.com") {
        const QString path = url.path();
        QUrlQuery query(url);
        const QString productId = query.queryItemValue("id");
        if (path.startsWith("/plugin/") && !productId.isEmpty()) {
            query.removeAllQueryItems("id");
            targetUrl.setQuery(query);
            const std::string webUrl = targetUrl.toString(QUrl::FullyEncoded).toStdString();
            targetUrl = QUrl(QString::fromStdString(GetProductUrl(webUrl, productId.toStdString())));
        } else if ((path == "/category/plugin" || path == "/category/plugin/") && IsMuseHubRegistered()) {
            targetUrl = QUrl("musehub://plugins");
            targetUrl.setQuery(url.query());
        }
    }

    // Qt uses the default handler when openUrl is called from a URL handler.
    QDesktopServices::openUrl(targetUrl);
}
}
