/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QUrl>

namespace audacity::musehub {
class MuseHubUrlHandler : public QObject
{
    Q_OBJECT

public:
    explicit MuseHubUrlHandler(QObject* parent);
    ~MuseHubUrlHandler() override;

private slots:
    void openUrl(const QUrl& url);
};
}
