#pragma once

#include <QOAuthOobReplyHandler>
#include <QHostAddress>

#include "framework/global/modularity/ioc.h"

namespace au::au3cloud {
class OAuthHttpServerReplyHandler : public QOAuthOobReplyHandler
{
    Q_OBJECT

public:
    explicit OAuthHttpServerReplyHandler(const muse::modularity::ContextPtr& iocCtx, QObject* parent = nullptr);
    explicit OAuthHttpServerReplyHandler(quint16 port, const muse::modularity::ContextPtr& iocCtx, QObject* parent = nullptr);
    explicit OAuthHttpServerReplyHandler(const QHostAddress& address, quint16 port, const muse::modularity::ContextPtr& iocCtx,
                                         QObject* parent = nullptr);
    ~OAuthHttpServerReplyHandler();

    QString callback() const override;

    QString callbackPath() const;
    void setCallbackPath(const QString& path);

    quint16 port() const;

    bool listen(const QHostAddress& address = QHostAddress::Any, quint16 port = 0);
    void close();
    bool isListening() const;

    void setRedirectUrl(const QUrl& url);

signals:
    void callbackReceived(const QVariantMap& data);

private:
    class Impl;

    std::unique_ptr<Impl> m_impl;
};
}
