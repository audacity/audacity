#include "oauthhttpserverreplyhandler.h"

#include <cctype>
#include <cstring>

#include <QCoreApplication>
#include <QNetworkReply>
#include <QPointer>
#include <QTcpServer>
#include <QTcpSocket>
#include <QUrl>
#include <QUrlQuery>

#include "framework/global/translation.h"
#include "framework/global/log.h"

using namespace au::au3cloud;

class OAuthHttpServerReplyHandler::Impl
{
public:
    explicit Impl(OAuthHttpServerReplyHandler* p);
    ~Impl();

    QTcpServer m_httpServer;
    QHostAddress m_listenAddress = QHostAddress::LocalHost;
    QString m_path;

    QUrl m_redirectUrl;

    bool hasPendingSocket();
    void sendRedirect(const QUrl& url);
    void sendError();

private:
    void onClientConnected();
    void readData(QTcpSocket* socket);
    void answerClient(QTcpSocket* socket, const QUrl& url);
    void answerPendingClient(const QByteArray& response);
    QByteArray makeResponse(const QByteArray& status, const QUrl& location, const QString& text) const;

    struct HttpRequest {
        quint16 port = 0;

        bool readMethod(QTcpSocket* socket);
        bool readUrl(QTcpSocket* socket);
        bool readStatus(QTcpSocket* socket);
        bool readHeader(QTcpSocket* socket);

        enum class State {
            ReadingMethod,
            ReadingUrl,
            ReadingStatus,
            ReadingHeader,
            ReadingBody,
            AllDone
        } state = State::ReadingMethod;

        QByteArray m_fragment;

        enum class Method {
            Unknown,
            Head,
            Get,
            Put,
            Post,
            Delete,
        } method = Method::Unknown;

        QUrl url;
        QPair<quint8, quint8> version;
        QMap<QByteArray, QByteArray> headers;
    };

    QMap<QTcpSocket*, HttpRequest> m_clients;
    QPointer<QTcpSocket> m_pendingSocket;

    OAuthHttpServerReplyHandler* m_public = nullptr;
};

OAuthHttpServerReplyHandler::Impl::Impl(OAuthHttpServerReplyHandler* p)
    : m_public(p)
{
    QObject::connect(&m_httpServer, &QTcpServer::newConnection, [this]() { onClientConnected(); });
}

OAuthHttpServerReplyHandler::Impl::~Impl()
{
    if (m_httpServer.isListening()) {
        m_httpServer.close();
    }
}

void OAuthHttpServerReplyHandler::Impl::onClientConnected()
{
    QTcpSocket* socket = m_httpServer.nextPendingConnection();

    QObject::connect(socket, &QTcpSocket::disconnected, socket, &QTcpSocket::deleteLater);
    QObject::connect(socket, &QTcpSocket::readyRead, [this, socket]() { readData(socket); });
}

void OAuthHttpServerReplyHandler::Impl::readData(QTcpSocket* socket)
{
    if (!m_clients.contains(socket)) {
        m_clients[socket].port = m_httpServer.serverPort();
    }

    HttpRequest* request = &m_clients[socket];
    bool error = false;

    if (Q_LIKELY(request->state == HttpRequest::State::ReadingMethod)) {
        if (Q_UNLIKELY(error = !request->readMethod(socket))) {
            LOGW() << "Invalid Method";
        }
    }

    if (Q_LIKELY(!error && request->state == HttpRequest::State::ReadingUrl)) {
        if (Q_UNLIKELY(error = !request->readUrl(socket))) {
            LOGW() << "Invalid URL";
        }
    }

    if (Q_LIKELY(!error && request->state == HttpRequest::State::ReadingStatus)) {
        if (Q_UNLIKELY(error = !request->readStatus(socket))) {
            LOGW() << "Invalid Status";
        }
    }

    if (Q_LIKELY(!error && request->state == HttpRequest::State::ReadingHeader)) {
        if (Q_UNLIKELY(error = !request->readHeader(socket))) {
            LOGW() << "Invalid Header";
        }
    }

    if (error) {
        socket->disconnectFromHost();
        m_clients.remove(socket);
    } else if (!request->url.isEmpty()) {
        Q_ASSERT(request->state != HttpRequest::State::ReadingUrl);
        answerClient(socket, request->url);
        m_clients.remove(socket);
    }
}

void OAuthHttpServerReplyHandler::Impl::answerClient(QTcpSocket* socket, const QUrl& url)
{
    if (!url.path().startsWith(QLatin1String("/") + m_path)) {
        LOGW() << "Invalid request: " << url;
        socket->disconnectFromHost();
        return;
    }

    QVariantMap receivedData;
    const QUrlQuery query(url.query());
    const auto items = query.queryItems();
    for (auto it = items.begin(), end = items.end(); it != end; ++it) {
        receivedData.insert(it->first, it->second);
    }

    if (!receivedData.contains(QStringLiteral("code")) && !receivedData.contains(QStringLiteral("error"))) {
        // Not an OAuth callback (e.g. a favicon request); don't touch the pending socket
        socket->write(QByteArrayLiteral("HTTP/1.0 404 Not Found\r\nContent-Length: 0\r\n\r\n"));
        socket->disconnectFromHost();
        return;
    }

    if (m_pendingSocket) {
        m_pendingSocket->disconnectFromHost();
    }

    m_pendingSocket = socket;

    Q_EMIT m_public->callbackReceived(receivedData);
}

bool OAuthHttpServerReplyHandler::Impl::hasPendingSocket()
{
    return m_pendingSocket;
}

void OAuthHttpServerReplyHandler::Impl::sendRedirect(const QUrl& url)
{
    if (!m_pendingSocket) {
        return;
    }

    const QUrl target = url.isEmpty() ? m_redirectUrl : url;
    const QString text = muse::qtrc("cloud", "Sign in successful! You’re good to go back to Audacity.");
    const QByteArray status = target.isEmpty() ? QByteArrayLiteral("200 OK") : QByteArrayLiteral("302 Found");

    answerPendingClient(makeResponse(status, target, text));
}

void OAuthHttpServerReplyHandler::Impl::sendError()
{
    if (!m_pendingSocket) {
        return;
    }

    const QString text = muse::qtrc("cloud", "Sign in failed. Please return to Audacity and try again.");
    answerPendingClient(makeResponse(QByteArrayLiteral("200 OK"), QUrl(), text));
}

QByteArray OAuthHttpServerReplyHandler::Impl::makeResponse(const QByteArray& status, const QUrl& location, const QString& text) const
{
    const QByteArray html = QByteArrayLiteral("<html><head><title>")
                            + qApp->applicationName().toUtf8()
                            + QByteArrayLiteral("</title></head><body>")
                            + text.toUtf8()
                            + QByteArrayLiteral("</body></html>");

    QByteArray response = QByteArrayLiteral("HTTP/1.0 ") + status + QByteArrayLiteral("\r\n");
    if (!location.isEmpty()) {
        response += QByteArrayLiteral("Location: ") + location.toString().toUtf8() + QByteArrayLiteral("\r\n");
    }
    response += QByteArrayLiteral("Content-Type: text/html; charset=\"utf-8\"\r\n"
                                  "Content-Length: ") + QByteArray::number(html.size())
                + QByteArrayLiteral("\r\n\r\n")
                + html;

    return response;
}

void OAuthHttpServerReplyHandler::Impl::answerPendingClient(const QByteArray& response)
{
    m_pendingSocket->write(response);
    m_pendingSocket->disconnectFromHost();
    m_pendingSocket.clear();
}

bool OAuthHttpServerReplyHandler::Impl::HttpRequest::readMethod(QTcpSocket* socket)
{
    bool finished = false;
    while (socket->bytesAvailable() && !finished) {
        char c;
        socket->getChar(&c);
        if (std::isupper(c) && m_fragment.size() < 6) {
            m_fragment += c;
        } else {
            finished = true;
        }
    }
    if (finished) {
        if (m_fragment == "HEAD") {
            method = Method::Head;
        } else if (m_fragment == "GET") {
            method = Method::Get;
        } else if (m_fragment == "PUT") {
            method = Method::Put;
        } else if (m_fragment == "POST") {
            method = Method::Post;
        } else if (m_fragment == "DELETE") {
            method = Method::Delete;
        } else {
            LOGW() << "Invalid operation: " << m_fragment;
        }

        state = State::ReadingUrl;
        m_fragment.clear();

        return method != Method::Unknown;
    }
    return true;
}

bool OAuthHttpServerReplyHandler::Impl::HttpRequest::readUrl(QTcpSocket* socket)
{
    bool finished = false;
    while (socket->bytesAvailable() && !finished) {
        char c;
        socket->getChar(&c);
        if (std::isspace(c)) {
            finished = true;
        } else {
            m_fragment += c;
        }
    }
    if (finished) {
        if (!m_fragment.startsWith("/")) {
            LOGW() << "Invalid URL path: " << m_fragment;
            return false;
        }
        url.setUrl(QStringLiteral("http://127.0.0.1:") + QString::number(port)
                   + QString::fromUtf8(m_fragment));
        state = State::ReadingStatus;
        if (!url.isValid()) {
            LOGW() << "Invalid URL " << m_fragment;
            return false;
        }
        m_fragment.clear();
        return true;
    }
    return true;
}

bool OAuthHttpServerReplyHandler::Impl::HttpRequest::readStatus(QTcpSocket* socket)
{
    bool finished = false;
    while (socket->bytesAvailable() && !finished) {
        char c;
        socket->getChar(&c);
        m_fragment += c;
        if (m_fragment.endsWith("\r\n")) {
            finished = true;
            m_fragment.resize(m_fragment.size() - 2);
        }
    }
    if (finished) {
        if (!std::isdigit(m_fragment.at(m_fragment.size() - 3))
            || !std::isdigit(m_fragment.at(m_fragment.size() - 1))) {
            LOGW() << "Invalid version";
            return false;
        }
        version = qMakePair(m_fragment.at(m_fragment.size() - 3) - '0',
                            m_fragment.at(m_fragment.size() - 1) - '0');
        state = State::ReadingHeader;
        m_fragment.clear();
    }
    return true;
}

bool OAuthHttpServerReplyHandler::Impl::HttpRequest::readHeader(QTcpSocket* socket)
{
    while (socket->bytesAvailable()) {
        char c;
        socket->getChar(&c);
        m_fragment += c;
        if (m_fragment.endsWith("\r\n")) {
            if (m_fragment == "\r\n") {
                state = State::ReadingBody;
                m_fragment.clear();
                return true;
            } else {
                m_fragment.chop(2);
                const int index = m_fragment.indexOf(':');
                if (index == -1) {
                    return false;
                }

                const QByteArray key = m_fragment.mid(0, index).trimmed();
                const QByteArray value = m_fragment.mid(index + 1).trimmed();
                headers.insert(key, value);
                m_fragment.clear();
            }
        }
    }
    return false;
}

OAuthHttpServerReplyHandler::OAuthHttpServerReplyHandler(QObject* parent)
    : OAuthHttpServerReplyHandler(QHostAddress::Any, 0, parent)
{}

OAuthHttpServerReplyHandler::OAuthHttpServerReplyHandler(quint16 port, QObject* parent)
    : OAuthHttpServerReplyHandler(QHostAddress::Any, port, parent)
{}

OAuthHttpServerReplyHandler::OAuthHttpServerReplyHandler(const QHostAddress& address, quint16 port,
                                                         QObject* parent)
    : QOAuthOobReplyHandler(parent), m_impl(std::make_unique<Impl>(this))
{
    listen(address, port);
}

OAuthHttpServerReplyHandler::~OAuthHttpServerReplyHandler()
{}

QString OAuthHttpServerReplyHandler::callback() const
{
    Q_ASSERT(m_impl->m_httpServer.isListening());
    const QUrl url(QString::fromLatin1("http://127.0.0.1:%1/%2")
                   .arg(m_impl->m_httpServer.serverPort()).arg(m_impl->m_path));
    return url.toString(QUrl::EncodeDelimiters);
}

QString OAuthHttpServerReplyHandler::callbackPath() const
{
    return m_impl->m_path;
}

void OAuthHttpServerReplyHandler::setCallbackPath(const QString& path)
{
    QString copy = path;
    while (copy.startsWith(QLatin1Char('/'))) {
        copy = copy.mid(1);
    }

    m_impl->m_path = copy;
}

quint16 OAuthHttpServerReplyHandler::port() const
{
    return m_impl->m_httpServer.serverPort();
}

bool OAuthHttpServerReplyHandler::listen(const QHostAddress& address, quint16 port)
{
    return m_impl->m_httpServer.listen(address, port);
}

void OAuthHttpServerReplyHandler::close()
{
    return m_impl->m_httpServer.close();
}

bool OAuthHttpServerReplyHandler::isListening() const
{
    return m_impl->m_httpServer.isListening();
}

bool OAuthHttpServerReplyHandler::hasPendingSocket()
{
    return m_impl->hasPendingSocket();
}

void OAuthHttpServerReplyHandler::setRedirectUrl(const QUrl& url)
{
    m_impl->m_redirectUrl = url;
}

void OAuthHttpServerReplyHandler::sendRedirect(const QUrl& url)
{
    QMetaObject::invokeMethod(this, [this, url]() {
        m_impl->sendRedirect(url);
    }, Qt::QueuedConnection);
}

void OAuthHttpServerReplyHandler::sendError()
{
    QMetaObject::invokeMethod(this, [this]() {
        m_impl->sendError();
    }, Qt::QueuedConnection);
}
