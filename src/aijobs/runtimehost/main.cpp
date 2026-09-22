/*
 * Audacity: A Digital Audio Editor
 */
#include <QCommandLineParser>
#include <QCoreApplication>
#include <QDir>
#include <QElapsedTimer>
#include <QEventLoop>
#include <QFile>
#include <QJsonDocument>
#include <QJsonObject>
#include <QPointer>
#include <QTcpServer>
#include <QTcpSocket>
#include <QTextStream>
#include <QTimer>

#include <cmath>

namespace {
constexpr int ProtocolVersion = 1;

QByteArray response(bool ok, const QString& code, const QJsonObject& values = {})
{
    QJsonObject payload = values;
    payload.insert("ok", ok);
    payload.insert("code", code);
    payload.insert("protocolVersion", ProtocolVersion);
    return QJsonDocument(payload).toJson(QJsonDocument::Compact) + '\n';
}

bool writeDeterministicWav(const QString& path)
{
    constexpr int sampleRate = 48000;
    constexpr int frames = sampleRate;
    QByteArray wav(44 + frames * 2, '\0');
    auto write16 = [&wav](int offset, quint16 value) {
        wav[offset] = static_cast<char>(value & 0xff);
        wav[offset + 1] = static_cast<char>((value >> 8) & 0xff);
    };
    auto write32 = [&wav](int offset, quint32 value) {
        for (int index = 0; index < 4; ++index) {
            wav[offset + index] = static_cast<char>((value >> (index * 8)) & 0xff);
        }
    };
    std::memcpy(wav.data(), "RIFF", 4);
    write32(4, static_cast<quint32>(wav.size() - 8));
    std::memcpy(wav.data() + 8, "WAVEfmt ", 8);
    write32(16, 16);
    write16(20, 1);
    write16(22, 1);
    write32(24, sampleRate);
    write32(28, sampleRate * 2);
    write16(32, 2);
    write16(34, 16);
    std::memcpy(wav.data() + 36, "data", 4);
    write32(40, frames * 2);
    for (int frame = 0; frame < frames; ++frame) {
        const double sample = std::sin((2.0 * M_PI * 440.0 * frame) / sampleRate) * 0.2;
        write16(44 + frame * 2, static_cast<quint16>(static_cast<qint16>(sample * 32767.0)));
    }
    QFile output(path);
    return output.open(QIODevice::WriteOnly | QIODevice::Truncate) && output.write(wav) == wav.size();
}

class RuntimeHost final : public QObject
{
public:
    RuntimeHost(QString token, QString workspace)
        : m_token(std::move(token)), m_workspace(std::move(workspace))
    {
        m_jobTimer.setSingleShot(true);
        connect(&m_jobTimer, &QTimer::timeout, this, [this] { finishTestJob(); });
        connect(&m_server, &QTcpServer::newConnection, this, [this] {
            while (auto* socket = m_server.nextPendingConnection()) {
                connect(socket, &QTcpSocket::readyRead, socket, [this, socket] {
                    while (socket->canReadLine()) {
                        const auto document = QJsonDocument::fromJson(socket->readLine().trimmed());
                        const auto request = document.object();
                        if (document.isNull() || request.value("token").toString() != m_token) {
                            socket->write(response(false, "unauthorized"));
                            continue;
                        }
                        if (request.value("protocolVersion").toInt() != ProtocolVersion) {
                            socket->write(response(false, "unsupported-protocol"));
                            continue;
                        }
                        const QString action = request.value("action").toString();
                        if (action == "health") {
                            socket->write(response(true, "healthy", { { "state", "healthy" } }));
                        } else if (action == "test-job") {
                            startTestJob(socket);
                        } else if (action == "test-worker-failure") {
                            startFailureTest(socket);
                        } else if (action == "cancel") {
                            cancelTestJob(socket, request.value("jobId").toString());
                        } else {
                            socket->write(response(false, "unknown-action"));
                        }
                    }
                });
            }
        });
    }

    bool listen()
    {
        return m_server.listen(QHostAddress::LocalHost, 0);
    }

    quint16 port() const { return m_server.serverPort(); }

private:
    void startTestJob(QTcpSocket* socket)
    {
        if (!m_activeJobId.isEmpty()) {
            socket->write(response(false, "job-already-running", { { "jobId", m_activeJobId } }));
            return;
        }
        m_activeJobId = "test-provider-job";
        m_activeJobSocket = socket;
        socket->write(response(true, "accepted", { { "jobId", m_activeJobId }, { "state", "running" } }));
        m_jobTimer.start(1500);
    }

    void cancelTestJob(QTcpSocket* socket, const QString& jobId)
    {
        if (m_activeJobId.isEmpty() || jobId != m_activeJobId) {
            socket->write(response(false, "job-not-running", { { "jobId", jobId } }));
            return;
        }
        m_jobTimer.stop();
        m_activeJobId.clear();
        m_activeJobSocket = nullptr;
        socket->write(response(true, "cancelled", { { "jobId", jobId }, { "state", "cancelled" } }));
    }

    void startFailureTest(QTcpSocket* socket)
    {
        if (!m_activeJobId.isEmpty()) {
            socket->write(response(false, "job-already-running", { { "jobId", m_activeJobId } }));
            return;
        }
        m_activeJobId = "test-worker-failure-job";
        m_activeJobSocket = socket;
        socket->write(response(true, "accepted", { { "jobId", m_activeJobId }, { "state", "running" } }));
        QTimer::singleShot(500, this, [] { QCoreApplication::exit(70); });
    }

    void finishTestJob()
    {
        if (m_activeJobId.isEmpty()) {
            return;
        }
        const QString jobId = m_activeJobId;
        QTcpSocket* socket = m_activeJobSocket;
        m_activeJobId.clear();
        m_activeJobSocket = nullptr;
        const QString jobDirectory = QDir(m_workspace).filePath("jobs/" + jobId);
        if (!QDir().mkpath(jobDirectory)) {
            if (socket) {
                socket->write(response(false, "workspace-create-failed", { { "jobId", jobId } }));
            }
            return;
        }
        const QString wavPath = QDir(jobDirectory).filePath("output.wav");
        if (!writeDeterministicWav(wavPath)) {
            if (socket) {
                socket->write(response(false, "wav-write-failed", { { "jobId", jobId } }));
            }
            return;
        }
        const QJsonObject manifest {
            { "protocolVersion", ProtocolVersion },
            { "jobId", jobId },
            { "providerId", "test-provider" },
            { "state", "complete" },
            { "asset", "output.wav" }
        };
        QFile manifestFile(QDir(jobDirectory).filePath("result.json"));
        if (!manifestFile.open(QIODevice::WriteOnly | QIODevice::Truncate)
            || manifestFile.write(QJsonDocument(manifest).toJson(QJsonDocument::Compact)) < 1) {
            if (socket) {
                socket->write(response(false, "manifest-write-failed", { { "jobId", jobId } }));
            }
            return;
        }
        if (socket) {
            socket->write(response(true, "complete", { { "jobId", jobId }, { "resultManifest", "jobs/test-provider-job/result.json" } }));
        }
    }

    QString m_token;
    QString m_workspace;
    QTcpServer m_server;
    QTimer m_jobTimer;
    QString m_activeJobId;
    QPointer<QTcpSocket> m_activeJobSocket;
};

int selfTest(const QString& workspace)
{
    const QString token = "self-test-token";
    RuntimeHost host(token, workspace);
    if (!host.listen()) {
        return 10;
    }

    QTcpSocket client;
    client.connectToHost(QHostAddress::LocalHost, host.port());
    if (!client.waitForConnected(2000)) {
        return 11;
    }
    const auto waitForResponse = [&client] {
        QElapsedTimer timer;
        timer.start();
        while (!client.canReadLine() && timer.elapsed() < 2000) {
            QCoreApplication::processEvents(QEventLoop::AllEvents, 20);
        }
        return client.canReadLine() ? QJsonDocument::fromJson(client.readLine().trimmed()).object() : QJsonObject {};
    };
    const auto send = [&client, &waitForResponse](const QJsonObject& request) {
        client.write(QJsonDocument(request).toJson(QJsonDocument::Compact) + '\n');
        client.flush();
        return waitForResponse();
    };
    const auto denied = send({ { "token", "wrong" }, { "protocolVersion", ProtocolVersion }, { "action", "health" } });
    if (denied.value("code").toString() != "unauthorized") {
        return 12;
    }
    const auto accepted = send({ { "token", token }, { "protocolVersion", ProtocolVersion }, { "action", "test-job" } });
    if (accepted.value("code").toString() != "accepted") {
        return 13;
    }
    const auto completed = waitForResponse();
    if (completed.value("code").toString() != "complete") {
        return 14;
    }
    const QDir output(workspace);
    return QFile::exists(output.filePath("jobs/test-provider-job/output.wav"))
           && QFile::exists(output.filePath("jobs/test-provider-job/result.json")) ? 0 : 15;
}
}

int main(int argc, char* argv[])
{
    QCoreApplication application(argc, argv);
    QCommandLineParser parser;
    parser.addOption({ "workspace", "Workspace for job output.", "path" });
    parser.addOption({ "token", "Required session token.", "token" });
    parser.addOption(QCommandLineOption("self-test", "Run authenticated loopback and test-provider verification."));
    parser.process(application);

    const QString workspace = parser.value("workspace");
    if (workspace.isEmpty()) {
        return 2;
    }
    if (parser.isSet("self-test")) {
        return selfTest(workspace);
    }
    const QString token = parser.value("token");
    if (token.isEmpty()) {
        return 3;
    }
    RuntimeHost host(token, workspace);
    if (!host.listen()) {
        return 4;
    }
    QTextStream(stdout) << QJsonDocument(QJsonObject { { "port", host.port() }, { "protocolVersion", ProtocolVersion } }).toJson(QJsonDocument::Compact)
                        << Qt::endl;
    return application.exec();
}
