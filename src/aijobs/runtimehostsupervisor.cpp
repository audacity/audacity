/*
 * Audacity: A Digital Audio Editor
 */
#include "runtimehostsupervisor.h"

#include <QCoreApplication>
#include <QDir>
#include <QJsonDocument>
#include <QJsonObject>
#include <QProcess>
#include <QStandardPaths>
#include <QTcpSocket>
#include <QTimer>
#include <QUuid>

using namespace au::aijobs;

RuntimeHostSupervisor::RuntimeHostSupervisor(QObject* parent)
    : QObject(parent), m_status(tr("Runtime host not started"))
{
    m_process = new QProcess(this);
    m_socket = new QTcpSocket(this);
    m_startupTimer = new QTimer(this);
    m_startupTimer->setSingleShot(true);

    connect(m_startupTimer, &QTimer::timeout, this, [this] {
        if (m_status != tr("Runtime host healthy")) {
            setStatus(tr("Runtime host did not become ready"));
        }
    });
    connect(m_process, &QProcess::errorOccurred, this, [this] {
        setStatus(tr("Runtime host failed to start"));
    });
    connect(m_process, qOverload<int, QProcess::ExitStatus>(&QProcess::finished), this,
            [this](int, QProcess::ExitStatus) {
        if (!m_activeJobId.isEmpty()) {
            const QString failedJobId = m_activeJobId;
            m_activeJobId.clear();
            emit testJobFailed(failedJobId);
            setStatus(tr("Runtime host stopped while the test provider job was running"));
        }
    });
    connect(m_process, &QProcess::readyReadStandardOutput, this, [this] {
        while (m_process->canReadLine()) {
            const auto message = QJsonDocument::fromJson(m_process->readLine().trimmed()).object();
            const int port = message.value("port").toInt();
            if (port > 0 && message.value("protocolVersion").toInt() == 1) {
                connectHealthCheck(static_cast<quint16>(port));
            }
        }
    });
    connect(m_socket, &QTcpSocket::connected, this, [this] {
        const QJsonObject healthRequest {
            { "token", m_token },
            { "protocolVersion", 1 },
            { "action", "health" }
        };
        m_socket->write(QJsonDocument(healthRequest).toJson(QJsonDocument::Compact) + '\n');
    });
    connect(m_socket, &QTcpSocket::readyRead, this, [this] {
        while (m_socket->canReadLine()) {
            const auto response = QJsonDocument::fromJson(m_socket->readLine().trimmed()).object();
            const QString code = response.value("code").toString();
            if (response.value("ok").toBool() && code == "healthy") {
                m_startupTimer->stop();
                setStatus(tr("Runtime host healthy"));
            } else if (response.value("ok").toBool() && code == "accepted") {
                m_activeJobId = response.value("jobId").toString();
                emit testJobAccepted(m_activeJobId);
                setStatus(tr("Test provider job running"));
            } else if (response.value("ok").toBool() && code == "complete") {
                m_activeJobId.clear();
                emit testJobCompleted(response.value("jobId").toString(), response.value("resultManifest").toString());
                setStatus(tr("Test provider job complete"));
            } else if (response.value("ok").toBool() && code == "cancelled") {
                m_activeJobId.clear();
                emit testJobCancelled(response.value("jobId").toString());
                setStatus(tr("Test provider job cancelled"));
            } else {
                setStatus(tr("Runtime host request failed"));
            }
        }
    });
}

RuntimeHostSupervisor::~RuntimeHostSupervisor()
{
    if (m_process->state() != QProcess::NotRunning) {
        m_process->terminate();
        if (!m_process->waitForFinished(1000)) {
            m_process->kill();
        }
    }
}

void RuntimeHostSupervisor::start()
{
    if (m_process->state() != QProcess::NotRunning) {
        return;
    }
#ifdef Q_OS_WIN
    const QString executableName = "ai_runtime_host.exe";
#else
    const QString executableName = "ai_runtime_host";
#endif
    const QString executable = QDir(QCoreApplication::applicationDirPath()).filePath(executableName);
    if (m_workspace.isEmpty()) {
        m_workspace = QDir(QStandardPaths::writableLocation(QStandardPaths::TempLocation))
                      .filePath("AI Music Studio/runtime-host");
    }
    QDir().mkpath(m_workspace);
    m_token = QUuid::createUuid().toString(QUuid::WithoutBraces);
    setStatus(tr("Starting local runtime host"));
    m_process->start(executable, { "--workspace", m_workspace, "--token", m_token });
    m_startupTimer->start(5000);
}

void RuntimeHostSupervisor::restartInWorkspace(const QString& workspace)
{
    if (workspace.isEmpty()) {
        return;
    }
    if (m_process->state() != QProcess::NotRunning) {
        m_process->terminate();
        if (!m_process->waitForFinished(1000)) {
            m_process->kill();
            m_process->waitForFinished(1000);
        }
    }
    m_socket->abort();
    m_workspace = workspace;
    start();
}

void RuntimeHostSupervisor::submitTestJob()
{
    if (m_socket->state() != QAbstractSocket::ConnectedState || m_status != tr("Runtime host healthy")) {
        setStatus(tr("Runtime host is not ready for a test job"));
        return;
    }

    const QJsonObject request {
        { "token", m_token },
        { "protocolVersion", 1 },
        { "action", "test-job" }
    };
    setStatus(tr("Submitting test provider job"));
    m_socket->write(QJsonDocument(request).toJson(QJsonDocument::Compact) + '\n');
}

void RuntimeHostSupervisor::submitFailureTest()
{
    if (m_socket->state() != QAbstractSocket::ConnectedState || m_status != tr("Runtime host healthy")) {
        setStatus(tr("Runtime host is not ready for a failure test"));
        return;
    }
    const QJsonObject request {
        { "token", m_token },
        { "protocolVersion", 1 },
        { "action", "test-worker-failure" }
    };
    setStatus(tr("Submitting worker-failure test"));
    m_socket->write(QJsonDocument(request).toJson(QJsonDocument::Compact) + '\n');
}

void RuntimeHostSupervisor::cancelTestJob()
{
    if (m_socket->state() != QAbstractSocket::ConnectedState || m_activeJobId.isEmpty()) {
        setStatus(tr("No test provider job is running"));
        return;
    }
    const QJsonObject request {
        { "token", m_token },
        { "protocolVersion", 1 },
        { "action", "cancel" },
        { "jobId", m_activeJobId }
    };
    setStatus(tr("Cancelling test provider job"));
    m_socket->write(QJsonDocument(request).toJson(QJsonDocument::Compact) + '\n');
}

QString RuntimeHostSupervisor::statusText() const
{
    return m_status;
}

void RuntimeHostSupervisor::setStatus(const QString& status)
{
    if (m_status == status) {
        return;
    }
    m_status = status;
    emit statusChanged(m_status);
}

void RuntimeHostSupervisor::connectHealthCheck(quint16 port)
{
    if (m_socket->state() == QAbstractSocket::UnconnectedState) {
        m_socket->connectToHost(QHostAddress::LocalHost, port);
    }
}
