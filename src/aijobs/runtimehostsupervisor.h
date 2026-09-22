/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

class QProcess;
class QTcpSocket;
class QTimer;

namespace au::aijobs {
class RuntimeHostSupervisor final : public QObject
{
    Q_OBJECT

public:
    explicit RuntimeHostSupervisor(QObject* parent = nullptr);
    ~RuntimeHostSupervisor() override;

    void start();
    void restartInWorkspace(const QString& workspace);
    void submitTestJob();
    void submitFailureTest();
    void cancelTestJob();
    QString statusText() const;

signals:
    void statusChanged(const QString& status);
    void testJobCompleted(const QString& jobId, const QString& resultManifest);
    void testJobAccepted(const QString& jobId);
    void testJobCancelled(const QString& jobId);
    void testJobFailed(const QString& jobId);

private:
    void setStatus(const QString& status);
    void connectHealthCheck(quint16 port);

    QProcess* m_process = nullptr;
    QTcpSocket* m_socket = nullptr;
    QTimer* m_startupTimer = nullptr;
    QString m_token;
    QString m_status;
    QString m_workspace;
    QString m_activeJobId;
};
}
