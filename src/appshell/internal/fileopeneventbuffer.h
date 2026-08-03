/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QList>
#include <QObject>
#include <QUrl>

class QCoreApplication;

namespace au::appshell {
//! Buffers QFileOpenEvent urls delivered before ApplicationActionController installs
//! its event filter. On macOS the launch url can arrive during an early event pump
//! (splash screen, module init), where an unfiltered QFileOpenEvent is silently lost.
class FileOpenEventBuffer : public QObject
{
public:
    static void install(QCoreApplication* app);
    static QList<QUrl> takePendingUrls();

private:
    explicit FileOpenEventBuffer(QObject* parent);

    bool eventFilter(QObject* watched, QEvent* event) override;

    static FileOpenEventBuffer* s_instance;
    QList<QUrl> m_pendingUrls;
};
}
