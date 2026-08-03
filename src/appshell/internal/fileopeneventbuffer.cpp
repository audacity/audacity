/*
 * Audacity: A Digital Audio Editor
 */
#include "fileopeneventbuffer.h"

#include <utility>

#include <QCoreApplication>
#include <QFileOpenEvent>

using namespace au::appshell;

FileOpenEventBuffer* FileOpenEventBuffer::s_instance = nullptr;

FileOpenEventBuffer::FileOpenEventBuffer(QObject* parent)
    : QObject(parent)
{
}

void FileOpenEventBuffer::install(QCoreApplication* app)
{
    if (s_instance) {
        return;
    }

    s_instance = new FileOpenEventBuffer(app);
    app->installEventFilter(s_instance);
}

QList<QUrl> FileOpenEventBuffer::takePendingUrls()
{
    if (!s_instance) {
        return {};
    }

    return std::exchange(s_instance->m_pendingUrls, {});
}

bool FileOpenEventBuffer::eventFilter(QObject* watched, QEvent* event)
{
    if (event->type() == QEvent::FileOpen && watched == qApp) {
        const QUrl url = static_cast<const QFileOpenEvent*>(event)->url();
        m_pendingUrls << url;
        return true;
    }

    return QObject::eventFilter(watched, event);
}
