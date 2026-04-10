/*
* Audacity: A Digital Audio Editor
*/

#include "customcursor.h"

#include <QGuiApplication>
#include <QScreen>
#include <QPixmap>
#include <QWindow>
#include <QQuickWindow>

#include "log.h"

using namespace au::projectscene;

namespace {
qreal resolveDevicePixelRatio(const QQuickItem* item)
{
    // Prefer the screen that currently owns the item's window, so cursors are
    // sized correctly on multi-monitor setups with mixed DPI.
    if (item) {
        if (QQuickWindow* window = item->window()) {
            if (QScreen* screen = window->screen()) {
                return screen->devicePixelRatio();
            }
        }
    }

    // Fall back to the screen of the currently focused window.
    if (const QWindow* focused = QGuiApplication::focusWindow()) {
        if (QScreen* screen = focused->screen()) {
            return screen->devicePixelRatio();
        }
    }

    // Last resort: the primary screen.
    if (QScreen* screen = QGuiApplication::primaryScreen()) {
        return screen->devicePixelRatio();
    }

    return 1.0;
}
}

QCursor CustomCursor::createScaledCursor(const QString& source, int size, const QQuickItem* item)
{
    const qreal dpr = resolveDevicePixelRatio(item);

    QPixmap pixmap(source);
    if (pixmap.isNull()) {
        LOGW() << "Failed to load bitmap from source: " << source;
        return QCursor();
    }

    const int physicalSize = qRound(size * dpr);
    QPixmap scaledPixmap = pixmap.scaled(physicalSize, physicalSize, Qt::KeepAspectRatio, Qt::SmoothTransformation);
    scaledPixmap.setDevicePixelRatio(dpr);
    return QCursor(scaledPixmap);
}

void CustomCursor::setCursorShape(QQuickItem* item, const QString& source, int size)
{
    if (!item) {
        return;
    }

    QCursor cursor = createScaledCursor(source, size, item);
    if (cursor.shape() == Qt::BitmapCursor) {
        item->setCursor(cursor);
    }
}

void CustomCursor::overrideCursor(const QString& source, int size)
{
    QCursor cursor = createScaledCursor(source, size);
    if (cursor.shape() == Qt::BitmapCursor) {
        QGuiApplication::setOverrideCursor(cursor);
    }
}

void CustomCursor::overrideStandardCursor(int shape)
{
    QGuiApplication::setOverrideCursor(QCursor(static_cast<Qt::CursorShape>(shape)));
}

void CustomCursor::restoreCursor()
{
    QGuiApplication::restoreOverrideCursor();
}

CustomCursor::CustomCursor(QQuickItem*)
{
    auto changeCursor = [this](){
        if (m_active) {
            QGuiApplication::restoreOverrideCursor();

            QCursor cursor = createScaledCursor(m_source, m_size);
            if (cursor.shape() == Qt::BitmapCursor) {
                m_cursor = cursor;
                QGuiApplication::setOverrideCursor(m_cursor);
            }
        } else {
            QGuiApplication::restoreOverrideCursor();
        }
    };

    connect(this, &CustomCursor::activeChanged, changeCursor);
    connect(this, &CustomCursor::sourceChanged, changeCursor);
    connect(this, &CustomCursor::sizeChanged, changeCursor);
}

bool CustomCursor::active() const
{
    return m_active;
}

QString CustomCursor::source() const
{
    return m_source;
}

int CustomCursor::size() const
{
    return m_size;
}

void CustomCursor::setActive(bool active)
{
    if (m_active == active) {
        return;
    }

    m_active = active;
    emit activeChanged();
}

void CustomCursor::setSource(QString source)
{
    if (m_source == source) {
        return;
    }

    m_source = source;
    emit sourceChanged();
}

void CustomCursor::setSize(int size)
{
    if (m_size == size) {
        return;
    }

    m_size = size;
    emit sizeChanged();
}
