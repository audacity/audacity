/*
* Audacity: A Digital Audio Editor
*/

#include "customcursor.h"

using namespace au::projectscene;

CustomCursor::CustomCursor(QQuickItem*)
{
    auto changeCursor = [this](){
        if (m_active) {
            QGuiApplication::restoreOverrideCursor();

            QPixmap pixmap(m_source);
            if (!pixmap.isNull()) {
                m_cursor = QCursor(pixmap.scaled(m_size, m_size, Qt::KeepAspectRatio, Qt::SmoothTransformation));
                QGuiApplication::setOverrideCursor(m_cursor);
            } else {
                qWarning() << "Failed to load bitmap from source:" << m_source;
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
