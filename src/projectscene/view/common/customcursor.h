/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>
#include <QString>
#include <QQuickItem>
#include <QCursor>

namespace {
constexpr static int DEFAULT_CURSOR_SIZE = 32;
}
namespace au::projectscene {
class CustomCursor : public QObject
{
    Q_OBJECT

    Q_PROPERTY(bool active READ active WRITE setActive NOTIFY activeChanged FINAL)
    Q_PROPERTY(QString source READ source WRITE setSource NOTIFY sourceChanged FINAL)
    Q_PROPERTY(int size READ size WRITE setSize NOTIFY sizeChanged FINAL)

public:
    explicit CustomCursor(QQuickItem* parent = nullptr);
    ~CustomCursor() = default;

    bool active() const;
    QString source() const;
    int size() const;

    void setActive(bool active);
    void setSource(QString source);
    void setSize(int size);

    //! Create a DPI-aware QCursor from a pixmap source path, scaled to logical \a size
    static QCursor createScaledCursor(const QString& source, int size);

    //! Apply a custom pixmap cursor to a specific QQuickItem (HiDPI-aware)
    Q_INVOKABLE static void setCursorShape(QQuickItem* item, const QString& source, int size = DEFAULT_CURSOR_SIZE);

    //! Globally override the cursor with a custom pixmap (HiDPI-aware).
    //! Use during drag operations to keep the cursor stable regardless of hover.
    Q_INVOKABLE static void overrideCursor(const QString& source, int size = DEFAULT_CURSOR_SIZE);

    //! Restore the cursor previously set with overrideCursor()
    Q_INVOKABLE static void restoreCursor();

signals:
    void activeChanged();
    void sourceChanged();
    void sizeChanged();

private:
    bool m_active = false;
    QString m_source;
    QCursor m_cursor;
    int m_size = DEFAULT_CURSOR_SIZE;
};
}
