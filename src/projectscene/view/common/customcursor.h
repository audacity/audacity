/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>
#include <QString>
#include <QQuickItem>
#include <QCursor>

namespace au::projectscene {
constexpr static int DEFAULT_CURSOR_SIZE = 32;

//! QML component that installs a global override cursor from a pixmap source
//! while \a active is true. Instantiated from QML via CustomCursor {} elements.
class CustomCursor : public QObject
{
    Q_OBJECT

    Q_PROPERTY(bool active READ active WRITE setActive NOTIFY activeChanged FINAL)
    Q_PROPERTY(QString source READ source WRITE setSource NOTIFY sourceChanged FINAL)
    Q_PROPERTY(int size READ size WRITE setSize NOTIFY sizeChanged FINAL)

public:
    explicit CustomCursor(QObject* parent = nullptr);
    ~CustomCursor() = default;

    bool active() const;
    QString source() const;
    int size() const;

    void setActive(bool active);
    void setSource(QString source);
    void setSize(int size);

signals:
    void activeChanged();
    void sourceChanged();
    void sizeChanged();

private:
    void refresh();

    bool m_active = false;
    QString m_source;
    QCursor m_cursor;
    int m_size = DEFAULT_CURSOR_SIZE;
};

//! QML singleton exposing cursor helpers to QML code.
//! Use CustomCursorProvider.setCursorShape(...) / overrideCursor(...) / restoreCursor()
//! from QML to apply HiDPI-aware custom cursors.
class CustomCursorProvider : public QObject
{
    Q_OBJECT

    Q_PROPERTY(int defaultSize READ defaultSize CONSTANT FINAL)

public:
    explicit CustomCursorProvider(QObject* parent = nullptr);
    ~CustomCursorProvider() = default;

    int defaultSize() const { return DEFAULT_CURSOR_SIZE; }

    //! Create a DPI-aware QCursor from a pixmap source path, scaled to logical \a size.
    //! If \a item is provided, its window's screen is used to read the device pixel ratio,
    //! so the cursor is correctly sized on the screen that currently owns the item
    //! (important for multi-monitor setups with mixed DPI).
    static QCursor createScaledCursor(const QString& source, int size, const QQuickItem* item = nullptr);

    //! Create a DPI-aware QCursor from a glyph of an icon font, drawn like an icon label with an outline style:
    //! black, outlined in white. \a pixelSize is the logical font size.
    static QCursor createIconCursor(const QString& fontFamily, int iconCode, int pixelSize, const QQuickItem* item = nullptr);

    //! Apply a custom pixmap cursor to a specific QQuickItem (HiDPI-aware)
    Q_INVOKABLE static void setCursorShape(QQuickItem* item, const QString& source, int size = DEFAULT_CURSOR_SIZE);

    //! Apply a cursor drawn from an icon font glyph to a specific QQuickItem (HiDPI-aware)
    Q_INVOKABLE static void setIconCursorShape(QQuickItem* item, const QString& fontFamily, int iconCode, int pixelSize);

    Q_INVOKABLE static void setStandardCursorShape(QQuickItem* item, Qt::CursorShape shape);

    //! Globally override the cursor with a custom pixmap (HiDPI-aware).
    //! Use during drag operations to keep the cursor stable regardless of hover.
    Q_INVOKABLE static void overrideCursor(const QString& source, int size = DEFAULT_CURSOR_SIZE);

    //! Globally override the cursor with one drawn from an icon font glyph (HiDPI-aware).
    //! Use during drag operations to keep the cursor stable regardless of hover.
    //! Always overrides the cursor, so that each call can be paired with restoreCursor().
    Q_INVOKABLE static void overrideIconCursor(const QString& fontFamily, int iconCode, int pixelSize);

    //! Globally override the cursor with a standard Qt cursor shape.
    //! Use during drag operations to keep the cursor stable regardless of hover.
    Q_INVOKABLE static void overrideStandardCursor(Qt::CursorShape shape);

    //! Restore the cursor previously set with overrideCursor() / overrideStandardCursor()
    Q_INVOKABLE static void restoreCursor();
};
}
