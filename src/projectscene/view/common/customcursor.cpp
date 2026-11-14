/*
* Audacity: A Digital Audio Editor
*/

#include "customcursor.h"

#include <QGuiApplication>
#include <QScreen>
#include <QPainter>
#include <QPainterPath>
#include <QPixmap>
#include <QtMath>
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

// ---------- CustomCursorProvider ----------

CustomCursorProvider::CustomCursorProvider(QObject* parent)
    : QObject(parent)
{
}

QCursor CustomCursorProvider::createScaledCursor(const QString& source, int size, const QQuickItem* item)
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

QCursor CustomCursorProvider::createIconCursor(const QString& fontFamily, int iconCode, int pixelSize, const QQuickItem* item)
{
    const qreal dpr = resolveDevicePixelRatio(item);

    QFont font(fontFamily);
    font.setPixelSize(qRound(pixelSize * dpr));
    QPainterPath path;
    path.addText(0, 0, font, QString(QChar(iconCode)));
    if (path.isEmpty()) {
        LOGW() << "Failed to draw icon " << iconCode << " from font " << fontFamily;
        return QCursor();
    }

    // Room for the outline; the hot spot is the center
    constexpr auto outlineWidth = 1.0;
    const int physicalSize = qCeil((pixelSize + 4 * outlineWidth) * dpr);
    path.translate(QPointF(physicalSize, physicalSize) / 2 - path.boundingRect().center());

    QPixmap pixmap(physicalSize, physicalSize);
    pixmap.fill(Qt::transparent);
    {
        QPainter painter(&pixmap);
        painter.setRenderHint(QPainter::Antialiasing);
        painter.strokePath(path, QPen(Qt::white, 2 * outlineWidth * dpr, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin));
        painter.fillPath(path, Qt::black);
    }
    pixmap.setDevicePixelRatio(dpr);
    return QCursor(pixmap);
}

namespace {
constexpr const char* kCursorSourceProp = "_customCursorSource";
constexpr const char* kCursorSizeProp = "_customCursorSize";
constexpr const char* kCursorIconFontFamilyProp = "_customCursorIconFontFamily";
constexpr const char* kCursorIconCodeProp = "_customCursorIconCode";
constexpr const char* kCursorItemHookedProp = "_customCursorItemHooked";
constexpr const char* kCursorHookedWindowProp = "_customCursorHookedWindow";

void applyStoredCursor(QQuickItem* item)
{
    if (!item) {
        return;
    }

    const int size = item->property(kCursorSizeProp).toInt();
    QCursor cursor;
    if (const QString source = item->property(kCursorSourceProp).toString(); !source.isEmpty()) {
        cursor = CustomCursorProvider::createScaledCursor(source, size, item);
    } else if (const QString fontFamily = item->property(kCursorIconFontFamilyProp).toString(); !fontFamily.isEmpty()) {
        cursor = CustomCursorProvider::createIconCursor(fontFamily, item->property(kCursorIconCodeProp).toInt(), size, item);
    } else {
        return;
    }

    if (cursor.shape() == Qt::BitmapCursor) {
        item->setCursor(cursor);
    }
}

void hookScreenChanges(QQuickItem* item)
{
    if (!item) {
        return;
    }

    // Hook the current window's screenChanged so the cursor is rebuilt with
    // the correct device pixel ratio whenever the window moves between screens.
    // We store a raw pointer of the hooked window on the item to avoid
    // connecting the same window twice.
    QQuickWindow* window = item->window();
    if (window) {
        const QVariant hookedVar = item->property(kCursorHookedWindowProp);
        const auto previouslyHooked = hookedVar.value<QObject*>();
        if (previouslyHooked != window) {
            item->setProperty(kCursorHookedWindowProp, QVariant::fromValue<QObject*>(window));
            QObject::connect(window, &QWindow::screenChanged, item, [item]() {
                applyStoredCursor(item);
            });
        }
    }

    // The item's window may be set later or swapped. Hook windowChanged once.
    if (!item->property(kCursorItemHookedProp).toBool()) {
        item->setProperty(kCursorItemHookedProp, true);
        QObject::connect(item, &QQuickItem::windowChanged, item, [item](QQuickWindow*) {
            hookScreenChanges(item);
            applyStoredCursor(item);
        });
    }
}
}

void CustomCursorProvider::setCursorShape(QQuickItem* item, const QString& source, int size)
{
    if (!item) {
        return;
    }

    item->setProperty(kCursorSourceProp, source);
    item->setProperty(kCursorSizeProp, size);

    QCursor cursor = createScaledCursor(source, size, item);
    if (cursor.shape() == Qt::BitmapCursor) {
        item->setCursor(cursor);
    }

    hookScreenChanges(item);
}

void CustomCursorProvider::setIconCursorShape(QQuickItem* item, const QString& fontFamily, int iconCode, int pixelSize)
{
    if (!item) {
        return;
    }

    item->setProperty(kCursorSourceProp, QString());
    item->setProperty(kCursorIconFontFamilyProp, fontFamily);
    item->setProperty(kCursorIconCodeProp, iconCode);
    item->setProperty(kCursorSizeProp, pixelSize);

    applyStoredCursor(item);

    hookScreenChanges(item);
}

void CustomCursorProvider::setStandardCursorShape(QQuickItem* item, Qt::CursorShape shape)
{
    if (!item) {
        return;
    }

    // Forget the custom cursor, so that `applyStoredCursor` doesn't bring it back
    item->setProperty(kCursorSourceProp, QString());
    item->setProperty(kCursorIconFontFamilyProp, QString());

    item->setCursor(QCursor(shape));
}

void CustomCursorProvider::overrideCursor(const QString& source, int size)
{
    QCursor cursor = createScaledCursor(source, size);
    if (cursor.shape() == Qt::BitmapCursor) {
        QGuiApplication::setOverrideCursor(cursor);
    }
}

void CustomCursorProvider::overrideIconCursor(const QString& fontFamily, int iconCode, int pixelSize)
{
    QCursor cursor = createIconCursor(fontFamily, iconCode, pixelSize);
    QGuiApplication::setOverrideCursor(cursor.shape() == Qt::BitmapCursor ? cursor : QCursor(Qt::ArrowCursor));
}

void CustomCursorProvider::overrideStandardCursor(Qt::CursorShape shape)
{
    QGuiApplication::setOverrideCursor(QCursor(shape));
}

void CustomCursorProvider::restoreCursor()
{
    QGuiApplication::restoreOverrideCursor();
}

// ---------- CustomCursor (QML component) ----------

CustomCursor::CustomCursor(QObject* parent)
    : QObject(parent)
{
    connect(this, &CustomCursor::activeChanged, this, &CustomCursor::refresh);
    connect(this, &CustomCursor::sourceChanged, this, &CustomCursor::refresh);
    connect(this, &CustomCursor::sizeChanged, this, &CustomCursor::refresh);
}

void CustomCursor::refresh()
{
    if (!m_active) {
        QGuiApplication::restoreOverrideCursor();
        return;
    }

    QGuiApplication::restoreOverrideCursor();

    QCursor cursor = CustomCursorProvider::createScaledCursor(m_source, m_size);
    if (cursor.shape() == Qt::BitmapCursor) {
        m_cursor = cursor;
        QGuiApplication::setOverrideCursor(m_cursor);
    }
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
