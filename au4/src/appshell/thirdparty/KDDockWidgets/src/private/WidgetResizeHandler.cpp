/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "WidgetResizeHandler_p.h"
#include "FloatingWindow_p.h"
#include "TitleBar_p.h"
#include "DragController_p.h"
#include "Config.h"
#include "Qt5Qt6Compat_p.h"
#include "Utils_p.h"
#include "DockRegistry_p.h"
#include "MDILayoutWidget_p.h"

#include <QEvent>
#include <QMouseEvent>
#include <QDebug>
#include <QGuiApplication>
#include <QScreen>
#include <QWindow>
#include <QScopedValueRollback>

#if defined(Q_OS_WIN)
#include <QtGui/private/qhighdpiscaling_p.h>
#include <windowsx.h>
#include <windows.h>
#include <dwmapi.h>
#if defined(Q_CC_MSVC)
#pragma comment(lib, "Dwmapi.lib")
#pragma comment(lib, "User32.lib")
#endif
#endif

using namespace KDDockWidgets;

bool WidgetResizeHandler::s_disableAllHandlers = false;
WidgetResizeHandler::WidgetResizeHandler(bool isTopLevelResizer, QWidgetOrQuick *target)
    : QObject(target)
    , m_isTopLevelWindowResizer(isTopLevelResizer)
{
    setTarget(target);
}

WidgetResizeHandler::~WidgetResizeHandler()
{
}

void WidgetResizeHandler::setAllowedResizeSides(CursorPositions sides)
{
    mAllowedResizeSides = sides;
}

void WidgetResizeHandler::setResizeGap(int gap)
{
    m_resizeGap = gap;
}

bool WidgetResizeHandler::isMDI() const
{
    auto frame = qobject_cast<Frame *>(mTarget);
    return frame && frame->isMDI();
}

bool WidgetResizeHandler::isResizing() const
{
    return m_resizingInProgress;
}

int WidgetResizeHandler::widgetResizeHandlerMargin()
{
    return 4; // pixels
}

bool WidgetResizeHandler::eventFilter(QObject *o, QEvent *e)
{
    if (s_disableAllHandlers)
        return false;

    auto widget = qobject_cast<QWidgetOrQuick *>(o);
    if (!widget)
        return false;

    if (m_isTopLevelWindowResizer && (!widget->isTopLevel() || o != mTarget))
        return false;

    switch (e->type()) {
    case QEvent::MouseButtonPress: {
        if (mTarget->isMaximized())
            break;

        auto mouseEvent = static_cast<QMouseEvent *>(e);
        auto cursorPos = cursorPosition(Qt5Qt6Compat::eventGlobalPos(mouseEvent));
        updateCursor(cursorPos);
        if (cursorPos == CursorPosition_Undefined)
            return false;

        const int m = widgetResizeHandlerMargin();
        const QRect widgetRect = mTarget->rect().marginsAdded(QMargins(m, m, m, m));
        const QPoint cursorPoint = mTarget->mapFromGlobal(Qt5Qt6Compat::eventGlobalPos(mouseEvent));
        if (!widgetRect.contains(cursorPoint) || mouseEvent->button() != Qt::LeftButton)
            return false;

        m_resizingInProgress = true;
        if (isMDI())
            Q_EMIT DockRegistry::self()->frameInMDIResizeChanged();
        mNewPosition = Qt5Qt6Compat::eventGlobalPos(mouseEvent);
        mCursorPos = cursorPos;

        return true;
    }
    case QEvent::MouseButtonRelease: {
        m_resizingInProgress = false;
        if (isMDI()) {
            Q_EMIT DockRegistry::self()->frameInMDIResizeChanged();
            auto frame = static_cast<Frame *>(mTarget);
            // Usually in KDDW all geometry changes are done in the layout items, which propagate to the widgets
            // When resizing a MDI however, we're resizing the widget directly. So update the corresponding layout
            // item when we're finished.
            frame->mdiLayoutWidget()->setDockWidgetGeometry(frame, frame->QWidgetAdapter::geometry());
        }
        updateCursor(CursorPosition_Undefined);
        auto mouseEvent = static_cast<QMouseEvent *>(e);

        if (mTarget->isMaximized() || !m_resizingInProgress || mouseEvent->button() != Qt::LeftButton)
            break;

        mTarget->releaseMouse();
        mTarget->releaseKeyboard();
        return true;

        break;
    }
    case QEvent::MouseMove: {
        if (mTarget->isMaximized())
            break;

        if (isMDI()) {
            const Frame *frameBeingResized = DockRegistry::self()->frameInMDIResize();
            const bool otherFrameBeingResized = frameBeingResized && frameBeingResized != mTarget;
            if (otherFrameBeingResized) {
                // only one at a time!
                return false;
            }
        }

        auto mouseEvent = static_cast<QMouseEvent *>(e);
        m_resizingInProgress = m_resizingInProgress && (mouseEvent->buttons() & Qt::LeftButton);
        const bool state = m_resizingInProgress;
        if (m_isTopLevelWindowResizer)
            m_resizingInProgress = ((o == mTarget) && m_resizingInProgress);
        const bool consumed = mouseMoveEvent(mouseEvent);
        m_resizingInProgress = state;
        return consumed;
    }
    default:
        break;
    }
    return false;
}

bool WidgetResizeHandler::mouseMoveEvent(QMouseEvent *e)
{
    const QPoint globalPos = Qt5Qt6Compat::eventGlobalPos(e);
    if (!m_resizingInProgress) {
        const CursorPosition pos = cursorPosition(globalPos);
        updateCursor(pos);
        return pos != CursorPosition_Undefined;
    }

    const QRect oldGeometry = KDDockWidgets::globalGeometry(mTarget);
    QRect newGeometry = oldGeometry;

    QRect parentGeometry;
    if (!mTarget->isTopLevel()) {
        auto parent = KDDockWidgets::Private::parentWidget(mTarget);
        parentGeometry = KDDockWidgets::globalGeometry(parent);
    }

    {
        int deltaWidth = 0;
        int newWidth = 0;
        const int maxWidth = Layouting::Widget::widgetMaxSize(mTarget).width();
        const int minWidth = Layouting::Widget::widgetMinSize(mTarget).width();

        switch (mCursorPos) {
        case CursorPosition_TopLeft:
        case CursorPosition_Left:
        case CursorPosition_BottomLeft: {
            parentGeometry = parentGeometry.adjusted(0, m_resizeGap, 0, 0);
            deltaWidth = oldGeometry.left() - globalPos.x();
            newWidth = qBound(minWidth, mTarget->width() + deltaWidth, maxWidth);
            deltaWidth = newWidth - mTarget->width();
            if (deltaWidth != 0) {
                newGeometry.setLeft(newGeometry.left() - deltaWidth);
            }

            break;
        }

        case CursorPosition_TopRight:
        case CursorPosition_Right:
        case CursorPosition_BottomRight: {
            parentGeometry = parentGeometry.adjusted(0, 0, -m_resizeGap, 0);
            deltaWidth = globalPos.x() - newGeometry.right();
            newWidth = qBound(minWidth, mTarget->width() + deltaWidth, maxWidth);
            deltaWidth = newWidth - mTarget->width();
            if (deltaWidth != 0) {
                newGeometry.setRight(oldGeometry.right() + deltaWidth);
            }
            break;
        }
        default:
            break;
        }
    }

    {
        const int maxHeight = Layouting::Widget::widgetMaxSize(mTarget).height();
        const int minHeight = Layouting::Widget::widgetMinSize(mTarget).height();
        int deltaHeight = 0;
        int newHeight = 0;
        switch (mCursorPos) {
        case CursorPosition_TopLeft:
        case CursorPosition_Top:
        case CursorPosition_TopRight: {
            parentGeometry = parentGeometry.adjusted(0, m_resizeGap, 0, 0);
            deltaHeight = oldGeometry.top() - globalPos.y();
            newHeight = qBound(minHeight, mTarget->height() + deltaHeight, maxHeight);
            deltaHeight = newHeight - mTarget->height();
            if (deltaHeight != 0) {
                newGeometry.setTop(newGeometry.top() - deltaHeight);
            }

            break;
        }

        case CursorPosition_BottomLeft:
        case CursorPosition_Bottom:
        case CursorPosition_BottomRight: {
            parentGeometry = parentGeometry.adjusted(0, 0, 0, -m_resizeGap);
            deltaHeight = globalPos.y() - newGeometry.bottom();
            newHeight = qBound(minHeight, mTarget->height() + deltaHeight, maxHeight);
            deltaHeight = newHeight - mTarget->height();
            if (deltaHeight != 0) {
                newGeometry.setBottom(oldGeometry.bottom() + deltaHeight);
            }
            break;
        }
        default:
            break;
        }
    }

    if (newGeometry == mTarget->geometry()) {
        // Nothing to do.
        return true;
    }

    if (!mTarget->isTopLevel()) {

        // Clip to parent's geometry.
        newGeometry = newGeometry.intersected(parentGeometry);

        // Back to local.
        newGeometry.moveTopLeft(mTarget->mapFromGlobal(newGeometry.topLeft()) + mTarget->pos());
    }

    mTarget->setGeometry(newGeometry);
    return true;
}

#ifdef Q_OS_WIN

/// Handler to enable Aero-snap
bool WidgetResizeHandler::handleWindowsNativeEvent(FloatingWindow *fw, const QByteArray &eventType,
                                                   void *message, Qt5Qt6Compat::qintptr *result)
{
    if (eventType != "windows_generic_MSG")
        return false;

    auto msg = static_cast<MSG *>(message);
    if (msg->message == WM_NCHITTEST) {
        if (DragController::instance()->isInClientDrag()) {
            // There's a non-native drag going on.
            *result = 0;
            return false;
        }

        const QRect htCaptionRect = fw->dragRect();
        const bool ret = handleWindowsNativeEvent(fw->windowHandle(), msg, result, htCaptionRect);

        fw->setLastHitTest(*result);
        return ret;
    } else if (msg->message == WM_NCLBUTTONDBLCLK) {
        if ((Config::self().flags() & Config::Flag_DoubleClickMaximizes)) {
            return handleWindowsNativeEvent(fw->windowHandle(), msg, result, {});
        } else {
            // Let the title bar handle it. It will re-dock the window.
            if (TitleBar *titleBar = fw->titleBar()) {
                if (titleBar->isVisible()) { // can't be invisible afaik
                    titleBar->onDoubleClicked();
                }
            }

            return true;
        }
    }

    return handleWindowsNativeEvent(fw->windowHandle(), msg, result, {});
}

bool WidgetResizeHandler::handleWindowsNativeEvent(QWindow *w, MSG *msg,
                                                   Qt5Qt6Compat::qintptr *result,
                                                   const NativeFeatures &features)
{
    if (msg->message == WM_NCCALCSIZE && features.hasShadow()) {
        *result = 0;
        return true;
    } else if (msg->message == WM_NCHITTEST && (features.hasResize() || features.hasDrag())) {
        const int borderWidth = 8;
        const bool hasFixedWidth = w->minimumWidth() == w->maximumWidth();
        const bool hasFixedHeight = w->minimumHeight() == w->maximumHeight();

        *result = 0;
        const int xPos = GET_X_LPARAM(msg->lParam);
        const int yPos = GET_Y_LPARAM(msg->lParam);
        RECT rect;
        GetWindowRect(reinterpret_cast<HWND>(w->winId()), &rect);

        if (xPos >= rect.left && xPos <= rect.left + borderWidth && yPos <= rect.bottom && yPos >= rect.bottom - borderWidth && features.hasResize()) {
            *result = HTBOTTOMLEFT;
        } else if (xPos < rect.right && xPos >= rect.right - borderWidth && yPos <= rect.bottom && yPos >= rect.bottom - borderWidth && features.hasResize()) {
            *result = HTBOTTOMRIGHT;
        } else if (xPos >= rect.left && xPos <= rect.left + borderWidth && yPos >= rect.top && yPos <= rect.top + borderWidth && features.hasResize()) {
            *result = HTTOPLEFT;
        } else if (xPos <= rect.right && xPos >= rect.right - borderWidth && yPos >= rect.top && yPos < rect.top + borderWidth && features.hasResize()) {
            *result = HTTOPRIGHT;
        } else if (!hasFixedWidth && xPos >= rect.left && xPos <= rect.left + borderWidth && features.hasResize()) {
            *result = HTLEFT;
        } else if (!hasFixedHeight && yPos >= rect.top && yPos <= rect.top + borderWidth && features.hasResize()) {
            *result = HTTOP;
        } else if (!hasFixedHeight && yPos <= rect.bottom && yPos >= rect.bottom - borderWidth && features.hasResize()) {
            *result = HTBOTTOM;
        } else if (!hasFixedWidth && xPos <= rect.right && xPos >= rect.right - borderWidth && features.hasResize()) {
            *result = HTRIGHT;
        } else if (features.hasDrag()) {
            const QPoint globalPosQt = QHighDpi::fromNativePixels(QPoint(xPos, yPos), w);
            // htCaptionRect is the rect on which we allow for Windows to do a native drag
            const QRect htCaptionRect = features.htCaptionRect;
            if (globalPosQt.y() >= htCaptionRect.top() && globalPosQt.y() <= htCaptionRect.bottom() && globalPosQt.x() >= htCaptionRect.left() && globalPosQt.x() <= htCaptionRect.right()) {
                if (!KDDockWidgets::inDisallowDragWidget(globalPosQt)) { // Just makes sure the mouse isn't over the close button, we don't allow drag in that case.
                    *result = HTCAPTION;
                }
            }
        }

        return *result != 0;
    } else if (msg->message == WM_NCLBUTTONDBLCLK && features.hasMaximize()) {
        // By returning false we accept Windows native action, a maximize.
        // We could also call titleBar->onDoubleClicked(); here which will maximize if Flag_DoubleClickMaximizes is set,
        // but there's a bug in QWidget::showMaximized() on Windows when we're covering the native title bar, the window is maximized with an offset.
        // So instead, use a native maximize which works well
        return false;
    } else if (msg->message == WM_GETMINMAXINFO) {
        // Qt doesn't work well with windows that don't have title bar but have native frames.
        // When maximized they go out of bounds and the title bar is clipped, so catch WM_GETMINMAXINFO
        // and patch the size

        // According to microsoft docs it only works for the primary screen, but extrapolates for the others
        QScreen *screen = QGuiApplication::primaryScreen();
        if (!screen || w->screen() != screen) {
            return false;
        }

        DefWindowProc(msg->hwnd, msg->message, msg->wParam, msg->lParam);

        const QRect availableGeometry = screen->availableGeometry();

        auto mmi = reinterpret_cast<MINMAXINFO *>(msg->lParam);
        const qreal dpr = screen->devicePixelRatio();

        mmi->ptMaxSize.y = int(availableGeometry.height() * dpr);
        mmi->ptMaxSize.x = int(availableGeometry.width() * dpr) - 1; // -1 otherwise it gets bogus size
        mmi->ptMaxPosition.x = availableGeometry.x();
        mmi->ptMaxPosition.y = availableGeometry.y();

        mmi->ptMinTrackSize.x = int(w->minimumWidth() * dpr);
        mmi->ptMinTrackSize.y = int(w->minimumHeight() * dpr);

        *result = 0;
        return true;
    }

    return false;
}

#endif

void WidgetResizeHandler::setTarget(QWidgetOrQuick *w)
{
    if (w) {
        mTarget = w;
        mTarget->setMouseTracking(true);
        if (m_isTopLevelWindowResizer) {
            mTarget->installEventFilter(this);
        } else {
            qApp->installEventFilter(this);
        }
    } else {
        qWarning() << "Target widget is null!";
    }
}

void WidgetResizeHandler::updateCursor(CursorPosition m)
{
#ifdef KDDOCKWIDGETS_QTWIDGETS
    //Need for updating cursor when we change child widget
    const QObjectList children = mTarget->children();
    for (int i = 0, total = children.size(); i < total; ++i) {
        if (auto child = qobject_cast<WidgetType *>(children.at(i))) {

            if (!child->testAttribute(Qt::WA_SetCursor)) {
                child->setCursor(Qt::ArrowCursor);
            }
        }
    }
#endif

    switch (m) {
    case CursorPosition_TopLeft:
    case CursorPosition_BottomRight:
        setMouseCursor(Qt::SizeFDiagCursor);
        break;
    case CursorPosition_BottomLeft:
    case CursorPosition_TopRight:
        setMouseCursor(Qt::SizeBDiagCursor);
        break;
    case CursorPosition_Top:
    case CursorPosition_Bottom:
        setMouseCursor(Qt::SizeVerCursor);
        break;
    case CursorPosition_Left:
    case CursorPosition_Right:
        setMouseCursor(Qt::SizeHorCursor);
        break;
    case CursorPosition_Undefined:
        restoreMouseCursor();
        break;
    case CursorPosition_All:
    case CursorPosition_Horizontal:
    case CursorPosition_Vertical:
        // Doesn't happen
        break;
    }
}

void WidgetResizeHandler::setMouseCursor(Qt::CursorShape cursor)
{
    if (m_isTopLevelWindowResizer)
        mTarget->setCursor(cursor);
    else
        qApp->setOverrideCursor(cursor);
}

void WidgetResizeHandler::restoreMouseCursor()
{
    if (m_isTopLevelWindowResizer)
        mTarget->setCursor(Qt::ArrowCursor);
    else
        qApp->restoreOverrideCursor();
}

CursorPosition WidgetResizeHandler::cursorPosition(QPoint globalPos) const
{
    if (!mTarget)
        return CursorPosition_Undefined;

#ifdef KDDOCKWIDGETS_QTQUICK
    if (isMDI()) {
        // Special case for QtQuick. The MouseAreas are driving it and know better what's the
        // cursor position
        return CursorPosition(mTarget->property("cursorPosition").toInt());
    }
#endif

    QPoint pos = mTarget->mapFromGlobal(globalPos);

    const int x = pos.x();
    const int y = pos.y();
    const int margin = widgetResizeHandlerMargin();

    QFlags<CursorPosition>::Int result = CursorPosition_Undefined;
    if (qAbs(x) <= margin)
        result |= CursorPosition_Left;
    else if (qAbs(x - (mTarget->width() - margin)) <= margin)
        result |= CursorPosition_Right;

    if (qAbs(y) <= margin)
        result |= CursorPosition_Top;
    else if (qAbs(y - (mTarget->height() - margin)) <= margin)
        result |= CursorPosition_Bottom;

    // Filter out sides we don't allow
    result = result & mAllowedResizeSides;

    return static_cast<CursorPosition>(result);
}

/** static */
void WidgetResizeHandler::setupWindow(QWindow *window)
{
    // Does some minor setup on our QWindow.
    // Like adding the drop shadow on Windows and two other workarounds.

#if defined(Q_OS_WIN)
    if (KDDockWidgets::usesAeroSnapWithCustomDecos()) {
        const auto wid = HWND(window->winId());
        connect(window, &QWindow::screenChanged, window, [wid] {
            // Qt honors our frame hijacking usually... but when screen changes we must give it a
            // nudge. Otherwise what Qt thinks is the client area is not what Windows knows it is.
            // SetWindowPos() will trigger an NCCALCSIZE message, which Qt will intercept and take
            // note of the margins we're using.
            SetWindowPos(wid, 0, 0, 0, 0, 0,
                         SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_FRAMECHANGED);
        });

        const bool usesTransparentFloatingWindow =
            Config::self().internalFlags() & Config::InternalFlag_UseTransparentFloatingWindow;
        if (!usesTransparentFloatingWindow) {
            // This enables the native drop shadow.
            // Doesn't work well if the floating window has transparent round corners (shows weird white line).

            MARGINS margins = { 0, 0, 0, 1 }; // arbitrary, just needs to be > 0 it seems
            DwmExtendFrameIntoClientArea(wid, &margins);
        }
    }
#else
    Q_UNUSED(window);
#endif // Q_OS_WIN
}

#ifdef Q_OS_WIN
bool WidgetResizeHandler::isInterestingNativeEvent(unsigned int nativeEvent)
{
    switch (nativeEvent) {
    case WM_NCHITTEST:
    case WM_NCCALCSIZE:
    case WM_NCLBUTTONDBLCLK:
    case WM_GETMINMAXINFO:
        return true;
    default:
        return false;
    }
}
#endif

#if defined(Q_OS_WIN) && defined(KDDOCKWIDGETS_QTWIDGETS)
bool NCHITTESTEventFilter::nativeEventFilter(const QByteArray &eventType, void *message,
                                             Qt5Qt6Compat::qintptr *result)

{
    if (eventType != "windows_generic_MSG" || !m_floatingWindow)
        return false;

    auto msg = static_cast<MSG *>(message);
    if (msg->message != WM_NCHITTEST)
        return false;
    const WId wid = WId(msg->hwnd);

    QWidget *child = QWidget::find(wid);
    if (!child || child->window() != m_floatingWindow)
        return false;
    const bool isThisWindow = child == m_floatingWindow;

    if (!isThisWindow) {
        *result = HTTRANSPARENT;
        return true;
    }

    return false;
}
#endif


CustomFrameHelper::CustomFrameHelper(ShouldUseCustomFrame func, QObject *parent)
    : QObject(parent)
    , QAbstractNativeEventFilter()
    , m_shouldUseCustomFrameFunc(func)
{
#ifdef Q_OS_WIN
    qApp->installNativeEventFilter(this);
#endif
}

CustomFrameHelper::~CustomFrameHelper()
{
    m_inDtor = true;
}

void CustomFrameHelper::applyCustomFrame(QWindow *window)
{
#ifdef Q_OS_WIN
    WidgetResizeHandler::setupWindow(window);
#else
    Q_UNUSED(window);
    qWarning() << Q_FUNC_INFO << "Not implemented on this platform";
#endif
}

bool CustomFrameHelper::nativeEventFilter(const QByteArray &eventType, void *message,
                                          Qt5Qt6Compat::qintptr *result)
{
    if (m_shouldUseCustomFrameFunc == nullptr || m_recursionGuard)
        return false;

    QScopedValueRollback<bool> guard(m_recursionGuard, true);

#ifdef Q_OS_WIN
    if (m_inDtor || !KDDockWidgets::usesAeroSnapWithCustomDecos())
        return false;

    if (eventType != "windows_generic_MSG")
        return false;

    auto msg = static_cast<MSG *>(message);
    if (!WidgetResizeHandler::isInterestingNativeEvent(msg->message)) {
        // Save some CPU cycles
        return false;
    }

    QWindow *window = DockRegistry::self()->windowForHandle(WId(msg->hwnd));
    if (!window)
        return false;

    const WidgetResizeHandler::NativeFeatures features = m_shouldUseCustomFrameFunc(window);
    if (!features.hasFeatures()) {
        // No native support for is desired for this window
        return false;
    }

    const char *propertyName = "kddw_customframe_setup_ran";
    const bool setupRan = window->property(propertyName).toBool();
    if (!setupRan) {
        // Add drop shadow
        WidgetResizeHandler::setupWindow(window);
        window->setProperty(propertyName, true);
    }

    return WidgetResizeHandler::handleWindowsNativeEvent(window, msg, result, features);
#else
    Q_UNUSED(eventType);
    Q_UNUSED(message);
    Q_UNUSED(result);
    return false;
#endif
}
