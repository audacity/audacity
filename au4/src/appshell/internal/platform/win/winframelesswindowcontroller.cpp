/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "winframelesswindowcontroller.h"

#if defined(_WIN32_WINNT) && (_WIN32_WINNT < 0x600)
#undef _WIN32_WINNT // like defined to `0x502` in _mingw.h for Qt 5.15
#define _WIN32_WINNT 0x0600 // Vista or later, needed for `iPaddedBorderWidth`
#endif
#include <Windows.h>
#include <windowsx.h>
#include <dwmapi.h>

#include <QScreen>
#include <QApplication>

#include "log.h"

using namespace au::appshell;

static HWND s_hwnd = 0;

static void updateWindowPosition()
{
    SetWindowPos(s_hwnd, nullptr, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE);
}

WinFramelessWindowController::WinFramelessWindowController()
    : FramelessWindowController()
{
    memset(&m_monitorInfo, 0, sizeof(MONITORINFO));
    m_monitorInfo.cbSize = sizeof(MONITORINFO);

    qApp->installEventFilter(this);
    qApp->installNativeEventFilter(this);
}

void WinFramelessWindowController::init()
{
    QWindow* window = mainWindow()->qWindow();
    IF_ASSERT_FAILED(window) {
        return;
    }

    s_hwnd = (HWND)window->winId();

    SetWindowLongPtr(s_hwnd, GWL_STYLE,
                     static_cast<LONG>(WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_THICKFRAME
                                       | WS_CLIPCHILDREN));

    const MARGINS shadow_on = { 1, 1, 1, 1 };
    DwmExtendFrameIntoClientArea(s_hwnd, &shadow_on);

    updateWindowPosition();
}

bool WinFramelessWindowController::eventFilter(QObject* watched, QEvent* event)
{
    if (event->type() != QEvent::Move || !watched->isWindowType()) {
        return false;
    }

    QWindow* window = dynamic_cast<QWindow*>(watched);
    if (!window) {
        return false;
    }

    if (!m_screen) {
        m_screen = window->screen();
        return false;
    }

    if (m_screen != window->screen()) {
        m_screen = window->screen();
        //! Redrawing a window by updating a position
        updateWindowPosition();
    }

    return false;
}

#ifdef MU_QT5_COMPAT
bool WinFramelessWindowController::nativeEventFilter(const QByteArray& eventType, void* message, long* result)
#else
bool WinFramelessWindowController::nativeEventFilter(const QByteArray& eventType, void* message, qintptr* result)
#endif
{
    if (eventType != "windows_generic_MSG") {
        return false;
    }

    MSG* msg = static_cast<MSG*>(message);
    if (msg == nullptr) {
        return false;
    }

    if (msg->hwnd != s_hwnd) {
        return false;
    }

    switch (msg->message) {
    case WM_NCCALCSIZE: {
        return removeWindowFrame(msg, result);
    }
    case WM_GETMINMAXINFO: {
        return calculateWindowSize(msg, result);
    }
    case WM_NCHITTEST: {
        return processMouseMove(msg, result);
    }
    case WM_NCRBUTTONDOWN: {
        return processMouseRightClick(msg);
    }
    default:
        break;
    }

    return false;
}

#ifdef MU_QT5_COMPAT
bool WinFramelessWindowController::removeWindowFrame(MSG* message, long* result)
#else
bool WinFramelessWindowController::removeWindowFrame(MSG* message, qintptr* result)
#endif
{
    NCCALCSIZE_PARAMS& params = *reinterpret_cast<NCCALCSIZE_PARAMS*>(message->lParam);

    WINDOWPLACEMENT placement = {};
    placement.length = sizeof(WINDOWPLACEMENT);
    GetWindowPlacement(s_hwnd, &placement);

    if (placement.showCmd == SW_SHOWMAXIMIZED) {
        HMONITOR hMonitor = MonitorFromWindow(message->hwnd, MONITOR_DEFAULTTONULL);
        if (hMonitor != NULL) {
            GetMonitorInfoW(hMonitor, &m_monitorInfo);
        }

        params.rgrc[0] = m_monitorInfo.rcWork;

        if (isTaskbarInAutohideState()) {
            std::optional<UINT> edge = taskbarEdge();
            if (edge.has_value()) {
                if (ABE_LEFT == edge.value()) {
                    params.rgrc[0].left += 1;
                } else if (ABE_RIGHT == edge.value()) {
                    params.rgrc[0].right -= 1;
                } else if (ABE_TOP == edge.value()) {
                    params.rgrc[0].top += 1;
                } else if (ABE_BOTTOM == edge.value()) {
                    params.rgrc[0].bottom -= 1;
                }
            }
        }
    }

    /// NOTE: remove window frame
    *result = WVR_REDRAW;
    return true;
}

#ifdef MU_QT5_COMPAT
bool WinFramelessWindowController::calculateWindowSize(MSG* message, long* result)
#else
bool WinFramelessWindowController::calculateWindowSize(MSG* message, qintptr* result)
#endif
{
    if (!isWindowMaximized(message->hwnd)) {
        return false;
    }

    RECT windowRect;
    if (!GetWindowRect(message->hwnd, &windowRect)) {
        return false;
    }

    HMONITOR hMonitor = MonitorFromRect(&windowRect, MONITOR_DEFAULTTONULL);
    if (!hMonitor) {
        return false;
    }

    GetMonitorInfoW(hMonitor, &m_monitorInfo);
    RECT monitorRect = m_monitorInfo.rcMonitor;
    RECT monitorWorkAreaRect = m_monitorInfo.rcWork;

    auto minMaxInfo = reinterpret_cast<MINMAXINFO*>(message->lParam);

    minMaxInfo->ptMaxSize.x = monitorWorkAreaRect.right - monitorWorkAreaRect.left;
    minMaxInfo->ptMaxSize.y =  monitorWorkAreaRect.bottom - monitorWorkAreaRect.top;
    minMaxInfo->ptMaxPosition.x = std::abs(windowRect.left - monitorRect.left);
    minMaxInfo->ptMaxPosition.y = std::abs(windowRect.top - monitorRect.top);
    minMaxInfo->ptMinTrackSize.x =  minMaxInfo->ptMaxSize.x;
    minMaxInfo->ptMinTrackSize.y =  minMaxInfo->ptMaxSize.y;

    *result = 0;
    return true;
}

#ifdef MU_QT5_COMPAT
bool WinFramelessWindowController::processMouseMove(MSG* message, long* result) const
#else
bool WinFramelessWindowController::processMouseMove(MSG* message, qintptr* result) const
#endif
{
    const LONG borderWidth = this->borderWidth();
    RECT windowRect;
    if (!GetWindowRect(message->hwnd, &windowRect)) {
        return false;
    }

    long x = GET_X_LPARAM(message->lParam);
    long y = GET_Y_LPARAM(message->lParam);

    double scaleFactor = uiConfiguration()->guiScaling();
    QRect moveAreaRect = windowTitleBarMoveArea();
    int moveAreaHeight = static_cast<int>(moveAreaRect.height() * scaleFactor);
    int moveAreaWidth = static_cast<int>(moveAreaRect.width() * scaleFactor);
    int moveAreaX = windowRect.left + static_cast<int>(moveAreaRect.x() * scaleFactor);
    int moveAreaY = windowRect.top + borderWidth + static_cast<int>(moveAreaRect.y() * scaleFactor);

    /// NOTE: titlebar`s move area
    if (x >= moveAreaX && x < moveAreaX + moveAreaWidth
        && y > moveAreaY && y < moveAreaY + moveAreaHeight) {
        *result = HTCAPTION;
        return true;
    }

    /// NOTE: bottom left corner
    if (x >= windowRect.left && x < windowRect.left + borderWidth
        && y < windowRect.bottom && y >= windowRect.bottom - borderWidth) {
        *result = HTBOTTOMLEFT;
        return true;
    }

    /// NOTE: bottom right corner
    if (x < windowRect.right && x >= windowRect.right - borderWidth
        && y < windowRect.bottom && y >= windowRect.bottom - borderWidth) {
        *result = HTBOTTOMRIGHT;
        return true;
    }

    /// NOTE: top left corner
    if (x >= windowRect.left && x < windowRect.left + borderWidth
        && y >= windowRect.top && y < windowRect.top + borderWidth) {
        *result = HTTOPLEFT;
        return true;
    }

    /// NOTE: top right corner
    if (x < windowRect.right && x >= windowRect.right - borderWidth
        && y >= windowRect.top && y < windowRect.top + borderWidth) {
        *result = HTTOPRIGHT;
        return true;
    }

    /// NOTE: left border
    if (x >= windowRect.left && x < windowRect.left + borderWidth) {
        *result = HTLEFT;
        return true;
    }

    /// NOTE: right border
    if (x < windowRect.right && x >= windowRect.right - borderWidth) {
        *result = HTRIGHT;
        return true;
    }

    /// NOTE: bottom border
    if (y < windowRect.bottom && y >= windowRect.bottom - borderWidth) {
        *result = HTBOTTOM;
        return true;
    }

    /// NOTE: top border
    if (y >= windowRect.top && y < windowRect.top + borderWidth) {
        *result = HTTOP;
        return true;
    }

    return false;
}

bool WinFramelessWindowController::processMouseRightClick(MSG* message) const
{
    return showSystemMenuIfNeed(message);
}

void WinFramelessWindowController::updateContextMenuState(MSG* message) const
{
    HMENU menu = GetSystemMenu(message->hwnd, false);

    MENUITEMINFO menuItemInfo;
    menuItemInfo.cbSize = sizeof(MENUITEMINFO);
    menuItemInfo.fMask = MIIM_STATE;
    menuItemInfo.fType = 0;

    menuItemInfo.fState = MF_ENABLED;

    SetMenuItemInfo(menu, SC_RESTORE, FALSE, &menuItemInfo);
    SetMenuItemInfo(menu, SC_SIZE, FALSE, &menuItemInfo);
    SetMenuItemInfo(menu, SC_MOVE, FALSE, &menuItemInfo);
    SetMenuItemInfo(menu, SC_MAXIMIZE, FALSE, &menuItemInfo);
    SetMenuItemInfo(menu, SC_MINIMIZE, FALSE, &menuItemInfo);

    menuItemInfo.fState = MF_GRAYED;

    WINDOWPLACEMENT windowPlacement;
    GetWindowPlacement(s_hwnd, &windowPlacement);

    switch (windowPlacement.showCmd) {
    case SW_SHOWMAXIMIZED:
        SetMenuItemInfo(menu, SC_SIZE, FALSE, &menuItemInfo);
        SetMenuItemInfo(menu, SC_MOVE, FALSE, &menuItemInfo);
        SetMenuItemInfo(menu, SC_MAXIMIZE, FALSE, &menuItemInfo);
        SetMenuDefaultItem(menu, SC_CLOSE, FALSE);
        break;
    case SW_SHOWMINIMIZED:
        SetMenuItemInfo(menu, SC_MINIMIZE, FALSE, &menuItemInfo);
        SetMenuDefaultItem(menu, SC_RESTORE, FALSE);
        break;
    case SW_SHOWNORMAL:
        SetMenuItemInfo(menu, SC_RESTORE, FALSE, &menuItemInfo);
        SetMenuDefaultItem(menu, SC_CLOSE, FALSE);
        break;
    }
}

bool WinFramelessWindowController::showSystemMenuIfNeed(MSG* message) const
{
    updateContextMenuState(message);

    HMENU menu = GetSystemMenu(message->hwnd, false);

    long x = GET_X_LPARAM(message->lParam);
    long y = GET_Y_LPARAM(message->lParam);

    uint command = TrackPopupMenu(menu, TPM_LEFTBUTTON | TPM_RETURNCMD, x, y, 0, message->hwnd, nullptr);
    if (command == 0) {
        return false;
    }

    PostMessage(message->hwnd, WM_SYSCOMMAND, command, 0);
    return true;
}

bool WinFramelessWindowController::isWindowMaximized(HWND hWnd) const
{
    WINDOWPLACEMENT wp;
    wp.length = sizeof(WINDOWPLACEMENT);
    if (!GetWindowPlacement(hWnd, &wp)) {
        return false;
    }

    return wp.showCmd == SW_MAXIMIZE;
}

bool WinFramelessWindowController::isTaskbarInAutohideState() const
{
    APPBARDATA appBarData;
    appBarData.cbSize = sizeof(appBarData);

    UINT taskbarState = SHAppBarMessage(ABM_GETSTATE, &appBarData);

    return ABS_AUTOHIDE & taskbarState;
}

std::optional<UINT> WinFramelessWindowController::taskbarEdge() const
{
    APPBARDATA appBarData;
    appBarData.cbSize = sizeof(appBarData);

    appBarData.hWnd = FindWindow(L"Shell_TrayWnd", nullptr);
    if (!appBarData.hWnd) {
        return std::nullopt;
    }

    SHAppBarMessage(ABM_GETTASKBARPOS, &appBarData);
    return appBarData.uEdge;
}

int WinFramelessWindowController::borderWidth() const
{
    NONCLIENTMETRICS nonClientMetrics = {};
    nonClientMetrics.cbSize = sizeof(nonClientMetrics);
    SystemParametersInfo(SPI_GETNONCLIENTMETRICS, sizeof(nonClientMetrics), &nonClientMetrics, 0);

    int borderWidth = nonClientMetrics.iBorderWidth + nonClientMetrics.iPaddedBorderWidth + 2;
    return borderWidth;
}
