/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2022 MuseScore BVBA and others
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

#include "windowsplatformtheme.h"
#include "log.h"

#include <Windows.h>

using namespace mu::ui;
using namespace mu::async;

static const std::wstring windowsThemesKey = L"Software\\Microsoft\\Windows\\CurrentVersion\\Themes";
static const std::wstring windowsThemesPersonalizeKey = L"Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize";
static const std::wstring windowsThemesPersonalizeSubkey = L"AppsUseLightTheme";

HKEY hKey = nullptr;

HANDLE hThemeChangeEvent = nullptr;
HANDLE hStopListeningEvent = nullptr;

WindowsPlatformTheme::WindowsPlatformTheme()
{
    using fnRtlGetNtVersionNumbers = void(WINAPI*)(LPDWORD major, LPDWORD minor, LPDWORD build);

    // The (void*) cast silences an unnecessary MinGW warning about incompatible function cast
    auto rtlGetNtVersionNumbers
        = reinterpret_cast<fnRtlGetNtVersionNumbers>((void*)GetProcAddress(GetModuleHandleW(L"ntdll.dll"), "RtlGetNtVersionNumbers"));
    if (rtlGetNtVersionNumbers) {
        DWORD buildNumber;
        rtlGetNtVersionNumbers(NULL, NULL, &buildNumber);
        m_buildNumber = buildNumber & ~0xF0000000;
    } else {
        LOGE() << "Could not get Windows build number";
    }
}

void WindowsPlatformTheme::startListening()
{
    if (m_isListening) {
        return;
    }

    m_isListening = true;
    m_isSystemThemeDark.val = isSystemThemeCurrentlyDark();

    if (RegOpenKeyExW(HKEY_CURRENT_USER, windowsThemesKey.c_str(), 0,
                      KEY_NOTIFY | KEY_CREATE_SUB_KEY | KEY_ENUMERATE_SUB_KEYS | KEY_QUERY_VALUE | KEY_WOW64_64KEY,
                      &hKey) == ERROR_SUCCESS) {
        m_listenThread = std::thread([this]() { th_listen(); });
    } else {
        LOGW() << "Failed opening key for listening dark theme changes.";
    }
}

void WindowsPlatformTheme::stopListening()
{
    if (!m_isListening) {
        return;
    }

    m_isListening = false;

    // The following _might_ fail; in that case, the app won't respond
    // for max. 4000 ms (or whatever value is specified in th_listen()).
    SetEvent(hStopListeningEvent);
    m_listenThread.join();
}

bool WindowsPlatformTheme::isFollowSystemThemeAvailable() const
{
    return m_buildNumber >= 17763; // Dark theme was introduced in Windows 1809
}

bool WindowsPlatformTheme::isSystemThemeDark() const
{
    if (m_isListening) {
        return m_isSystemThemeDark.val;
    }

    return isSystemThemeCurrentlyDark();
}

bool WindowsPlatformTheme::isGlobalMenuAvailable() const
{
    return false;
}

Notification WindowsPlatformTheme::platformThemeChanged() const
{
    return m_isSystemThemeDark.notification;
}

bool WindowsPlatformTheme::isSystemThemeCurrentlyDark() const
{
    DWORD data {};
    DWORD datasize = sizeof(data);

    if (RegGetValue(HKEY_CURRENT_USER, windowsThemesPersonalizeKey.c_str(), windowsThemesPersonalizeSubkey.c_str(),
                    RRF_RT_REG_DWORD, nullptr, &data, &datasize) == ERROR_SUCCESS) {
        return data == 0;
    }

    return false;
}

void WindowsPlatformTheme::th_listen()
{
    static const DWORD dwFilter = REG_NOTIFY_CHANGE_NAME
                                  | REG_NOTIFY_CHANGE_ATTRIBUTES
                                  | REG_NOTIFY_CHANGE_LAST_SET
                                  | REG_NOTIFY_CHANGE_SECURITY;

    while (m_isListening) {
        hStopListeningEvent = CreateEvent(NULL, FALSE, TRUE, TEXT("StopListening"));
        hThemeChangeEvent = CreateEvent(NULL, FALSE, TRUE, TEXT("ThemeSettingChange"));

        if (RegNotifyChangeKeyValue(hKey, TRUE, dwFilter, hThemeChangeEvent, TRUE) == ERROR_SUCCESS) {
            HANDLE handles[2] = { hStopListeningEvent, hThemeChangeEvent };
            DWORD dwRet = WaitForMultipleObjects(2, handles, FALSE, 4000);

            if (dwRet != WAIT_TIMEOUT && dwRet != WAIT_FAILED) {
                if (m_isListening) {
                    //! NOTE There might be some delay before `isSystemHighContrast` returns the correct value
                    Sleep(100);

                    bool newIsDark = isSystemThemeCurrentlyDark();
                    if (newIsDark != m_isSystemThemeDark.val) {
                        m_isSystemThemeDark.set(newIsDark);
                    }
                }
                // Else, the received event must have been a stop event
            }
        } else {
            LOGD() << "Failed registering for dark theme change notifications.";
        }
    }

    RegCloseKey(hKey);
}

void WindowsPlatformTheme::applyPlatformStyleOnAppForTheme(const ThemeCode&)
{
}

void WindowsPlatformTheme::applyPlatformStyleOnWindowForTheme(QWindow*, const ThemeCode&)
{
}
