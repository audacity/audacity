/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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

#include <QtDBus/QDBusConnection>
#include <QtDBus/QDBusConnectionInterface>

#include "linuxplatformtheme.h"

using namespace mu::ui;
using namespace mu::async;

static const QLatin1String service("org.freedesktop.portal.Desktop");
static const QLatin1String path("/org/freedesktop/portal/desktop");
static const QLatin1String interface("org.freedesktop.portal.Settings");
static const QLatin1String settingGroup("org.freedesktop.appearance");
static const QLatin1String settingSignal("SettingChanged");
static const QLatin1String colorSchemeSetting("color-scheme");

LinuxPlatformTheme::LinuxPlatformTheme()
{
}

void LinuxPlatformTheme::startListening()
{
    QDBusConnection::sessionBus().connect(service, path, interface, settingSignal, this,
                                          SLOT(processSettingChange(QString,QString,QDBusVariant)));
    m_isSystemThemeDark = isSystemThemeCurrentlyDark();
    m_isListening = true;
}

void LinuxPlatformTheme::stopListening()
{
    QDBusConnection::sessionBus().disconnect(service, path, interface, settingSignal, this,
                                             SLOT(processSettingChange(QString,QString,QDBusVariant)));
    m_isListening = false;
}

bool LinuxPlatformTheme::isFollowSystemThemeAvailable() const
{
    const QDBusConnection connection = QDBusConnection::sessionBus();

    const auto iface = connection.interface();
    if (!iface->isServiceRegistered(service)) {
        return false;
    }

    QDBusReply<QVariant> reply = getSystemTheme();
    return reply.isValid();
}

bool LinuxPlatformTheme::isSystemThemeDark() const
{
    if (m_isListening) {
        return m_isSystemThemeDark;
    }

    return isSystemThemeCurrentlyDark();
}

bool LinuxPlatformTheme::isGlobalMenuAvailable() const
{
    const QDBusConnection connection = QDBusConnection::sessionBus();
    static const QString registrarService = QStringLiteral("com.canonical.AppMenu.Registrar");
    if (const auto iface = connection.interface()) {
        return iface->isServiceRegistered(registrarService);
    }
    return false;
}

Notification LinuxPlatformTheme::platformThemeChanged() const
{
    return m_platformThemeChanged;
}

void LinuxPlatformTheme::applyPlatformStyleOnAppForTheme(const ThemeCode&)
{
}

void LinuxPlatformTheme::applyPlatformStyleOnWindowForTheme(QWindow*, const ThemeCode&)
{
}

void LinuxPlatformTheme::processSettingChange(const QString& group, const QString& key, const QDBusVariant& value)
{
    if (group == settingGroup && key == colorSchemeSetting) {
        m_isSystemThemeDark = (value.variant().toUInt() == 1);
        m_platformThemeChanged.notify();
    }
}

bool LinuxPlatformTheme::isSystemThemeCurrentlyDark() const
{
    QDBusReply<QVariant> reply = getSystemTheme();

    if (reply.isValid()) {
        const QDBusVariant dbusVariant = qvariant_cast<QDBusVariant>(reply.value());
        return dbusVariant.variant().toUInt() == 1;
    }

    return false;
}

QDBusReply<QVariant> LinuxPlatformTheme::getSystemTheme() const
{
    auto message = QDBusMessage::createMethodCall(service, path, interface, QStringLiteral("Read"));
    message << settingGroup << colorSchemeSetting;

    return QDBusConnection::sessionBus().call(message);
}
