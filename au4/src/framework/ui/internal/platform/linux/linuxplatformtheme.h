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

#ifndef MU_UI_LINUXPLATFORMTHEME_H
#define MU_UI_LINUXPLATFORMTHEME_H

#include <QtDBus/QDBusReply>

#include "internal/iplatformtheme.h"

class QDBusVariant;

namespace mu::ui {
class LinuxPlatformTheme : public QObject, public IPlatformTheme
{
    Q_OBJECT
public:
    LinuxPlatformTheme();

    void startListening() override;
    void stopListening() override;

    bool isFollowSystemThemeAvailable() const override;

    bool isSystemThemeDark() const override;
    async::Notification platformThemeChanged() const override;

    bool isGlobalMenuAvailable() const override;

    void applyPlatformStyleOnAppForTheme(const ThemeCode& themeCode) override;
    void applyPlatformStyleOnWindowForTheme(QWindow* window, const ThemeCode& themeCode) override;

private slots:
    void processSettingChange(const QString& group, const QString& key, const QDBusVariant& value);

private:
    bool isSystemThemeCurrentlyDark() const;
    QDBusReply<QVariant> getSystemTheme() const;

    async::Notification m_platformThemeChanged;
    bool m_isListening = false;
    bool m_isSystemThemeDark = false;
};
}

#endif // MU_UI_LINUXPLATFORMTHEME_H
