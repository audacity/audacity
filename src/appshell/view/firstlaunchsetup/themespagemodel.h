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
#ifndef AU_APPSHELL_THEMESPAGEMODEL_H
#define AU_APPSHELL_THEMESPAGEMODEL_H

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

namespace au::appshell {
class ThemesPageModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(bool isFollowSystemThemeAvailable READ isFollowSystemThemeAvailable CONSTANT)
    Q_PROPERTY(bool isFollowSystemTheme READ isFollowSystemTheme WRITE setFollowSystemTheme NOTIFY isFollowSystemThemeChanged)

    Q_PROPERTY(bool highContrastEnabled READ highContrastEnabled WRITE setHighContrastEnabled NOTIFY highContrastEnabledChanged)
    Q_PROPERTY(QVariantList generalThemes READ generalThemes NOTIFY themesChanged)
    Q_PROPERTY(QVariantList highContrastThemes READ highContrastThemes NOTIFY themesChanged)
    Q_PROPERTY(QStringList accentColors READ accentColors NOTIFY themesChanged)

    Q_PROPERTY(QString currentThemeCode READ currentThemeCode WRITE setCurrentThemeCode NOTIFY themesChanged)
    Q_PROPERTY(int currentAccentColorIndex READ currentAccentColorIndex WRITE setCurrentAccentColorIndex NOTIFY themesChanged)

    INJECT(muse::ui::IUiConfiguration, uiConfiguration)

public:
    explicit ThemesPageModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    bool isFollowSystemThemeAvailable() const;
    bool isFollowSystemTheme() const;

    QVariantList generalThemes() const;
    QVariantList highContrastThemes() const;
    bool highContrastEnabled() const;
    QString currentThemeCode() const;

    QStringList accentColors() const;
    int currentAccentColorIndex() const;

public slots:
    void setFollowSystemTheme(bool enabled);
    void setHighContrastEnabled(bool enabled);
    void setCurrentThemeCode(const QString& themeCode);
    void setCurrentAccentColorIndex(int index);

signals:
    void isFollowSystemThemeChanged();
    void highContrastEnabledChanged();
    void themesChanged();

private:
    muse::ui::ThemeInfo currentTheme() const;
    muse::ui::ThemeList allThemes() const;
};
}

#endif // AU_APPSHELL_THEMESPAGEMODEL_H
