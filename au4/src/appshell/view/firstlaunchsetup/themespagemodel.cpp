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
#include "themespagemodel.h"

#include "ui/internal/themeconverter.h"

using namespace au::appshell;
using namespace muse::ui;

ThemesPageModel::ThemesPageModel(QObject* parent)
    : QObject(parent)
{
}

void ThemesPageModel::load()
{
    uiConfiguration()->isFollowSystemTheme().notification.onNotify(this, [this]() {
        emit isFollowSystemThemeChanged();
    });

    uiConfiguration()->currentThemeChanged().onNotify(this, [this]() {
        emit themesChanged();
    });
}

bool ThemesPageModel::isFollowSystemThemeAvailable() const
{
    return uiConfiguration()->isFollowSystemThemeAvailable();
}

bool ThemesPageModel::isFollowSystemTheme() const
{
    return uiConfiguration()->isFollowSystemTheme().val;
}

void ThemesPageModel::setFollowSystemTheme(bool enabled)
{
    if (enabled == isFollowSystemTheme()) {
        return;
    }

    uiConfiguration()->setFollowSystemTheme(enabled);
}

ThemeList ThemesPageModel::allThemes() const
{
    return uiConfiguration()->themes();
}

QVariantList ThemesPageModel::generalThemes() const
{
    QVariantList result;

    for (const ThemeInfo& theme: allThemes()) {
        if (theme.codeKey == LIGHT_THEME_CODE || theme.codeKey == DARK_THEME_CODE) {
            result << ThemeConverter::toMap(theme);
        }
    }

    return result;
}

QVariantList ThemesPageModel::highContrastThemes() const
{
    QVariantList result;

    for (const ThemeInfo& theme : allThemes()) {
        if (theme.codeKey == HIGH_CONTRAST_WHITE_THEME_CODE || theme.codeKey == HIGH_CONTRAST_BLACK_THEME_CODE) {
            result << ThemeConverter::toMap(theme);
        }
    }

    return result;
}

bool ThemesPageModel::highContrastEnabled() const
{
    return uiConfiguration()->isHighContrast();
}

void ThemesPageModel::setHighContrastEnabled(bool enabled)
{
    if (highContrastEnabled() == enabled) {
        return;
    }

    uiConfiguration()->setIsHighContrast(enabled);
    emit highContrastEnabledChanged();
}

ThemeInfo ThemesPageModel::currentTheme() const
{
    return uiConfiguration()->currentTheme();
}

QString ThemesPageModel::currentThemeCode() const
{
    return QString::fromStdString(currentTheme().codeKey);
}

void ThemesPageModel::setCurrentThemeCode(const QString& themeCode)
{
    if (themeCode == currentThemeCode() && !isFollowSystemTheme()) {
        return;
    }

    uiConfiguration()->setCurrentTheme(themeCodeFromString(themeCode));
}

QStringList ThemesPageModel::accentColors() const
{
    return uiConfiguration()->possibleAccentColors();
}

int ThemesPageModel::currentAccentColorIndex() const
{
    QStringList allColors = accentColors();
    QString color = currentTheme().values[ACCENT_COLOR].toString().toLower();

    for (int i = 0; i < static_cast<int>(allColors.size()); ++i) {
        if (allColors[i].toLower() == color) {
            return i;
        }
    }

    return -1;
}

void ThemesPageModel::setCurrentAccentColorIndex(int index)
{
    if (index < 0 || index >= accentColors().size()) {
        return;
    }

    if (index == currentAccentColorIndex()) {
        return;
    }

    QColor color = accentColors().at(index);
    uiConfiguration()->setCurrentThemeStyleValue(ThemeStyleKey::ACCENT_COLOR, muse::Val(color));
}
