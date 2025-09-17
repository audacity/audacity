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

void ThemesPageModel::setFollowSystemTheme(const bool enabled)
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

void ThemesPageModel::setHighContrastEnabled(const bool enabled)
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
    const QString color = currentTheme().values[ACCENT_COLOR].toString().toLower();

    for (int i = 0; i < static_cast<int>(allColors.size()); ++i) {
        if (allColors[i].toLower() == color) {
            return i;
        }
    }

    return -1;
}

void ThemesPageModel::setCurrentAccentColorIndex(const int index)
{
    if (index < 0 || index >= accentColors().size()) {
        return;
    }

    if (index == currentAccentColorIndex()) {
        return;
    }

    const QColor color = accentColors().at(index);
    uiConfiguration()->setCurrentThemeStyleValue(ThemeStyleKey::ACCENT_COLOR, muse::Val(color));
}

QString ThemesPageModel::pageTitle()
{
    return muse::qtrc("appshell/gettingstarted", "Select a theme");
}

QString ThemesPageModel::pageDescription()
{
    return muse::qtrc("appshell/gettingstarted", "Choose your preferred theme, follow system theme option, and accent color");
}

QString ThemesPageModel::checkboxesPanelAccessibleName()
{
    return muse::qtrc("appshell/gettingstarted", "Theme options");
}

QString ThemesPageModel::checkboxesPanelAccessibleDescription()
{
    return muse::qtrc("appshell/gettingstarted", "Additional theme configuration options");
}

QString ThemesPageModel::themeSelectionAccessibleName()
{
    return muse::qtrc("appshell/gettingstarted", "Theme selection");
}

QString ThemesPageModel::themeSelectionAccessibleDescription()
{
    return muse::qtrc("appshell/gettingstarted", "Choose between light, dark, or system theme");
}

QString ThemesPageModel::followSystemThemeText()
{
    return muse::qtrc("appshell/gettingstarted", "Follow system theme");
}

QString ThemesPageModel::followSystemThemeDescription()
{
    return muse::qtrc("appshell/gettingstarted", "When enabled, the theme will automatically match your system's theme setting");
}

QString ThemesPageModel::enableHighContrastText()
{
    return muse::qtrc("appshell/gettingstarted", "Enable high-contrast");
}

QString ThemesPageModel::enableHighContrastDescription() const
{
    //: here %1 represent the written text for the high contrast preferences hint "Further options for high contrast mode can be found in Preferences"
    return muse::qtrc("appshell/gettingstarted", "Enable high contrast mode for better visibility. %1").arg(highContrastPreferencesHint());
}

QString ThemesPageModel::accentColorText()
{
    return muse::qtrc("project", "Accent color");
}

QString ThemesPageModel::accentColorDescription()
{
    return muse::qtrc("appshell/gettingstarted", "Choose an accent color for the interface");
}

QString ThemesPageModel::highContrastPreferencesHint()
{
    return muse::qtrc("appshell/gettingstarted", "Further options for high contrast mode can be found in Preferences");
}

QString ThemesPageModel::themeConfigurationText()
{
    return muse::qtrc("appshell/gettingstarted", "Theme configuration");
}

QString ThemesPageModel::formatThemeConfigurationDescription() const
{
    QString themeName;
    if (currentThemeCode() == "light") {
        themeName = muse::qtrc("appshell/gettingstarted", "Light");
    } else if (currentThemeCode() == "dark") {
        themeName = muse::qtrc("appshell/gettingstarted", "Dark");
    } else {
        themeName = muse::qtrc("appshell/gettingstarted", "System");
    }

    //: %1 is the current theme name (Light, Dark, or System)
    QString baseDesc = muse::qtrc("appshell/gettingstarted", "Current theme: %1").arg(themeName);

    if (isFollowSystemTheme()) {
        //: %1 is the base description with current theme
        baseDesc = muse::qtrc("appshell/gettingstarted", "%1. Following system theme").arg(baseDesc);
    }

    if (highContrastEnabled()) {
        //: %1 is the base description with current theme and system theme status
        baseDesc = muse::qtrc("appshell/gettingstarted", "%1. High contrast enabled").arg(baseDesc);
    } else {
        //: %1 is the base description with current theme and system theme status
        baseDesc = muse::qtrc("appshell/gettingstarted", "%1. Accent color selected").arg(baseDesc);
    }

    return baseDesc;
}
