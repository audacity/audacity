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

#include "appearancepreferencesmodel.h"

#include "ui/internal/themeconverter.h"

#include "log.h"
#include "translation.h"

using namespace au::appshell;
using namespace muse::ui;

static constexpr int INVALID_INDEX = -1;

AppearancePreferencesModel::AppearancePreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void AppearancePreferencesModel::init()
{
    uiConfiguration()->isFollowSystemTheme().notification.onNotify(this, [this]() {
        emit isFollowSystemThemeChanged();
    });

    uiConfiguration()->currentThemeChanged().onNotify(this, [this]() {
        emit themesChanged();
    });

    uiConfiguration()->fontChanged().onNotify(this, [this]() {
        emit currentFontIndexChanged();
        emit bodyTextSizeChanged();
    });
}

bool AppearancePreferencesModel::isFollowSystemThemeAvailable() const
{
    return uiConfiguration()->isFollowSystemThemeAvailable();
}

bool AppearancePreferencesModel::isFollowSystemTheme() const
{
    return uiConfiguration()->isFollowSystemTheme().val;
}

void AppearancePreferencesModel::setFollowSystemTheme(bool enabled)
{
    if (enabled == isFollowSystemTheme()) {
        return;
    }

    uiConfiguration()->setFollowSystemTheme(enabled);
}

bool AppearancePreferencesModel::highContrastEnabled() const
{
    return uiConfiguration()->isHighContrast();
}

QVariantList AppearancePreferencesModel::generalThemes() const
{
    QVariantList result;

    for (const ThemeInfo& theme: allThemes()) {
        if (theme.codeKey == LIGHT_THEME_CODE || theme.codeKey == DARK_THEME_CODE) {
            result << ThemeConverter::toMap(theme);
        }
    }

    return result;
}

QVariantList AppearancePreferencesModel::highContrastThemes() const
{
    QVariantList result;

    for (const ThemeInfo& theme : allThemes()) {
        if (theme.codeKey == HIGH_CONTRAST_WHITE_THEME_CODE || theme.codeKey == HIGH_CONTRAST_BLACK_THEME_CODE) {
            result << ThemeConverter::toMap(theme);
        }
    }

    return result;
}

QStringList AppearancePreferencesModel::accentColors() const
{
    return uiConfiguration()->possibleAccentColors();
}

void AppearancePreferencesModel::resetAppearancePreferencesToDefault()
{
    uiConfiguration()->resetThemes();
    uiConfiguration()->resetFonts();
}

void AppearancePreferencesModel::setNewColor(const QColor& newColor, ColorType colorType)
{
    switch (colorType) {
    case AccentColor:
        uiConfiguration()->setCurrentThemeStyleValue(ThemeStyleKey::ACCENT_COLOR, muse::Val(newColor));
        break;
    case TextAndIconsColor:
        uiConfiguration()->setCurrentThemeStyleValue(ThemeStyleKey::FONT_PRIMARY_COLOR, muse::Val(newColor));
        break;
    case DisabledColor:
        NOT_IMPLEMENTED;
        return;
    case BorderColor:
        uiConfiguration()->setCurrentThemeStyleValue(ThemeStyleKey::STROKE_COLOR, muse::Val(newColor));
        break;
    }

    emit themesChanged();
}

QStringList AppearancePreferencesModel::allFonts() const
{
    return uiConfiguration()->possibleFontFamilies();
}

void AppearancePreferencesModel::setHighContrastEnabled(bool enabled)
{
    if (highContrastEnabled() == enabled) {
        return;
    }

    uiConfiguration()->setIsHighContrast(enabled);
}

QString AppearancePreferencesModel::currentThemeCode() const
{
    return QString::fromStdString(currentTheme().codeKey);
}

int AppearancePreferencesModel::currentAccentColorIndex() const
{
    QStringList allColors = accentColors();
    QString color = currentTheme().values[ACCENT_COLOR].toString().toLower();

    for (int i = 0; i < static_cast<int>(allColors.size()); ++i) {
        if (allColors[i].toLower() == color) {
            return i;
        }
    }

    return INVALID_INDEX;
}

ThemeInfo AppearancePreferencesModel::currentTheme() const
{
    return uiConfiguration()->currentTheme();
}

ThemeList AppearancePreferencesModel::allThemes() const
{
    return uiConfiguration()->themes();
}

int AppearancePreferencesModel::currentFontIndex() const
{
    QString currentFont = QString::fromStdString(uiConfiguration()->fontFamily());
    return allFonts().indexOf(currentFont);
}

int AppearancePreferencesModel::bodyTextSize() const
{
    return uiConfiguration()->fontSize(FontSizeType::BODY);
}

void AppearancePreferencesModel::setCurrentThemeCode(const QString& themeCode)
{
    if (themeCode == currentThemeCode() && !isFollowSystemTheme()) {
        return;
    }

    uiConfiguration()->setCurrentTheme(themeCodeFromString(themeCode));
}

void AppearancePreferencesModel::setCurrentAccentColorIndex(int index)
{
    if (index < 0 || index >= accentColors().size()) {
        return;
    }

    if (index == currentAccentColorIndex()) {
        return;
    }

    QColor color = accentColors()[index];
    uiConfiguration()->setCurrentThemeStyleValue(ThemeStyleKey::ACCENT_COLOR, muse::Val(color));
}

void AppearancePreferencesModel::setCurrentFontIndex(int index)
{
    QStringList fonts = allFonts();

    if (index < 0 || index >= fonts.size()) {
        return;
    }

    uiConfiguration()->setFontFamily(fonts[index].toStdString());
    emit currentFontIndexChanged();
}

void AppearancePreferencesModel::setBodyTextSize(int size)
{
    if (size == bodyTextSize() || size <= 0) {
        return;
    }

    uiConfiguration()->setBodyFontSize(size);
    emit bodyTextSizeChanged();
}
