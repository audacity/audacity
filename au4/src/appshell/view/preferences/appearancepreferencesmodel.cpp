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

using namespace mu::appshell;
using namespace mu::notation;
using namespace mu::ui;

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
        emit foregroundColorChanged();
    });

    uiConfiguration()->fontChanged().onNotify(this, [this]() {
        emit currentFontIndexChanged();
        emit bodyTextSizeChanged();
    });

    engravingConfiguration()->scoreInversionChanged().onNotify(this, [this]() {
        emit invertScoreColorChanged();
        emit foregroundColorChanged();
    });

    notationConfiguration()->backgroundChanged().onNotify(this, [this]() {
        emit backgroundColorChanged();
        emit backgroundUseColorChanged();
        emit backgroundWallpaperPathChanged();
    });

    notationConfiguration()->foregroundChanged().onNotify(this, [this]() {
        emit foregroundColorChanged();
        emit foregroundUseColorChanged();
        emit foregroundWallpaperPathChanged();
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
    notationConfiguration()->resetBackground();
    notationConfiguration()->resetForeground();
}

void AppearancePreferencesModel::setNewColor(const QColor& newColor, ColorType colorType)
{
    switch (colorType) {
    case AccentColor:
        uiConfiguration()->setCurrentThemeStyleValue(ThemeStyleKey::ACCENT_COLOR, Val(newColor));
        break;
    case TextAndIconsColor:
        uiConfiguration()->setCurrentThemeStyleValue(ThemeStyleKey::FONT_PRIMARY_COLOR, Val(newColor));
        break;
    case DisabledColor:
        NOT_IMPLEMENTED;
        return;
    case BorderColor:
        uiConfiguration()->setCurrentThemeStyleValue(ThemeStyleKey::STROKE_COLOR, Val(newColor));
        break;
    }

    emit themesChanged();
}

QStringList AppearancePreferencesModel::allFonts() const
{
    return uiConfiguration()->possibleFontFamilies();
}

QStringList AppearancePreferencesModel::wallpaperPathFilter() const
{
    return { qtrc("appshell/preferences", "Images") + " (*.jpg *.jpeg *.png *.bmp *.tif *.tiff)",
             qtrc("appshell/preferences", "All") + " (*)" };
}

QString AppearancePreferencesModel::wallpapersDir() const
{
    return notationConfiguration()->wallpapersDefaultDirPath().toQString();
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

bool AppearancePreferencesModel::backgroundUseColor() const
{
    return notationConfiguration()->backgroundUseColor();
}

QColor AppearancePreferencesModel::backgroundColor() const
{
    return notationConfiguration()->backgroundColor();
}

QString AppearancePreferencesModel::backgroundWallpaperPath() const
{
    return notationConfiguration()->backgroundWallpaperPath().toQString();
}

bool AppearancePreferencesModel::foregroundUseColor() const
{
    return notationConfiguration()->foregroundUseColor();
}

QColor AppearancePreferencesModel::foregroundColor() const
{
    return notationConfiguration()->foregroundColor();
}

QString AppearancePreferencesModel::foregroundWallpaperPath() const
{
    return notationConfiguration()->foregroundWallpaperPath().toQString();
}

bool AppearancePreferencesModel::scoreInversionEnabled() const
{
    return engravingConfiguration()->scoreInversionEnabled();
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
    uiConfiguration()->setCurrentThemeStyleValue(ThemeStyleKey::ACCENT_COLOR, Val(color));
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

void AppearancePreferencesModel::setBackgroundUseColor(bool value)
{
    if (value == backgroundUseColor()) {
        return;
    }

    notationConfiguration()->setBackgroundUseColor(value);
    emit backgroundUseColorChanged();
}

void AppearancePreferencesModel::setBackgroundColor(const QColor& color)
{
    if (color == backgroundColor()) {
        return;
    }

    notationConfiguration()->setBackgroundColor(color);
    emit backgroundColorChanged();
}

void AppearancePreferencesModel::setBackgroundWallpaperPath(const QString& path)
{
    if (path == backgroundWallpaperPath()) {
        return;
    }

    notationConfiguration()->setBackgroundWallpaperPath(path);
    emit backgroundWallpaperPathChanged();
}

void AppearancePreferencesModel::setForegroundUseColor(bool value)
{
    if (value == foregroundUseColor()) {
        return;
    }

    notationConfiguration()->setForegroundUseColor(value);
    emit foregroundUseColorChanged();
}

void AppearancePreferencesModel::setForegroundColor(const QColor& color)
{
    if (color == foregroundColor()) {
        return;
    }

    notationConfiguration()->setForegroundColor(color);
    emit foregroundColorChanged();
}

void AppearancePreferencesModel::setForegroundWallpaperPath(const QString& path)
{
    if (path == foregroundWallpaperPath()) {
        return;
    }

    notationConfiguration()->setForegroundWallpaperPath(path);
    emit foregroundWallpaperPathChanged();
}

void AppearancePreferencesModel::setScoreInversionEnabled(bool value)
{
    if (value == scoreInversionEnabled()) {
        return;
    }

    engravingConfiguration()->setScoreInversionEnabled(value);
    emit invertScoreColorChanged();
}
