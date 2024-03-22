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
import QtQuick 2.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0
import MuseScore.Preferences 1.0

import "internal"

PreferencesPage {
    id: root

    AppearancePreferencesModel {
        id: appearanceModel
    }

    Component.onCompleted: {
        appearanceModel.init()
    }

    Column {
        width: parent.width
        spacing: root.sectionsSpacing

        ThemesSection {
            width: parent.width

            themes: appearanceModel.highContrastEnabled ? appearanceModel.highContrastThemes : appearanceModel.generalThemes
            currentThemeCode: appearanceModel.currentThemeCode
            highContrastEnabled: appearanceModel.highContrastEnabled
            isFollowSystemThemeAvailable: appearanceModel.isFollowSystemThemeAvailable
            isFollowSystemTheme: appearanceModel.isFollowSystemTheme
            accentColors: appearanceModel.accentColors
            currentAccentColorIndex: appearanceModel.currentAccentColorIndex

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1

            onThemeChangeRequested: function(newThemeCode) {
                appearanceModel.currentThemeCode = newThemeCode
            }

            onHighContrastChangeRequested: function(enabled) {
                appearanceModel.highContrastEnabled = enabled
            }

            onSetFollowSystemThemeRequested: function(enabled) {
                appearanceModel.isFollowSystemTheme = enabled
            }

            onAccentColorChangeRequested: function(newColorIndex) {
                appearanceModel.currentAccentColorIndex = newColorIndex
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }

            onEnsureContentVisibleRequested: function(contentRect) {
                root.ensureContentVisibleRequested(contentRect)
            }
        }

        SeparatorLine {
            visible: uiColorsSection.visible
        }

        UiColorsSection {
            id: uiColorsSection

            width: parent.width

            visible: appearanceModel.highContrastEnabled

            navigation.section: root.navigationSection
            //! NOTE: 3 because ThemesSection have two panels
            navigation.order: root.navigationOrderStart + 3

            onColorChangeRequested: function(newColor, propertyType) {
                appearanceModel.setNewColor(newColor, propertyType)
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine {}

        UiFontSection {
            allFonts: appearanceModel.allFonts()
            currentFontIndex: appearanceModel.currentFontIndex
            bodyTextSize: appearanceModel.bodyTextSize

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 4

            onFontChangeRequested: function(newFontIndex) {
                appearanceModel.currentFontIndex = newFontIndex
            }

            onBodyTextSizeChangeRequested: function(newBodyTextSize) {
                appearanceModel.bodyTextSize = newBodyTextSize
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine {}

        ColorAndWallpaperSection {
            id: backgroundSettings

            width: parent.width

            title: qsTrc("appshell/preferences", "Background")
            wallpaperDialogTitle: qsTrc("appshell/preferences", "Choose background wallpaper")
            useColor: appearanceModel.backgroundUseColor
            color: appearanceModel.backgroundColor
            wallpaperPath: appearanceModel.backgroundWallpaperPath
            wallpapersDir: appearanceModel.wallpapersDir()
            wallpaperFilter: appearanceModel.wallpaperPathFilter()

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 5

            onUseColorChangeRequested: function(newValue) {
                appearanceModel.backgroundUseColor = newValue
            }

            onColorChangeRequested: function(newColor) {
                appearanceModel.backgroundColor = newColor
            }

            onWallpaperPathChangeRequested: function(newWallpaperPath) {
                appearanceModel.backgroundWallpaperPath = newWallpaperPath
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine {}

        ColorAndWallpaperSection {
            id: paperSettings

            width: parent.width

            enabled: !appearanceModel.scoreInversionEnabled
            opacityOverride: paperSettings.enabled ? 1.0 : 0.6

            title: qsTrc("appshell/preferences", "Paper")
            wallpaperDialogTitle: qsTrc("appshell/preferences", "Choose notepaper")
            useColor: appearanceModel.foregroundUseColor
            color: appearanceModel.foregroundColor
            wallpaperPath: appearanceModel.foregroundWallpaperPath
            wallpapersDir: appearanceModel.wallpapersDir()
            wallpaperFilter: appearanceModel.wallpaperPathFilter()

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 6

            onUseColorChangeRequested: function(newValue) {
                appearanceModel.foregroundUseColor = newValue
            }

            onColorChangeRequested: function(newColor) {
                appearanceModel.foregroundColor = newColor
            }

            onWallpaperPathChangeRequested: function(newWallpaperPath) {
                appearanceModel.foregroundWallpaperPath = newWallpaperPath
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine {}

        ThemeAdditionalOptionsSection {
            scoreInversionEnabled: appearanceModel.scoreInversionEnabled

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 7

            onResetThemeToDefaultRequested: {
                appearanceModel.resetAppearancePreferencesToDefault()
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }

            onScoreInversionEnableChangeRequested: function(enable) {
                appearanceModel.scoreInversionEnabled = enable
            }
        }
    }
}
