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

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import MuseScore.Preferences 1.0

import "internal"

PreferencesPage {
    id: root

    Component.onCompleted: {
        preferencesModel.load()
    }

    GeneralPreferencesModel {
        id: preferencesModel

        onReceivingUpdateForCurrentLanguage: function(current, total, status) {
            languagesSection.setUpdateProgress(current, total, status)
        }
    }

    Column {
        width: parent.width
        spacing: root.sectionsSpacing

        LanguagesSection {
            id: languagesSection

            languages: preferencesModel.languages
            currentLanguageCode: preferencesModel.currentLanguageCode
            isNeedRestart: preferencesModel.isNeedRestart

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1

            onLanguageSelected: function(languageCode) {
                preferencesModel.currentLanguageCode = languageCode
            }

            onCheckForUpdateRequested: {
                preferencesModel.checkUpdateForCurrentLanguage()
            }
        }

        SeparatorLine { }

        ProgramStartSection {
            startupModes: preferencesModel.startupModes
            scorePathFilter: preferencesModel.scorePathFilter()

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 2

            onCurrentStartupModesChanged: function(index) {
                preferencesModel.setCurrentStartupMode(index)
            }

            onStartupScorePathChanged: function(path) {
                preferencesModel.setStartupScorePath(path)
            }
        }

        /*
         * TODO: https://github.com/musescore/MuseScore/issues/9807
        KeyboardLayoutsSection {
            keyboardLayouts: preferencesModel.keyboardLayouts
            currentKeyboardLayout: preferencesModel.currentKeyboardLayout

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 2

            onKeyboardLayoutSelected: function(keyboardLayout) {
                preferencesModel.currentKeyboardLayout = keyboardLayout
            }
        }

        SeparatorLine { }
        */

        /*
         * TODO: https://github.com/musescore/MuseScore/issues/9807
        SeparatorLine { }

        RemoteControlSection {
            isOSCRemoteControl: preferencesModel.isOSCRemoteControl
            oscPort: preferencesModel.oscPort

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 4

            onRemoteControlChanged: function(control) {
                preferencesModel.isOSCRemoteControl = control
            }

            onPortChanged: function(port) {
                preferencesModel.oscPort = port
            }
        }*/
    }
}
