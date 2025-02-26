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
import Audacity.Preferences 1.0

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

        NumberFormatSection {
            id: numberFormatSection

            numberFormats: ["System (English)"] //! TODO AU4: implement formats model
            currentNumberFormatCode: "System (English)"

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 2

            onNumberFormatSelected: function(numberFormatCode) {
                preferencesModel.setNumberFormat(numberFormatCode)
            }
        }

        SeparatorLine { }

        TemporaryFilesSection {
            id: temporaryFilesSection

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 3

            temporaryPath: preferencesModel.temporaryDir

            onTemporaryFilesLocationChanged: function(path) {
                preferencesModel.setTemporaryDir(path)
            }
        }

        FreeSpaceSection {
            id: freeSpaceSection

            availableSpace: preferencesModel.availableSpace
        }
    }
}
