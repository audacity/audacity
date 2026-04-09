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
import QtQuick

import Muse.Ui
import Muse.UiComponents
import Audacity.AppShell
import Audacity.Preferences

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

    UpdatePreferencesModel {
        id: updateModel
    }

    UsageInfoPreferencesModel {
        id: usageInfoModel
    }

    Column {
        width: parent.width
        spacing: root.sectionsSpacing

        LanguagesSection {
            id: languagesSection

            languages: preferencesModel.languages
            currentLanguageCode: preferencesModel.currentLanguageCode
            isNeedRestart: preferencesModel.restartRequired

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1

            onLanguageSelected: function(languageCode) {
                preferencesModel.currentLanguageCode = languageCode
            }

            onCheckForUpdateRequested: {
                preferencesModel.checkUpdateForCurrentLanguage()
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
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

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine { }

        AutomaticUpdateSection {
            isAppUpdatable: updateModel.isAppUpdatable()
            needCheckForNewAppVersion: updateModel.needCheckForNewAppVersion
            privacyPolicyUrl: updateModel.privacyPolicyUrl()

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 3

            onNeedCheckForNewAppVersionChangeRequested: function(check) {
                updateModel.needCheckForNewAppVersion = check
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine { }

        UsageInfoSection {
            sendAnonymousUsageInfo: usageInfoModel.sendAnonymousUsageInfo
            privacyPolicyUrl: updateModel.privacyPolicyUrl()

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 4

            onSendAnonymousUsageInfoChangeRequested: function(send) {
                usageInfoModel.sendAnonymousUsageInfo = send
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine { }

        TemporaryFilesSection {
            id: temporaryFilesSection

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 5

            temporaryPath: preferencesModel.temporaryDir

            onTemporaryFilesLocationChanged: function(path) {
                preferencesModel.setTemporaryDir(path)
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        FreeSpaceSection {
            id: freeSpaceSection

            availableSpace: preferencesModel.availableSpace
        }

        SeparatorLine { }

        FFmpegLibrarySection {
            id: ffmpegLibrarySection

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 6

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }
    }
}
