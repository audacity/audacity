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
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import MuseScore.Preferences 1.0

import "internal"

PreferencesPage {
    id: root

    contentFillsAvailableHeight: true

    Component.onCompleted: {
        preferencesModel.load()
    }

    AdvancedPreferencesModel {
        id: preferencesModel
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: root.sectionsSpacing

        AdvancedTopSection {
            id: topSection
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignTop
            Layout.preferredHeight: 30

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1

            onResetToDefaultRequested: {
                preferencesModel.resetToDefault()
            }
        }

        ValueList {
            Layout.fillWidth: true
            Layout.fillHeight: true

            keyRoleName: "descriptionRole"
            keyTitle: qsTrc("appshell/preferences", "Preference")
            valueRoleName: "valueRole"
            valueTitle: qsTrc("appshell/preferences", "Value")
            valueTypeRole: "typeRole"
            minValueRoleName: "minValueRole"
            maxValueRoleName: "maxValueRole"

            navigationSection: root.navigationSection
            navigationOrderStart: root.navigationOrderStart + 2

            model: SortFilterProxyModel {
                sourceModel: preferencesModel

                filters: [
                    FilterValue {
                        roleName: "descriptionRole"
                        roleValue: topSection.searchText
                        compareType: CompareType.Contains
                    }
                ]
            }
        }
    }
}
