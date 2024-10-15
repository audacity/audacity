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

    UpdatePreferencesModel {
        id: updateModel
    }

    Column {
        width: parent.width
        spacing: root.sectionsSpacing

        AutomaticUpdateSection {
            isAppUpdatable: updateModel.isAppUpdatable()
            needCheckForNewAppVersion: updateModel.needCheckForNewAppVersion
            museScorePrivacyPolicyUrl: updateModel.museScorePrivacyPolicyUrl()

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1

            onNeedCheckForNewAppVersionChangeRequested: function(check) {
                updateModel.needCheckForNewAppVersion = check
            }
        }
    }
}
