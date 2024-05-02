/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Audacity.Project 1.0
import Muse.Ui 1.0
import Muse.UiComponents 1.0

StyledListView {
    id: root

    required property ProjectPropertiesModel propertiesModel

    property int propertyNameWidth: 160
    property int propertyRowHorizontalSpacing: 8

    readonly property int propertyRowRightMargin: propertyRowHorizontalSpacing + visualScrollBarInset

    property NavigationPanel navigationPanel: null
    property int navigationColumnStart: 0

    spacing: 4

    model: propertiesModel

    scrollBarPolicy: ScrollBar.AlwaysOn

    Connections {
        target: root.propertiesModel

        function onPropertyAdded(index) {
            root.positionViewAtIndex(index, ListView.Contain)
            root.currentIndex = index
        }
    }

    delegate: PropertyItem {
        anchors.left: parent ? parent.left : undefined
        anchors.right: parent ? parent.right : undefined
        anchors.rightMargin: root.propertyRowRightMargin

        spacing: root.propertyRowHorizontalSpacing

        index: root.navigationColumnStart + model.index
        propertyName: model.propertyName
        propertyValue: model.propertyValue
        isStandardProperty: model.isStandardProperty
        isFileInfoPanelProperty: false
        propertyNameWidth: root.propertyNameWidth

        navigationPanel: root.navigationPanel

        onPropertyNameChanged: function() {
            model.propertyName = propertyName
        }

        onPropertyValueChanged: function() {
            model.propertyValue = propertyValue
        }

        onScrollIntoViewRequested: function() {
            root.positionViewAtIndex(model.index, ListView.Contain)
        }

        onDeletePropertyRequested: function() {
            root.propertiesModel.deleteProperty(model.index)
        }
    }
}
