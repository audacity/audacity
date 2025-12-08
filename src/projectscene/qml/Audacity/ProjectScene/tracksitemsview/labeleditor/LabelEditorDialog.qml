/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

StyledDialogView {
    id: root

    title: qsTrc("projectscene", "Label editor")

    contentWidth: 880
    contentHeight: 528

    resizable: false

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        LabelEditorTopPanel {
            Layout.fillWidth: true

            onAddLabelRequested: {
                labelsTableView.addNewLabel()
            }

            onRemoveSelectedLabelsRequested: {
                labelsTableView.removeSelectedLabels()
            }

            onExportRequested: {
                labelsTableView.exportLabels()
            }

            onImportRequested: {
                labelsTableView.importLabels()
            }
        }

        LabelEditorLabelsTableView {
            id: labelsTableView

            Layout.fillWidth: true
            Layout.fillHeight: true
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true
            Layout.margins: 12

            buttons: [ ButtonBoxModel.Close ]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 4

            onStandardButtonClicked: function(buttonId) {
                switch(buttonId) {
                case ButtonBoxModel.Close: root.accept(); break;
                }
            }
        }
    }
}
