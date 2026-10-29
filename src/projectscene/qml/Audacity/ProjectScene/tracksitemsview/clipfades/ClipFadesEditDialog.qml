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

    property int focusedFade: ClipSide.Left

    title: qsTrc("projectscene", "Edit fades") + " - " + (fadesModel.clipCount > 1 ? qsTrc("projectscene", "%n clips", "", fadesModel.clipCount) : fadesModel.clipTitle)

    contentWidth: 320
    contentHeight: content.implicitHeight

    modal: true
    resizable: false

    ClipFadesEditModel {
        id: fadesModel
    }

    QtObject {
        id: prv

        property bool applied: false
    }

    Component.onCompleted: {
        fadesModel.load()
    }

    // Closing by other means than "Apply" (Cancel, Escape, title bar) discards the preview
    onClosed: {
        if (!prv.applied) {
            fadesModel.cancel()
        }
    }

    onNavigationActivateRequested: {
        if (root.focusedFade === ClipSide.Right) {
            fadeOutSection.requestActiveFocus()
        } else {
            fadeInSection.requestActiveFocus()
        }
    }

    ColumnLayout {
        id: content

        anchors.left: parent.left
        anchors.right: parent.right
        spacing: 0

        Column {
            Layout.fillWidth: true
            Layout.margins: 16

            spacing: 16

            FadeSection {
                id: fadeInSection

                width: parent.width

                title: qsTrc("projectscene", "Fade-in")
                length: fadesModel.fadeInLength
                maxLength: fadesModel.maxFadeLength
                fadeType: fadesModel.fadeInType

                navigationSection: root.navigationSection
                navigationOrderStart: 1

                onLengthChangeRequested: function (newLength) {
                    fadesModel.fadeInLength = newLength
                }
                onFadeTypeChangeRequested: function (newType) {
                    fadesModel.fadeInType = newType
                }
            }

            FadeSection {
                id: fadeOutSection

                width: parent.width

                title: qsTrc("projectscene", "Fade-out")
                length: fadesModel.fadeOutLength
                maxLength: fadesModel.maxFadeLength
                fadeType: fadesModel.fadeOutType

                navigationSection: root.navigationSection
                navigationOrderStart: fadeInSection.navigationOrderStart + fadeInSection.navigationPanelCount

                onLengthChangeRequested: function (newLength) {
                    fadesModel.fadeOutLength = newLength
                }
                onFadeTypeChangeRequested: function (newType) {
                    fadesModel.fadeOutType = newType
                }
            }
        }

        SeparatorLine {}

        ButtonBox {
            Layout.fillWidth: true
            Layout.margins: 12

            navigationPanel.section: root.navigationSection
            navigationPanel.order: fadeOutSection.navigationOrderStart + fadeOutSection.navigationPanelCount

            FlatButton {
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel
                isNarrow: true

                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                text: qsTrc("global", "Apply")
                buttonRole: ButtonBoxModel.ApplyRole
                buttonId: ButtonBoxModel.Apply
                accentButton: true
                isNarrow: true

                onClicked: {
                    prv.applied = true
                    fadesModel.apply()
                    root.accept()
                }
            }
        }
    }
}
