/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.AppShell

StyledDialogView {
    id: root

    property alias isCreateAccountMode: signinPage.isCreateAccountMode

    title: isCreateAccountMode ? qsTrc("cloud", "Create an account on audio.com") : qsTrc("cloud", "Sign in to audio.com")

    contentHeight: content.implicitHeight
    contentWidth: content.implicitWidth

    margins: 20

    modal: true

    ColumnLayout {
        id: content

        spacing: 0

        SigninAudiocomPage {
            id: signinPage

            Layout.preferredWidth: 560
            Layout.preferredHeight: 404

            navigationSection: root.navigationSection

            onNavNextPageRequested: {
                root.accept()
            }
        }

        ButtonBox {
            Layout.fillWidth: true
            Layout.topMargin: 12

            buttons: [ButtonBoxModel.Cancel]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 1

            onStandardButtonClicked: function (buttonId) {
                if (buttonId === ButtonBoxModel.Cancel) {
                    root.reject()
                }
            }
        }
    }
}
