/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Usage info")

    property alias sendAnonymousUsageInfo: sendUsageInfoCheckBox.checked
    property string privacyPolicyUrl

    signal sendAnonymousUsageInfoChangeRequested(bool send)

    CheckBox {
        id: sendUsageInfoCheckBox
        width: parent.width

        text: qsTrc("appshell/preferences", "Send anonymous usage info")

        navigation.name: "SendUsageInfoCheckBox"
        navigation.panel: root.navigation
        navigation.row: 0

        onClicked: {
            root.sendAnonymousUsageInfoChangeRequested(!checked)
        }
    }

    StyledTextLabel {
        width: parent.width

        text: qsTrc("appshell/preferences", "To help us understand how often people use Audacity, we generate a random ID (UUID) for each installation. This ID does not contain any personally identifiable information. Want to know more? Check out our <a href=\"%1\">privacy policy</a>.")
              .arg(root.privacyPolicyUrl)
              .replace("\n", "<br>")

        horizontalAlignment: Qt.AlignLeft
        wrapMode: Text.WordWrap
        maximumLineCount: 3
    }
}
