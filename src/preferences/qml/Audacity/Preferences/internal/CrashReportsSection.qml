/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Automatic crash reports")

    property alias sendCrashReports: sendCrashReportsCheckBox.checked
    property string privacyPolicyUrl

    signal sendCrashReportsChangeRequested(bool send)

    CheckBox {
        id: sendCrashReportsCheckBox
        width: parent.width

        text: qsTrc("appshell/preferences", "Send crash reports")

        navigation.name: "SendCrashReportsCheckBox"
        navigation.panel: root.navigation
        navigation.row: 0

        onClicked: {
            root.sendCrashReportsChangeRequested(!checked)
        }
    }

    StyledTextLabel {
        width: parent.width

        text: qsTrc("appshell/preferences", "Crash reports require network access. When Audacity crashes, it sends a report that helps us find and fix the cause. These reports don’t contain your audio or personal information. See our <a href=\"%1\">privacy policy</a> for more info.").arg(root.privacyPolicyUrl).replace("\n", "<br>")

        horizontalAlignment: Qt.AlignLeft
        wrapMode: Text.WordWrap
        maximumLineCount: 3
    }
}
