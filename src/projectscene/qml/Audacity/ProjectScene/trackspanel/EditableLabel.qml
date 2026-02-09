import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

FocusScope {
    id: root

    property string text

    readonly property alias navigation: navCtrl

    implicitHeight: loader.implicitHeight
    implicitWidth: loader.implicitWidth

    signal textEdited(string text)

    function edit() {
        loader.edit(text)
    }

    NavigationControl {
        id: navCtrl
        name: root.objectName !== "" ? root.objectName : "EditableLabel"
        enabled: root.enabled && root.visible && !loader.isEditState

        accessible.role: MUAccessible.Information
        accessible.name: root.text
        accessible.visualItem: root

        onTriggered: {
            root.edit()
        }
    }

    NavigationFocusBorder {
        enabled: navCtrl.enabled
        navigationCtrl: navCtrl
    }

    Loader {
        id: loader

        anchors.fill: root

        property bool isEditState: false

        sourceComponent: isEditState ? textEditComp : labelComp

        function edit(text) {
            isEditState = true
            item.ensureActiveFocus()
        }

        function finishEdit() {
            var itemWasActive = loader.item.navigation.active

            isEditState = false

            if (itemWasActive) {
                navCtrl.requestActive()
            }
        }
    }

    Component {
        id: labelComp

        StyledTextLabel {
            text: root.text
            font: ui.theme.bodyFont
            horizontalAlignment: Text.AlignLeft
        }
    }

    Component {
        id: textEditComp

        TextInputField {
            property string newText: root.text

            currentText: root.text

            background.color: "transparent"
            background.border.width: 0
            background.radius: 0
            textSidePadding: 0

            navigation.panel: navCtrl.panel
            navigation.order: navCtrl.order
            navigation.enabled: !navCtrl.enabled
            navigation.onActiveChanged: {
                if (!navigation.active && loader.isEditState) {
                    loader.finishEdit()
                }
            }

            onTextEdited: function(text) {
                newText = text
            }

            onEscaped: {
                clear()
                newText = ""

                loader.finishEdit()
            }

            onFocusChanged: {
                if (focus) {
                    return
                }

                if (hasText) {
                    root.textEdited(newText)
                }

                loader.finishEdit()
            }
        }
    }
}
