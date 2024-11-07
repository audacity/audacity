import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Item {
    id: root

    property string text

    implicitHeight: loader.implicitHeight
    implicitWidth: loader.implicitWidth

    signal textEdited(string text)

    function edit() {
        loader.edit(text)
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
            background.color: "transparent"
            background.border.width: 0
            background.radius: 0
            textSidePadding: 0

            currentText: root.text
            property string newText: root.text

            onTextEdited: function(text) {
                newText = text
            }

            onEscaped: {
                clear()
                newText = ""
            }

            onFocusChanged: {
                if (focus) {
                    return
                }

                if (hasText) {
                    root.textEdited(newText)
                }
                loader.isEditState = false
            }
        }
    }
}
