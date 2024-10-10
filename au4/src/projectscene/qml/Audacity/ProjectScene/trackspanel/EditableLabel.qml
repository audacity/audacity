import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Item {
    id: root

    property string text

    signal textEdited(string text)

    function edit() {
        loader.edit(text)
    }

    MouseArea {
        anchors.fill: parent

        onDoubleClicked: {
            loader.edit(text)
        }
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

            onShortcutOverride: function(event) {
                if (event.key === Qt.Key_Up || event.key === Qt.Key_Down) {
                    event.accepted = true
                    return true
                }
                return false
            }

            onKeyPress: function(event) {
                let newPosition

                if (event.key === Qt.Key_Up)
                    newPosition = 0
                else if (event.key === Qt.Key_Down)
                    newPosition = newText.length
                else
                    return false

                if (event.modifiers & Qt.ShiftModifier)
                    selectText(cursorPosition, newPosition)
                else
                    cursorPosition = newPosition

                return true
            }

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
