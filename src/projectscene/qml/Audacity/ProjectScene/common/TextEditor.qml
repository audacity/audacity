import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: notepadWindow
    width: 800
    height: 600

    property alias notepad: notepad

    Notepad {
        id: notepad

        onFileChanged: {
            textArea.currentText = this.text
        }
    }

    ColumnLayout {
        width: parent.width
        height: parent.height

        TextInputArea {
            id: textArea

            Layout.fillWidth: true
            Layout.fillHeight: true

            onTextChanged: function (text) {
                notepad.setText(text)
            }
        }

        Text {
            Layout.fillWidth: true
            height: 40
            topPadding: 0
            rightPadding: 10
            bottomPadding: 5

            text: {
                const lines = notepad.text.split('\n').length;
                const numWords = notepad.text.trim().split(/\s+/).filter(w => w).length;
                const numChars = notepad.text.length;
                const fileName = notepad.file ? notepad.file.split('/').pop() : "Untitled";

                `File: ${fileName} | Lines: ${lines} | Words: ${numWords} | Characters: ${numChars}`
            }

            font.family: "monospace"
            color: ui.theme.fontPrimaryColor
            horizontalAlignment: Text.AlignRight
        }
    }
}
