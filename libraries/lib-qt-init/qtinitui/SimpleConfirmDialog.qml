import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Audacity.UiComponents
import Audacity.Ui

Dialog {
    
    property alias text : text.text 
    
    id : dialog
    modal : true
    anchors.centerIn : parent

    background: Rectangle {
        anchors.fill : parent
        color: ui.theme.backgroundColor2
    }

    Text {
        id : text
        padding : 16
        color: ui.theme.fontColor1
        font.family: ui.theme.bodyFont.family
    }
    footer : RowLayout {
        spacing : 8
        FlatButton {
            Layout.fillWidth : true
            Layout.leftMargin : 16
            Layout.bottomMargin : 16

            text : "Yes"
            height: 24
            buttonType : FlatButton.Horizontal
            textFont.pixelSize: 12
            onClicked: dialog.accept()
        }
        FlatButton {
            Layout.fillWidth : true
            Layout.rightMargin : 16
            Layout.bottomMargin : 16

            text : "No"
            accentButton : true
            height: 24
            buttonType : FlatButton.Horizontal
            textFont.pixelSize: 12
            onClicked: dialog.reject()
        }
    }
}
