/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15
import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Toast 1.0

Rectangle {
    id: root
    
    clip: true
    color: ui.theme.backgroundPrimaryColor
    
    ToastTestsModel {
        id: toastModel
    }
    
    Flickable {
        id: flickable

        anchors.fill: parent
        anchors.margins: 12

        contentHeight: contenColumn.implicitHeight

        Column {
            id: contenColumn

            width: parent.width
            spacing: 36
            
            StyledTextLabel {
                text: "Toast Notification Tests"
                font: ui.theme.largeBodyFont
            }
    
            StyledGroupBox {
                width: Math.min(parent.width, 480)
                title: "Toast with Timeout"
                
                Column {
                    width: parent.width
                    spacing: 12
                    
                    Row {
                        spacing: 12
                        width: parent.width
                        
                        Column {
                            width: parent.width / 2 - 6
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Title:"
                            }
                            
                            TextInputField {
                                id: timeoutTitle
                                width: parent.width
                                currentText: "Timeout Toast"
                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                        
                        Column {
                            width: parent.width / 2 - 6
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Message:"
                            }
                            
                            TextInputField {
                                id: timeoutMessage
                                width: parent.width
                                currentText: "This will disappear automatically"

                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                    }
                    
                    Row {
                        spacing: 12
                        width: parent.width
                        
                        Column {
                            width: (parent.width - 36) / 4
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Icon Code:"
                            }
                            
                            TextInputField {
                                id: timeoutIconCode
                                width: parent.width
                                currentText: "0xEF4E"

                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                        
                        Column {
                            width: (parent.width - 36) / 4
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Timeout (seconds):"
                            }
                            
                            TextInputField {
                                id: timeoutSeconds
                                width: parent.width
                                currentText: "5"

                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                        
                        CheckBox {
                            id: timeoutDismissible
                            text: "Dismissible"
                            checked: true

                            onClicked: function() {
                                timeoutDismissible.checked = !timeoutDismissible.checked
                            }
                        }
                    }
                    
                    FlatButton {
                        text: "Show Toast with Timeout"
                        width: parent.width
                        onClicked: {
                            toastModel.showToastWithTimeout(
                                timeoutTitle.currentText,
                                timeoutMessage.currentText,
                                parseInt(timeoutIconCode.currentText),
                                parseInt(timeoutSeconds.currentText),
                                timeoutDismissible.checked
                            )
                        }
                    }
                }
            }
            
            StyledGroupBox {
                width: Math.min(parent.width, 480)
                title: "Simple toast types"
    
                Column {
                    width: parent.width
                    spacing: 12
                    
                    Row {
                        spacing: 12
                        width: parent.width
                        
                        Column {
                            width: parent.width / 2 - 6
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Title:"
                            }
                            
                            TextInputField {
                                id: simpleTitle
                                width: parent.width
                                currentText: "Simple Test"
                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                        
                        Column {
                            width: parent.width / 2 - 6
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Message:"
                            }
                            
                            TextInputField {
                                id: simpleMessage
                                width: parent.width
                                currentText: "Simple toast message"
                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                    }
                    
                    Row {
                        spacing: 12
                        width: parent.width
                        
                        FlatButton {
                            text: "Success"
                            width: (parent.width - 36) / 4
                            onClicked: {
                                toastModel.showSuccess(simpleTitle.currentText, simpleMessage.currentText)
                            }
                        }
                        
                        FlatButton {
                            text: "Error"
                            width: (parent.width - 36) / 4
                            onClicked: {
                                toastModel.showError(simpleTitle.currentText, simpleMessage.currentText)
                            }
                        }
                        
                        FlatButton {
                            text: "Info"
                            width: (parent.width - 36) / 4
                            onClicked: {
                                toastModel.showInfo(simpleTitle.currentText, simpleMessage.currentText)
                            }
                        }
                        
                        FlatButton {
                            text: "Warning"
                            width: (parent.width - 36) / 4
                            onClicked: {
                                toastModel.showWarning(simpleTitle.currentText, simpleMessage.currentText)
                            }
                        }
                    }
                }
            }

            StyledGroupBox {
                width: Math.min(parent.width, 480)
                title: "Toast executing code on button press"
    
                Column {
                    width: parent.width
                    spacing: 12
                    
                    Row {
                        spacing: 12
                        width: parent.width
                        
                        Column {
                            width: parent.width / 2 - 6
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Title:"
                            }
                            
                            TextInputField {
                                id: asyncTitle
                                width: parent.width
                                currentText: "Title"
                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                        
                        Column {
                            width: parent.width / 2 - 6
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Message:"
                            }
                            
                            TextInputField {
                                id:  asyncMessage
                                width: parent.width
                                currentText: "Message"
                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                    }
                        
                    FlatButton {
                        text: "Throw info dialog when clicked"
                        width: parent.width
                        onClicked: {
                            toastModel.showToastWithAction(
                                asyncTitle.currentText,
                                asyncMessage.currentText,
                                IconCode.INFO,
                            )
                        }
                    }
                }
            }
            
            StyledGroupBox {
                width: Math.min(parent.width, 480)
                title: "Toast with Progress"
                
                Column {
                    width: parent.width
                    spacing: 12
                    
                    Row {
                        spacing: 12
                        width: parent.width
                        
                        Column {
                            width: parent.width / 2 - 6
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Title:"
                            }
                            
                            TextInputField {
                                id: progressTitle
                                width: parent.width
                                currentText: "Progress Toast"

                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                        
                        Column {
                            width: parent.width / 2 - 6
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Message:"
                            }
                            
                            TextInputField {
                                id: progressMessage
                                width: parent.width
                                currentText: "Processing..."

                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                    }
                    
                    Row {
                        spacing: 12
                        width: parent.width
                        
                        Column {
                            width: parent.width / 2 - 6
                            spacing: 8
                            
                            StyledTextLabel {
                                text: "Icon Code:"
                            }
                            
                            TextInputField {
                                id: progressIconCode
                                width: parent.width
                                currentText: "0xF3C9"

                                onTextChanged: function(newTextValue) {
                                    currentText = newTextValue
                                }
                            }
                        }
                        
                        CheckBox {
                            id: progressDismissible
                            text: "Dismissible"
                            checked: true

                            onClicked: function() {
                                progressDismissible.checked = !progressDismissible.checked
                            }
                        }
                    }
                    
                    Row {
                        width: parent.width
                        spacing: 12
                        
                        StyledTextLabel {
                            text: "Progress:"
                            width: 80
                            anchors.verticalCenter: parent.verticalCenter
                        }
                        
                        StyledSlider {
                            id: progressSlider
                            width: parent.width - 140
                            from: 0
                            to: 100
                            value: 0
                            stepSize: 1
                            
                            onMoved: {
                                toastModel.updateProgress(value)

                                if (value >= 100) {
                                    value = 0
                                }
                            }
                        }
                    }

                    FlatButton {
                        text: "Show Progress Toast"
                        width: parent.width
                        onClicked: {
                            toastModel.showWithProgress(
                                progressTitle.currentText,
                                progressMessage.currentText,
                                parseInt(progressIconCode.currentText),
                                progressDismissible.checked
                            )
                            toastModel.updateProgress(progressSlider.value)
                        }
                    }
                    
                }
            }
        }
    }
}