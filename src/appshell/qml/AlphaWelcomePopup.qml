/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

StyledDialogView {
    id: root

    title: qsTrc("appshell/about", "Audacity 4")

    contentHeight: 578
    contentWidth: 560

    background.color: ui.theme.backgroundSecondaryColor

    ColumnLayout {
        anchors.fill: parent

        ColumnLayout {

            anchors.fill: parent

            spacing: 24
            anchors.margins: 24

            StyledTextLabel {
                text: "<b>Notes on Audacity 4 - Alpha 1</b>"

                font.pixelSize: 16
                anchors.horizontalCenter: parent.horizontalCenter
            }

            Rectangle {
                anchors.horizontalCenter: parent.horizontalCenter

                width: 512
                height: 426

                color: ui.theme.textFieldColor
                border.width: 1
                border.color: ui.theme.strokeColor

                clip: true

                StyledFlickable {
                    anchors.fill: parent
                    anchors.margins: 16

                    contentHeight: message.implicitHeight

                    ScrollBar.vertical: StyledScrollBar {
                        id: scrollBar
                        policy: ScrollBar.AlwaysOn
                    }

                    StyledTextLabel {
                        id: message

                        width: parent.width
                        horizontalAlignment: Text.AlignLeft
                        wrapMode: Text.WordWrap

                        text: "<b>What am I testing in this build?</b><br><br>" +

                              "The purpose of this early-stage alpha release is to provide a build for our <br>" +
                              "community so they can test <b>core</b> aspects of Audacity’s functionality. This will <br>" +
                              "greatly help us spot issues and refine the experience. When reporting issues, <br>" +
                              "please keep them limited to these six areas.<br><br>" +

                              "<b>•</b> Recording & playback<br>" +
                              "<b>•</b> Editing audio<br>" +
                              "<b>•</b> Applying destructive and realtime effects<br>" +
                              "<b>•</b> Exporting projects and audio files<br>" +
                              "<b>•</b> Customising the interface to your liking<br>" +
                              "<b>•</b> Anything related to the interface (dialogs, popups, entering text values, viewing<br>" +
                              "&nbsp;&nbsp;&nbsp;on multiple monitors, etc.)<br><br>" +

                              "<b>Please log issues on GitHub (requires a free GitHub account):</b><br><br>" +

                              "We really value your feedback at this stage - it directly shapes Audacity 4’s<br>" +
                              "development<br><br>" +

                              "<a href=\"https://github.com/audacity/audacity/issues\">Link to bug report</a><br><br>" +

                              "<b>New features & improvements in Audacity 4</b><br><br>" +

                              "<b>•</b> A new interface<br>" +
                              "<b>•</b> Multiple colour theming options<br>" +
                              "<b>•</b> Multiple clip visualisation options<br>" +
                              "<b>•</b> When working with the ‘modern’ clip style, you can change the colour of clips<br>" +
                              "&nbsp;&nbsp;&nbsp;as well as tracks<br>" +
                              "<b>•</b> A new <b>Customise toolbar</b> widget, accessible via the ‘cog’ icon on the far right<br>" +
                              "&nbsp;&nbsp;&nbsp;of the top bar<br>" +
                              "<b>•</b> A new <b>Workspaces</b> feature, which can be used to save different layouts of the<br>" +
                              "&nbsp;&nbsp;&nbsp;interface<br>" +
                              "<b>•</b> A persistent playhead<br>" +
                              "<b>•</b> Clips can be moved around freely - no longer obstructed by other clips<br>" +
                              "<b>•</b> Recording can be initiated from any position in the timeline<br>" +
                              "<b>•</b> Multiple clips can be selected and edited at the same time<br>" +
                              "<b>•</b> When full clips are selected by clicking on their header, drag handles appear<br>" +
                              "&nbsp;&nbsp;&nbsp;that allow them to be trimmed or time-stretched<br>" +
                              "<b>•</b> Multiple clips can be <b>grouped together</b>. This feature can be found by selecting<br>" +
                              "&nbsp;&nbsp;&nbsp;the ‘...’ icon on the clip header, or by right-clicking on selected clips<br>" +
                              "<b>•</b> A new <b>split tool</b> for splitting clips quickly. You can toggle this feature by<br>" +
                              "&nbsp;&nbsp;&nbsp;pressing the <b>S</b> key. If you hold the <b>S</b> key down, the feature will be temporarily<br>" +
                              "&nbsp;&nbsp;&nbsp;enabled until you release the <b>S</b> key<br>" +
                              "<b>•</b> New settings & shortcuts for <b>‘ripple’ editing</b>, which allow you to delete and<br>" +
                              "&nbsp;&nbsp;&nbsp;paste clips, whilst preserving synchronisation across your project. Along with<br>" +
                              "&nbsp;&nbsp;&nbsp;our grouping feature, this is intended as a replacement to the ‘Sync-lock’<br>" +
                              "&nbsp;&nbsp;&nbsp;feature found in Audacity 3. The new shortcuts for ‘ripple editing’ can be found<br>" +
                              "&nbsp;&nbsp;&nbsp;in <b>Preferences > Shortcuts</b><br>" +
                              "<b>•</b> Tools like the Selection, Multi, and Draw functions are now built directly into the<br>" +
                              "&nbsp;&nbsp;&nbsp;main workflow, eliminating the need for separate buttons. For example, when<br>" +
                              "&nbsp;&nbsp;&nbsp;you zoom in to the sample level, the Draw tool activates automatically. As a<br>" +
                              "&nbsp;&nbsp;&nbsp;result, the Selection and Multi tools are no longer required, since all their<br>" +
                              "&nbsp;&nbsp;&nbsp;features are always available by default. This streamlines the interface by<br>" +
                              "&nbsp;&nbsp;&nbsp;reducing complexity without sacrificing functionality.<br>" +
                              "<b>•</b> The <b>looping</b> feature has been improved<br><br>" +

                              "<b>Missing features in this alpha release</b><br><br>" +

                              "These features are currently in development and will be included in the main<br>" +
                              "release of Audacity 4.0<br><br>" +

                              "<b>•</b> Macros<br>" +
                              "<b>•</b> Spectrogram<br>" +
                              "<b>•</b> Label tracks<br>" +
                              "<b>•</b> Label track editor<br>" +
                              "<b>•</b> Metadata editor<br>" +
                              "<b>•</b> The plugin manager<br>" +
                              "<b>•</b> Cloud uploading<br>" +
                              "<b>•</b> OpenVino plugin support<br>" +
                              "<b>•</b> Numerous native Audacity effects<br>" +
                              "<b>•</b> The ‘Envelope’ tool<br>"
                    }
                }
            }
        }

        SeparatorLine {}

        RowLayout {
            Layout.alignment: Qt.AlignRight
            Layout.rightMargin: 12
            Layout.bottomMargin: 12

            spacing: 12

            FlatButton {
                text: qsTrc("global", "OK")

                accentButton: true

                onClicked: {
                    root.accept()
                }
            }
        }
    }
}
