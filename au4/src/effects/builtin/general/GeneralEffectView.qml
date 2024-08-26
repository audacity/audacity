import QtQuick

import Muse.UiComponents

import Audacity.Effects

Rectangle {

    property string title: "Amplify"
    property alias instanceId: viewModel.instanceId

    color: ui.theme.backgroundPrimaryColor
    width: 300
    height: 400

    GeneralViewModel {
        id: viewModel
    }

    Component.onCompleted: {
        viewModel.init()
    }

    StyledListView {
        anchors.fill: parent

        model: viewModel.params

        delegate: StyledTextLabel {
            text: modelData.title
        }
    }
}
