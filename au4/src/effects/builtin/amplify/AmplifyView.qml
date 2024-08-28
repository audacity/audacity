import QtQuick

import Muse.UiComponents

import Audacity.Effects

Rectangle {

    property string title: "Amplify"
    property alias effectId: viewModel.effectId

    color: "#909000"
    width: 300
    height: 400

    AmplifyViewModel {
        id: viewModel
    }

    Component.onCompleted: {
        viewModel.init()
    }

    StyledSlider {
        width: parent.width

        value: viewModel.ratio
        to: 10
        from: 0
        stepSize: 0.1

        onMoved: {
            viewModel.ratio = value
        }
    }
}
