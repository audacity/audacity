import QtQuick

import Audacity.Effects

Rectangle {

    color: "#909000"

    AmplifyViewModel {
        id: viewModel
    }

    Component.onCompleted: {
        viewModel.init()
    }

}
