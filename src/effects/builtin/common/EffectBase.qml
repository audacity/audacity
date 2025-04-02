import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

Rectangle {

    id: root

    property var instanceId: null

    property AbstractEffectModel model: null

    color: ui.theme.backgroundPrimaryColor

    function preview() {
        root.model.preview()
    }
}
