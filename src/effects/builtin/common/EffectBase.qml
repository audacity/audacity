import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.BuiltinEffects

Rectangle {

    id: root

    property var instanceId: null
    property var dialogView: null

    property AbstractEffectModel model: null

    color: ui.theme.backgroundPrimaryColor

    function preview() {
        root.model.preview()
    }
}
