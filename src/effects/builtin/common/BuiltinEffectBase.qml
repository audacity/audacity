import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.BuiltinEffects

Rectangle {
    id: root

    property var dialogView: null

    property BuiltinEffectModel model: null
    property bool usesPresets: model ? model.usesPresets : true

    color: ui.theme.backgroundPrimaryColor

    function preview() {
        root.model.preview()
    }
}
