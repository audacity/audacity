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
    property bool isPreviewing: model ? model.isPreviewing : false
    property bool usedDestructively: true

    color: ui.theme.backgroundPrimaryColor

    function init() {
        root.model.init()
    }

    function startPreview() {
        root.model.startPreview()
    }

    function stopPreview() {
        root.model.stopPreview()
    }
}
