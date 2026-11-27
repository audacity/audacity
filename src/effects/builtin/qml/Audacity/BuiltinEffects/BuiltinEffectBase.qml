import QtQuick

import Audacity.BuiltinEffects

Rectangle {
    id: root

    property var dialogView: null

    required property int instanceId
    required property BuiltinEffectModel builtinEffectModel
    property bool usesPresets: builtinEffectModel.usesPresets
    property bool isPreviewing: builtinEffectModel.isPreviewing
    property bool usedDestructively: true
    required property int numNavigationPanels

    //! Emit to apply the effect and close the dialog, as if the Apply button was clicked
    signal applyRequested

    color: ui.theme.backgroundPrimaryColor

    function init() {
        root.builtinEffectModel.init()
    }

    function startPreview() {
        root.builtinEffectModel.startPreview()
    }

    function stopPreview() {
        root.builtinEffectModel.stopPreview()
    }
}
