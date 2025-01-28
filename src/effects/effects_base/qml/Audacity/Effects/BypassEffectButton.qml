import QtQuick

import Muse.Ui
import Muse.UiComponents

FlatButton {
    id: root

    property bool isMasterEffect: false

    height: width
    icon: IconCode.BYPASS
    iconColor: accentButton ? "white" : ui.theme.fontPrimaryColor
    iconFont: ui.theme.toolbarIconsFont
    accentColor: isMasterEffect ? "black" : ui.theme.accentColor
}
