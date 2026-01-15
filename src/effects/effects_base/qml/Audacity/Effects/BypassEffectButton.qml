import QtQuick

import Muse.Ui
import Muse.UiComponents

FlatButton {
    id: root

    property bool isMasterEffect: false
    property int size: 24

    width: size
    height: size
    icon: IconCode.BYPASS
    iconColor: accentButton ? ui.theme.extra["white_color"] : ui.theme.fontPrimaryColor
    iconFont: ui.theme.toolbarIconsFont
    accentColor: isMasterEffect ? ui.theme.extra["black_color"] : ui.theme.accentColor
}
