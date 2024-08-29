import QtQuick
import QtQuick.Layouts

Rectangle {
    property bool isSelected: false

    Layout.fillHeight: true
    Layout.preferredWidth: 6

    color: isSelected ? ui.theme.accentColor : ui.theme.strokeColor
}
