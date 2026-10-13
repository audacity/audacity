import QtQuick

Item {
    id: root

    property int handleHeight: 32
    property bool handlesVisible: false

    readonly property bool hovered: fadeInHandle.containsMouse || fadeOutHandle.containsMouse

    QtObject {
        id: prv

        readonly property int handleWidth: 30
    }

    ClipFadeHandle {
        id: fadeInHandle

        width: prv.handleWidth
        height: root.handleHeight
        visible: root.handlesVisible

        isFadeIn: true

        x: 0
    }

    ClipFadeHandle {
        id: fadeOutHandle

        width: prv.handleWidth
        height: root.handleHeight
        visible: root.handlesVisible

        isFadeIn: false

        x: root.width - prv.handleWidth
    }
}
