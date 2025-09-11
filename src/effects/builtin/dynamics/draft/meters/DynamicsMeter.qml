import QtQuick 2.15
import Audacity.BuiltinEffects 1.0

Item {
    id: root

    required property real dbMin
    required property color areaColor
    required property color lineColor
    required property int clipIndicatorHeight
    required property double currentMax
    required property double fiveSecMax
    required property bool upwards

    clip: true

    QtObject {
        id: prv

        function dbToY(db) {
            return db / root.dbMin * root.height
        }
    }

    Repeater {
        model: [currentMax, fiveSecMax]

        Rectangle {
            id: bar

            width: root.width
            height: upwards ? root.height - y : prv.dbToY(modelData)
            y: upwards ? prv.dbToY(modelData) : 0

            color: root.areaColor

            Rectangle {
                width: parent.width
                height: clipIndicatorHeight
                y: upwards ? 0 : bar.height - clipIndicatorHeight

                color: root.lineColor
                radius: 3
            }
        }
    }
}
