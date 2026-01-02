import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.ToastNotification 1.0

StyledListView {
    id: root

    width: 360
    implicitHeight: contentHeight
    height: implicitHeight

    spacing: 20

    function dismissNotificationAt(index) {
        root.model.dismissNotification(index);
    }

    delegate: ToastNotificationItem {
        id: itemRect

        width: root.width
        x: root.width
        opacity: 0
        
        property bool enableAnimation: false

        title: model.title
        iconCode: model.iconCode
        message: model.message
        actions: model.actions
        dismissable: model.dismissable
        autoDismissTimeout: model.autoDismissTimeout
        
        onActionTriggered: function(actionStr) {
            root.model.executeAction(model.id, actionStr);
        }
        
        Component.onCompleted: {
            x = 0;
            opacity = 1;
        }
        
        Behavior on x {
            NumberAnimation {
                duration: 1000
                easing.type: Easing.OutCubic
            }
        }
        
        Behavior on opacity { OpacityAnimator { duration: 1000 } }

        onDismissed: {
            root.dismissNotificationAt(model.id);
        }
    }

    add: null
    addDisplaced: null
}