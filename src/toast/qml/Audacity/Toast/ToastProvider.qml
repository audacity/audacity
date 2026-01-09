import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import "."

StyledListView {
    id: root

    property int animationDuration: 600

    width: 360
    implicitHeight: contentHeight
    height: implicitHeight

    spacing: 20

    x: parent.width - width - 20
    y: parent.height - height - 50

    model: toastmodel

    ToastListModel {
        id: toastmodel
    }

    Component.onCompleted: {
        toastmodel.init()
    }

    function dismissToast(id) {
        root.model.dismissToast(id);
    }

    delegate: ToastItem {
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
        progress: model.progress
 
        onActionTriggered: function(actionStr) {
            root.model.executeAction(model.id, actionStr);
        }
        
        Component.onCompleted: {
            x = 0;
            opacity = 1;
        }
        
        Behavior on x {
            NumberAnimation {
                duration: root.animationDuration
                easing.type: Easing.OutCubic
            }
        }
        
        Behavior on opacity { OpacityAnimator { duration: root.animationDuration } }

        onDismissed: {
            root.dismissToast(model.id);
        }
    }

    add: null
    addDisplaced: null
}
