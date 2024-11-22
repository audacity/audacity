import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

Rectangle {

    id: root

    property AbstractEffectModel model: null

    color: ui.theme.backgroundPrimaryColor

    function manage(parent) {
        var px = parent.x
        var py = parent.y + parent.height
        var pos = mapFromItem(parent, px, py)

        var items = [{"id": -1, "title": "Not implemented"}]
        menuLoader.show(pos, items)
    }

    function preview() {
        root.model.preview()
    }

    ContextMenuLoader {
        id: menuLoader

        onHandleMenuItem: function(itemId) {

        }
    }
}
