/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
import QtQuick 2.15
import QtQuick.Window 2.15

import MuseScore.Ui 1.0

Item {
    id: root

    property var topParent: null
    property var provider: ui._interactiveProvider
    property var objects: ({})

    signal requestedDockPage(var uri, var params)

    anchors.fill: parent

    Rectangle {
        width: 100
        height: 100
        color: "#440000"
    }

    function onPageOpened() {
        root.provider.onOpen(ContainerType.PrimaryPage, {})
    }

    Connections {
        target: root.provider

        function onFireOpen(data) {

            var page = data.data()
            var uri = data.value("uri")

            console.log("try open uri: " + uri + ", page: " + JSON.stringify(page))
            if (!(page && (page.type === ContainerType.PrimaryPage || page.type === ContainerType.QmlDialog))) {
                data.setValue("ret", {errcode: 101 }) // ResolveFailed
                return;
            }

            if (page.type === ContainerType.PrimaryPage) {
                root.requestedDockPage(uri, page.params)
                data.setValue("ret", {errcode: 0 })
                return;
            }

            if (page.type === ContainerType.QmlDialog) {
                var dialogPath = "../../" + page.path

                var dialogObj = createDialog(dialogPath, page.params)
                data.setValue("ret", dialogObj.ret)
                data.setValue("objectId", dialogObj.object.objectId)

                if (dialogObj.ret.errcode > 0) {
                    return
                }

                if (Boolean(data.value("sync")) && data.value("sync") === true) {
                    dialogObj.object.exec()
                } else {
                    dialogObj.object.show()
                }
            }
        }

        function onFireOpenStandardDialog(data) {
            var dialog = data.data()
            var dialogObj = createDialog("internal/StandardDialog.qml", dialog.params)
            data.setValue("ret", dialogObj.ret)
            data.setValue("objectId", dialogObj.object.objectId)

            if (dialogObj.ret.errcode > 0) {
                return
            }

            dialogObj.object.exec()
        }

        function onFireClose(objectId) {
            var obj = root.findObject(objectId)
            if (obj) {
                obj.hide()
            }
            root.objects[objectId] = undefined
        }

        function onFireRaise(objectId) {
            var obj = root.findObject(objectId)
            if (obj) {
                obj.raise()
            }
        }

        function onFireOpenFileDialog(data) {
            var dialog = data.data()
            var dialogObj = null
            if (dialog.selectFolder) {
                dialogObj = createDialog("internal/FolderDialog.qml", dialog.params)
            } else {
                dialogObj = createDialog("internal/FileDialog.qml", dialog.params)
            }

            data.setValue("ret", dialogObj.ret)
            data.setValue("objectId", dialogObj.object.objectId)

            if (dialogObj.ret.errcode > 0) {
                return
            }

            dialogObj.object.open()
        }

        function onFireOpenProgressDialog(data) {
            var dialog = data.data()
            var dialogObj = createDialog("internal/ProgressDialog.qml", dialog.params)
            data.setValue("ret", dialogObj.ret)
            data.setValue("objectId", dialogObj.object.objectId)

            if (dialogObj.ret.errcode > 0) {
                return
            }

            dialogObj.object.open()
        }
    }

    function createDialog(path, params) {
        var comp = Qt.createComponent(path)
        if (comp.status !== Component.Ready) {
            console.log("[qml] failed create component: " + path + ", err: " + comp.errorString())
            return { "ret": { "errcode": 102 } } // CreateFailed
        }

        //! NOTE: set the top-level window as a parent,
        // because we want to ensure correct interaction between nested dialogs
        // (when opening one dialog from another)
        var obj = comp.createObject(root.provider.topWindow(), params)
        obj.objectId = root.provider.objectId(obj)
        root.objects[obj.objectId] = obj

        var ret = (obj.ret && obj.ret.errcode) ? obj.ret : {errcode: 0}

        if (ret.errcode > 0) {
            return { "ret": ret, "object" : obj }
        }

        obj.opened.connect(function() {
            var window = Boolean(obj.contentItem) ? obj.contentItem.Window.window : obj.Window.window
            root.provider.onOpen(ContainerType.QmlDialog, obj.objectId, window)
        })

        obj.closed.connect(function() {
            root.provider.onClose(obj.objectId, obj.ret ? obj.ret : {errcode: 0})
            obj.destroy()
        })

        return { "ret": ret, "object" : obj }
    }

    function findObject(objectId) {
        return root.objects[objectId]
    }
}
