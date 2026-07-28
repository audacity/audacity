/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.Ui
import Muse.UiComponents

import "internal"

PreferencesPage {
    id: root

    PluginPreferencesModel {
        id: preferencesModel
    }

    Component.onCompleted: {
        preferencesModel.init()
    }

    Column {
        id: sectionsColumn

        width: parent.width
        spacing: root.sectionsSpacing

        EffectOptionsSection {
            id: effectBehaviorSection

            pluginPreferencesModel: preferencesModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine {
            visible: lv2Section.visible
        }

        PluginLocationsSection {
            id: lv2Section

            visible: preferencesModel.lv2Supported

            title: qsTrc("preferences", "Custom LV2 plugins location")
            dialogTitle: qsTrc("preferences", "Choose custom LV2 plugins location")

            paths: preferencesModel.lv2CustomPaths
            pathValidator: function (p) {
                return preferencesModel.pathExists(p)
            }

            navigation.section: root.navigationSection
            navigation.order: effectBehaviorSection.navigation.order + 1

            onAddPathRequested: preferencesModel.addLv2Path()
            onPathChanged: function (index, newPath) {
                preferencesModel.setLv2Path(index, newPath)
            }
            onRemovePathRequested: function (index) {
                preferencesModel.removeLv2Path(index)
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine {
            visible: vst3Section.visible
        }

        PluginLocationsSection {
            id: vst3Section

            visible: preferencesModel.vst3Supported

            title: qsTrc("preferences", "Custom VST3 plugins location")
            dialogTitle: qsTrc("preferences", "Choose custom VST3 plugins location")

            paths: preferencesModel.vst3CustomPaths
            pathValidator: function (p) {
                return preferencesModel.pathExists(p)
            }

            navigation.section: root.navigationSection
            navigation.order: lv2Section.navigation.order + 1

            onAddPathRequested: preferencesModel.addVst3Path()
            onPathChanged: function (index, newPath) {
                preferencesModel.setVst3Path(index, newPath)
            }
            onRemovePathRequested: function (index) {
                preferencesModel.removeVst3Path(index)
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        Repeater {
            model: preferencesModel.extensionPreferences

            delegate: Column {
                id: extensionPreferences

                required property var modelData
                required property int index

                width: parent.width
                spacing: root.sectionsSpacing

                SeparatorLine {
                    width: parent.width
                }

                ExtensionPreferencesSection {
                    id: extensionSection

                    width: parent.width
                    preferenceGroup: extensionPreferences.modelData
                    pluginPreferencesModel: preferencesModel

                    navigation.section: root.navigationSection
                    navigation.order: vst3Section.navigation.order + 1 + extensionPreferences.index

                    onFocusChanged: {
                        if (activeFocus) {
                            const position = extensionSection.mapToItem(sectionsColumn, 0, 0)
                            root.ensureContentVisibleRequested(Qt.rect(position.x, position.y, width, height))
                        }
                    }
                }
            }
        }
    }
}
