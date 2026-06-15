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
        id: pluginPreferencesModel
    }

    Component.onCompleted: {
        pluginPreferencesModel.init()
    }

    Column {

        width: parent.width
        spacing: root.sectionsSpacing

        EffectOptionsSection {
            id: effectBehaviorSection

            pluginPreferencesModel: pluginPreferencesModel

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

            visible: pluginPreferencesModel.lv2Supported

            title: qsTrc("preferences", "Custom LV2 plugins location")
            dialogTitle: qsTrc("preferences", "Choose custom LV2 plugins location")

            paths: pluginPreferencesModel.lv2CustomPaths
            pathValidator: function (p) {
                return pluginPreferencesModel.pathExists(p)
            }

            navigation.section: root.navigationSection
            navigation.order: effectBehaviorSection.navigation.order + 1

            onAddPathRequested: pluginPreferencesModel.addLv2Path()
            onPathChanged: function (index, newPath) {
                pluginPreferencesModel.setLv2Path(index, newPath)
            }
            onRemovePathRequested: function (index) {
                pluginPreferencesModel.removeLv2Path(index)
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

            visible: pluginPreferencesModel.vst3Supported

            title: qsTrc("preferences", "Custom VST3 plugins location")
            dialogTitle: qsTrc("preferences", "Choose custom VST3 plugins location")

            paths: pluginPreferencesModel.vst3CustomPaths
            pathValidator: function (p) {
                return pluginPreferencesModel.pathExists(p)
            }

            navigation.section: root.navigationSection
            navigation.order: lv2Section.navigation.order + 1

            onAddPathRequested: pluginPreferencesModel.addVst3Path()
            onPathChanged: function (index, newPath) {
                pluginPreferencesModel.setVst3Path(index, newPath)
            }
            onRemovePathRequested: function (index) {
                pluginPreferencesModel.removeVst3Path(index)
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }
    }
}
