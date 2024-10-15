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

import Muse.UiComponents 1.0
import MuseScore.Preferences 1.0

import "internal"

PreferencesPage {
    id: root

    Component.onCompleted: {
        importPreferencesModel.load()
    }

    ImportPreferencesModel {
        id: importPreferencesModel
    }

    Column {
        width: parent.width
        spacing: root.sectionsSpacing

        ImportStyleSection {
            styleFileImportPath: importPreferencesModel.styleFileImportPath
            fileChooseTitle: importPreferencesModel.styleChooseTitle()
            filePathFilter: importPreferencesModel.stylePathFilter()
            fileDirectory: importPreferencesModel.fileDirectory(styleFileImportPath)

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1

            onStyleFileImportPathChangeRequested: function(path) {
                importPreferencesModel.styleFileImportPath = path
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine { }

        CharsetsSection {
            charsets: importPreferencesModel.charsets()
            currentOvertureCharset: importPreferencesModel.currentOvertureCharset

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 2

            onOvertureCharsetChangeRequested: function(charset) {
                importPreferencesModel.currentOvertureCharset = charset
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine { }

        MusicXmlSection {
            importLayout: importPreferencesModel.importLayout
            importBreaks: importPreferencesModel.importBreaks
            needUseDefaultFont: importPreferencesModel.needUseDefaultFont

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 3

            onImportLayoutChangeRequested: function(importLayout) {
                importPreferencesModel.importLayout = importLayout
            }

            onImportBreaksChangeRequested: function(importBreaks) {
                importPreferencesModel.importBreaks = importBreaks
            }

            onUseDefaultFontChangeRequested: function(use) {
                importPreferencesModel.needUseDefaultFont = use
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine { }

        MidiSection {
            shortestNotes: importPreferencesModel.shortestNotes()
            currentShortestNote: importPreferencesModel.currentShortestNote

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 4

            onCurrentShortestNoteChangeRequested: function(note) {
                importPreferencesModel.currentShortestNote = note
            }

            onFocusChanged: {
                if (activeFocus) {
                    root.ensureContentVisibleRequested(Qt.rect(x, y, width, height))
                }
            }
        }

        SeparatorLine { }

        MeiSection {
            meiImportLayout: importPreferencesModel.meiImportLayout

            onMeiImportLayoutChangeRequested: function(meiImportLayout) {
                importPreferencesModel.meiImportLayout = meiImportLayout
            }
        }
    }
}
