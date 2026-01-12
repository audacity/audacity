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
#include "importpreferencesmodel.h"

#include <QTextCodec>

#include "engraving/types/constants.h"

#include "translation.h"

using namespace au::appshell;

ImportPreferencesModel::ImportPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void ImportPreferencesModel::load()
{
}

QVariantList ImportPreferencesModel::charsets() const
{
    QList<QByteArray> charsets = QTextCodec::availableCodecs();
    std::sort(charsets.begin(), charsets.end());

    QVariantList result;
    for (QByteArray charset: charsets) {
        result << QString(charset);
    }

    return result;
}

QVariantList ImportPreferencesModel::shortestNotes() const
{
    constexpr int division =  engraving::Constants::DIVISION;

    QVariantList result = {
        QVariantMap { { "title", qtrc("appshell/preferences", "Quarter") }, { "value", division } },
        QVariantMap { { "title", qtrc("appshell/preferences", "Eighth") }, { "value", division / 2 } },
        QVariantMap { { "title", qtrc("appshell/preferences", "16th") }, { "value", division / 4 } },
        QVariantMap { { "title", qtrc("appshell/preferences", "32nd") }, { "value", division / 8 } },
        QVariantMap { { "title", qtrc("appshell/preferences", "64th") }, { "value", division / 16 } },
        QVariantMap { { "title", qtrc("appshell/preferences", "128th") }, { "value", division / 32 } },
        QVariantMap { { "title", qtrc("appshell/preferences", "256th") }, { "value", division / 64 } },
        QVariantMap { { "title", qtrc("appshell/preferences", "512th") }, { "value", division / 128 } },
        QVariantMap { { "title", qtrc("appshell/preferences", "1024th") }, { "value", division / 256 } }
    };

    return result;
}

QStringList ImportPreferencesModel::stylePathFilter() const
{
    return { qtrc("appshell/preferences", "MuseScore style file") + " (*.mss)" };
}

QString ImportPreferencesModel::styleChooseTitle() const
{
    return qtrc("appshell/preferences", "Choose default style for imports");
}

QString ImportPreferencesModel::fileDirectory(const QString& filePath) const
{
    return io::dirpath(filePath.toStdString()).toQString();
}

QString ImportPreferencesModel::styleFileImportPath() const
{
    return notationConfiguration()->styleFileImportPath().toQString();
}

QString ImportPreferencesModel::currentOvertureCharset() const
{
    return QString::fromStdString(oveConfiguration()->importOvertureCharset());
}

bool ImportPreferencesModel::importLayout() const
{
    return musicXmlConfiguration()->musicxmlImportLayout();
}

bool ImportPreferencesModel::importBreaks() const
{
    return musicXmlConfiguration()->musicxmlImportBreaks();
}

bool ImportPreferencesModel::needUseDefaultFont() const
{
    return musicXmlConfiguration()->needUseDefaultFont();
}

int ImportPreferencesModel::currentShortestNote() const
{
    return midiImportExportConfiguration()->midiShortestNote();
}

bool ImportPreferencesModel::needAskAboutApplyingNewStyle() const
{
    return musicXmlConfiguration()->needAskAboutApplyingNewStyle();
}

bool ImportPreferencesModel::meiImportLayout() const
{
    return meiConfiguration()->meiImportLayout();
}

void ImportPreferencesModel::setStyleFileImportPath(QString path)
{
    if (path == styleFileImportPath()) {
        return;
    }

    notationConfiguration()->setStyleFileImportPath(path.toStdString());
    emit styleFileImportPathChanged(path);
}

void ImportPreferencesModel::setCurrentOvertureCharset(QString charset)
{
    if (charset == currentOvertureCharset()) {
        return;
    }

    oveConfiguration()->setImportOvertureCharset(charset.toStdString());
    emit currentOvertureCharsetChanged(charset);
}

void ImportPreferencesModel::setImportLayout(bool import)
{
    if (import == importLayout()) {
        return;
    }

    musicXmlConfiguration()->setMusicxmlImportLayout(import);
    emit importLayoutChanged(import);
}

void ImportPreferencesModel::setImportBreaks(bool import)
{
    if (import == importBreaks()) {
        return;
    }

    musicXmlConfiguration()->setMusicxmlImportBreaks(import);
    emit importBreaksChanged(import);
}

void ImportPreferencesModel::setNeedUseDefaultFont(bool value)
{
    if (value == needUseDefaultFont()) {
        return;
    }

    musicXmlConfiguration()->setNeedUseDefaultFont(value);
    emit needUseDefaultFontChanged(value);
}

void ImportPreferencesModel::setCurrentShortestNote(int note)
{
    if (note == currentShortestNote()) {
        return;
    }

    midiImportExportConfiguration()->setMidiShortestNote(note);
    emit currentShortestNoteChanged(note);
}

void ImportPreferencesModel::setNeedAskAboutApplyingNewStyle(bool value)
{
    if (value == needAskAboutApplyingNewStyle()) {
        return;
    }

    musicXmlConfiguration()->setNeedAskAboutApplyingNewStyle(value);
    emit needAskAboutApplyingNewStyleChanged(value);
}

void ImportPreferencesModel::setMeiImportLayout(bool import)
{
    if (import == meiImportLayout()) {
        return;
    }

    meiConfiguration()->setMeiImportLayout(import);
    emit meiImportLayoutChanged(import);
}
