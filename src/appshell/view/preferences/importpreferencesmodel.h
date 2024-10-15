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
#ifndef AU_APPSHELL_IMPORTPREFERENCESMODEL_H
#define AU_APPSHELL_IMPORTPREFERENCESMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "async/asyncable.h"

#include "importexport/musicxml/imusicxmlconfiguration.h"
#include "importexport/guitarpro/iguitarproconfiguration.h"
#include "importexport/ove/ioveconfiguration.h"
#include "importexport/midi/imidiconfiguration.h"
#include "importexport/mei/imeiconfiguration.h"
#include "notation/inotationconfiguration.h"

namespace au::appshell {
class ImportPreferencesModel : public QObject, public async::Asyncable
{
    Q_OBJECT

    INJECT(iex::musicxml::IMusicXmlConfiguration, musicXmlConfiguration)
    INJECT(iex::guitarpro::IGuitarProConfiguration, guitarProConfiguration)
    INJECT(iex::ove::IOveConfiguration, oveConfiguration)
    INJECT(iex::midi::IMidiImportExportConfiguration, midiImportExportConfiguration)
    INJECT(iex::mei::IMeiConfiguration, meiConfiguration)
    INJECT(notation::INotationConfiguration, notationConfiguration)

    Q_PROPERTY(QString styleFileImportPath READ styleFileImportPath WRITE setStyleFileImportPath NOTIFY styleFileImportPathChanged)

    Q_PROPERTY(
        QString currentOvertureCharset READ currentOvertureCharset WRITE setCurrentOvertureCharset NOTIFY currentOvertureCharsetChanged)

    Q_PROPERTY(bool importLayout READ importLayout WRITE setImportLayout NOTIFY importLayoutChanged)
    Q_PROPERTY(bool importBreaks READ importBreaks WRITE setImportBreaks NOTIFY importBreaksChanged)
    Q_PROPERTY(bool needUseDefaultFont READ needUseDefaultFont WRITE setNeedUseDefaultFont NOTIFY needUseDefaultFontChanged)

    Q_PROPERTY(bool meiImportLayout READ meiImportLayout WRITE setMeiImportLayout NOTIFY meiImportLayoutChanged)

    Q_PROPERTY(int currentShortestNote READ currentShortestNote WRITE setCurrentShortestNote NOTIFY currentShortestNoteChanged)

    Q_PROPERTY(
        bool needAskAboutApplyingNewStyle READ needAskAboutApplyingNewStyle WRITE setNeedAskAboutApplyingNewStyle NOTIFY needAskAboutApplyingNewStyleChanged)

public:
    explicit ImportPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    Q_INVOKABLE QVariantList charsets() const;
    Q_INVOKABLE QVariantList shortestNotes() const;
    Q_INVOKABLE QStringList stylePathFilter() const;
    Q_INVOKABLE QString styleChooseTitle() const;
    Q_INVOKABLE QString fileDirectory(const QString& filePath) const;

    QString styleFileImportPath() const;
    QString currentOvertureCharset() const;

    bool importLayout() const;
    bool importBreaks() const;
    bool needUseDefaultFont() const;

    int currentShortestNote() const;

    bool needAskAboutApplyingNewStyle() const;

    bool meiImportLayout() const;

public slots:
    void setStyleFileImportPath(QString path);
    void setCurrentOvertureCharset(QString charset);

    void setImportLayout(bool import);
    void setImportBreaks(bool import);
    void setNeedUseDefaultFont(bool value);

    void setCurrentShortestNote(int note);

    void setNeedAskAboutApplyingNewStyle(bool value);

    void setMeiImportLayout(bool import);

signals:
    void styleFileImportPathChanged(QString styleFileImportPath);
    void currentOvertureCharsetChanged(QString currentOvertureCharset);
    void importLayoutChanged(bool importLayout);
    void importBreaksChanged(bool importBreaks);
    void needUseDefaultFontChanged(bool needUseDefaultFont);
    void currentShortestNoteChanged(int currentShortestNote);
    void needAskAboutApplyingNewStyleChanged(bool needAskAboutApplyingNewStyle);
    void meiImportLayoutChanged(bool importLayout);
};
}

#endif // AU_APPSHELL_IMPORTPREFERENCESMODEL_H
