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
#ifndef AU_APPSHELL_MAINWINDOWTITLEPROVIDER_H
#define AU_APPSHELL_MAINWINDOWTITLEPROVIDER_H

#include <QObject>
#include <QString>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::appshell {
class MainWindowTitleProvider : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> context;

    Q_PROPERTY(QString title READ title NOTIFY titleChanged)
    Q_PROPERTY(QString filePath READ filePath NOTIFY filePathChanged)
    Q_PROPERTY(bool fileModified READ fileModified NOTIFY fileModifiedChanged)

public:
    explicit MainWindowTitleProvider(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    QString title() const;
    QString filePath() const;
    bool fileModified() const;

signals:
    void titleChanged(QString title);
    void filePathChanged(QString filePath);
    void fileModifiedChanged(bool fileModified);

private:
    void update();

    void setTitle(const QString& title);
    void setFilePath(const QString& filePath);
    void setFileModified(bool fileModified);

    QString m_title;
    QString m_filePath;
    bool m_fileModified;
};
}

#endif // AU_APPSHELL_MAINWINDOWTITLEPROVIDER_H
