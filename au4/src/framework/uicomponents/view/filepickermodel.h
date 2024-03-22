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
#ifndef MU_UICOMPONENTS_FILEPICKERMODEL_H
#define MU_UICOMPONENTS_FILEPICKERMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "iinteractive.h"

namespace mu::uicomponents {
class FilePickerModel : public QObject
{
    Q_OBJECT

    INJECT(IInteractive, interactive)

    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged)
    Q_PROPERTY(QString dir READ dir WRITE setDir NOTIFY dirChanged)
    Q_PROPERTY(QStringList filter READ filter WRITE setFilter NOTIFY filterChanged)

public:
    explicit FilePickerModel(QObject* parent = nullptr);

    QString title() const;
    QString dir() const;
    QStringList filter() const;

    Q_INVOKABLE QString selectFile();
    Q_INVOKABLE QString selectDirectory();
    Q_INVOKABLE QString selectMultipleDirectories(const QString& selectedDirectoriesStr);

public slots:
    void setTitle(const QString& title);
    void setDir(const QString& dir);
    void setFilter(const QStringList& filter);

signals:
    void titleChanged(const QString& title);
    void dirChanged(const QString& dir);
    void filterChanged(const QStringList& filter);

private:
    QString m_title;
    QString m_dir;
    QStringList m_filter;
};
}

#endif // MU_UICOMPONENTS_FILEPICKERMODEL_H
