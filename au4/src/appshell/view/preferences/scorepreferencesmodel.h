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
#ifndef AU_APPSHELL_SCOREPREFERENCESMODEL_H
#define AU_APPSHELL_SCOREPREFERENCESMODEL_H

#include <QAbstractListModel>

#include "modularity/ioc.h"
#include "async/asyncable.h"

#include "notation/inotationconfiguration.h"
#include "audio/iaudioconfiguration.h"

namespace au::appshell {
class ScorePreferencesModel : public QAbstractListModel, public async::Asyncable
{
    Q_OBJECT

    INJECT(notation::INotationConfiguration, notationConfiguration)
    INJECT(audio::IAudioConfiguration, audioConfiguration)

public:
    explicit ScorePreferencesModel(QObject* parent = nullptr);

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role) override;
    QHash<int, QByteArray> roleNames() const override;

    Q_INVOKABLE void load();

private:
    enum Roles {
        TitleRole = Qt::UserRole + 1,
        PathRole,
        PathFilterRole,
        ChooseTitleRole,
        DirectoryRole
    };

    enum class DefaultFileType {
        Undefined,
        FirstScoreOrderList,
        SecondScoreOrderList,
        Style,
        PartStyle
    };

    struct DefaultFileInfo {
        DefaultFileType type = DefaultFileType::Undefined;
        QString title;
        QString path;
        QStringList pathFilter;
        QString chooseTitle;
    };

    void savePath(DefaultFileType fileType, const QString& path);

    QString firstScoreOrderListPath() const;
    void setFirstScoreOrderListPath(const QString& path);

    QString secondScoreOrderListPath() const;
    void setSecondScoreOrderListPath(const QString& path);

    QString stylePath() const;
    QString partStylePath() const;

    QStringList scoreOrderPathFilter() const;
    QStringList stylePathFilter() const;

    QString scoreOrderChooseTitle() const;
    QString styleChooseTitle() const;
    QString partStyleChooseTitle() const;

    void setPath(DefaultFileType fileType, const QString& path);
    QModelIndex fileIndex(DefaultFileType fileType);

    QString fileDirectory(const QString& filePath) const;

    QList<DefaultFileInfo> m_defaultFiles;
};
}

#endif // AU_APPSHELL_SCOREPREFERENCESMODEL_H
