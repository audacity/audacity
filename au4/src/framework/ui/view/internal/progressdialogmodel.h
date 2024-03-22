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
#ifndef MU_UI_PROGRESSDIALOGMODEL_H
#define MU_UI_PROGRESSDIALOGMODEL_H

#include <QObject>

#include "async/asyncable.h"
#include "progress.h"

namespace mu::ui {
class ProgressDialogModel : public QObject, public async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(int from READ from NOTIFY fromChanged)
    Q_PROPERTY(int value READ value NOTIFY valueChanged)
    Q_PROPERTY(int to READ to NOTIFY toChanged)
    Q_PROPERTY(QString statusMessage READ statusMessage NOTIFY statusMessageChanged)

public:
    explicit ProgressDialogModel(QObject* parent = nullptr);

    int from() const;
    int value() const;
    int to() const;
    QString statusMessage() const;

    Q_INVOKABLE void load(const QVariant& progressObj);
    Q_INVOKABLE void cancel();

signals:
    void fromChanged();
    void valueChanged();
    void toChanged();
    void statusMessageChanged();
    void finished();

private:
    void setFrom(int value);
    void setValue(int value);
    void setTo(int value);
    void setStatusMessage(const QString& msg);

    int m_from = 0;
    int m_value = 0;
    int m_to = 0;
    QString m_statusMessage;
    mu::Progress* m_progress = nullptr;
};
}

#endif //MU_UI_PROGRESSDIALOGMODEL_H
