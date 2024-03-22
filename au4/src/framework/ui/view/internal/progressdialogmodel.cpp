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

#include "progressdialogmodel.h"

#include "progress.h"
#include "log.h"

using namespace mu::ui;

ProgressDialogModel::ProgressDialogModel(QObject* parent)
    : QObject(parent)
{
}

int ProgressDialogModel::from() const
{
    return m_from;
}

int ProgressDialogModel::value() const
{
    return m_value;
}

int ProgressDialogModel::to() const
{
    return m_to;
}

QString ProgressDialogModel::statusMessage() const
{
    return m_statusMessage;
}

void ProgressDialogModel::load(const QVariant& progressObj)
{
    IF_ASSERT_FAILED(progressObj.canConvert<mu::Progress*>()) {
        return;
    }

    m_progress = progressObj.value<mu::Progress*>();
    IF_ASSERT_FAILED(m_progress) {
        return;
    }

    m_progress->progressChanged.onReceive(this, [this](int64_t current, int64_t total, const std::string& status) {
        setValue(current);
        setTo(total);
        setStatusMessage(QString::fromStdString(status));
    });

    m_progress->finished.onReceive(this, [this](const ProgressResult& res) {
        if (!res.ret) {
            LOGE() << res.ret.toString();
        }

        emit finished();
    });
}

void ProgressDialogModel::cancel()
{
    IF_ASSERT_FAILED(m_progress) {
        return;
    }

    m_progress->cancel();
}

void ProgressDialogModel::setFrom(int value)
{
    if (m_from == value) {
        return;
    }

    m_from = value;
    emit fromChanged();
}

void ProgressDialogModel::setValue(int value)
{
    if (m_value == value) {
        return;
    }

    m_value = value;
    emit valueChanged();
}

void ProgressDialogModel::setTo(int value)
{
    if (m_to == value) {
        return;
    }

    m_to = value;
    emit toChanged();
}

void ProgressDialogModel::setStatusMessage(const QString& msg)
{
    if (m_statusMessage == msg) {
        return;
    }

    m_statusMessage = msg;
    emit statusMessageChanged();
}
