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

#include "errordetailsmodel.h"

#include <QApplication>
#include <QClipboard>
#include <QTextDocument>

#include "log.h"

using namespace mu::ui;

static const QString ERROR_TEXT_KEY("errorText");
static const QString ERROR_PLAIN_TEXT_KEY("errorPlainText");

static QString toPlainText(const QString& html)
{
    QTextDocument doc;
    doc.setHtml(html);

    return doc.toPlainText();
}

ErrorDetailsModel::ErrorDetailsModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

QVariant ErrorDetailsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return QVariant();
    }

    const QVariantMap& item = m_items.at(index.row());
    switch (role) {
    case ErrorText: return item[ERROR_TEXT_KEY];
    case ErrorPlainText: return item[ERROR_PLAIN_TEXT_KEY];
    }

    return QVariant();
}

int ErrorDetailsModel::rowCount(const QModelIndex&) const
{
    return m_items.size();
}

QHash<int, QByteArray> ErrorDetailsModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { ErrorText, ERROR_TEXT_KEY.toUtf8() },
        { ErrorPlainText, ERROR_PLAIN_TEXT_KEY.toUtf8() },
    };

    return roles;
}

void ErrorDetailsModel::load(const QString& detailedText)
{
    TRACEFUNC;

    beginResetModel();

    QStringList errors = detailedText.split('\n');

    for (const QString& error : errors) {
        QVariantMap item;
        item[ERROR_TEXT_KEY] = error;
        item[ERROR_PLAIN_TEXT_KEY] = toPlainText(error);

        m_items << item;
    }

    endResetModel();
}

bool ErrorDetailsModel::copyDetailsToClipboard()
{
    QClipboard* clipboard = QGuiApplication::clipboard();
    if (!clipboard || m_items.empty()) {
        return false;
    }

    QString text;
    int lastIdx = m_items.size() - 1;

    for (int i = 0; i < m_items.size(); ++i) {
        text += m_items[i][ERROR_PLAIN_TEXT_KEY].toString();

        if (i != lastIdx) {
            text += "\n";
        }
    }

    clipboard->setText(text);

    return true;
}
