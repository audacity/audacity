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
#ifndef MU_UI_TESTDIALOG_H
#define MU_UI_TESTDIALOG_H

#include <QDialog>

namespace Ui {
class TestDialog;
}

namespace mu::ui {
class TestDialog : public QDialog
{
    Q_OBJECT

    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged)

public:
#ifdef MU_QT5_COMPAT
    TestDialog(const TestDialog& dialog);
#endif
    explicit TestDialog(QWidget* parent = nullptr);
    ~TestDialog() override;

    QString title() const;

public slots:
    void setTitle(QString title);

signals:
    void titleChanged(QString title);

private:
    Ui::TestDialog* ui = nullptr;
    QString m_title;
};
}

#endif // MU_UI_TESTDIALOG_H
