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
#include "qmltranslation.h"

#include "translation.h"

using namespace mu::ui;

QmlTranslation::QmlTranslation(QObject* parent)
    : QObject(parent)
{
}

QString QmlTranslation::translate(const QString& context, const QString& text, const QString& disambiguation, int n) const
{
    return qtrc(context.toUtf8().constData(),
                text.toUtf8().constData(),
                disambiguation.isEmpty() ? nullptr : disambiguation.toUtf8().constData(),
                n);
}
