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
#ifndef MU_API_JSRETVAL_H
#define MU_API_JSRETVAL_H

#include <QVariant>

#include "types/retval.h"
#include "io/path.h"

namespace mu::api {
#define JSVal QVariant
#define JSRet QVariantMap
#define JSRetVal QVariantMap

inline JSRet retToJs(const Ret& r)
{
    QVariantMap o;
    o["errcode"] = r.code();
    o["success"] = r.success();
    o["text"] = QString::fromStdString(r.text());
    return o;
}

inline Ret retFromJs(const JSRet& r)
{
    return Ret(r.value("errcode", 0).toInt(), r.value("text").toString().toStdString());
}

inline QVariant toQVariant(const QString& v)
{
    return QVariant::fromValue(v);
}

inline QVariant toQVariant(const io::path_t& v)
{
    return QVariant(v.toQString());
}

inline io::path_t fromQVariant(const QVariant& v)
{
    return v.toString();
}

inline QVariant toQVariant(const io::paths_t& v)
{
    QStringList l;
    l.reserve(int(v.size()));
    for (const io::path_t& p : v) {
        l << p.toQString();
    }
    return QVariant(l);
}

template<typename T>
inline JSRet retValToJs(const RetVal<T>& r)
{
    QVariantMap o = retToJs(r.ret);
    o["value"] = toQVariant(r.val);
    return o;
}

template<typename T>
inline RetVal<T> retValFromJs(const JSRet& r)
{
    RetVal<T> rv;
    rv.ret = retFromJs(r);
    rv.val = fromQVariant(r.value("value"));
    return rv;
}
}

#endif // MU_API_JSRETVAL_H
