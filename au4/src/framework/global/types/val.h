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
#ifndef MU_GLOBAL_VAL_H
#define MU_GLOBAL_VAL_H

#include <string>
#include <variant>
#include <type_traits>
#include <vector>
#include <map>

#include "io/path.h"

#ifndef NO_QT_SUPPORT
#include <QString>
#include <QColor>
#include <QVariant>
#endif

namespace mu {
class Val;
using ValList = std::vector<Val>;
using ValMap = std::map<std::string, Val>;
class Val
{
public:
    enum class Type {
        Undefined = 0,
        Bool,
        Int,
        Int64,
        Double,
        String,
        List,
        Map,
#ifndef NO_QT_SUPPORT
        Color
#endif
    };

    Val() = default;
    explicit Val(bool val);
    explicit Val(int val);
    explicit Val(int64_t val);
    explicit Val(double val);
    explicit Val(const std::string& str);
    explicit Val(const char* str);
    explicit Val(const io::path_t& path);
    explicit Val(const ValList& list);
    explicit Val(const ValMap& map);

    template<class E, typename = std::enable_if_t<std::is_enum_v<E> > >
    explicit Val(E val)
        : Val{static_cast<std::underlying_type_t<E> >(val)}
    {
    }

    void setType(Type t);
    Type type() const;

    bool isNull() const;
    bool toBool() const;
    int toInt() const;
    int64_t toInt64() const;
    double toDouble() const;
    float toFloat() const;
    std::string toString() const;
    io::path_t toPath() const;
    ValList toList() const;
    ValMap toMap() const;

    template<class E, typename = std::enable_if_t<std::is_enum_v<E> > >
    E toEnum() const
    {
        return static_cast<E>(toInt());
    }

    bool operator ==(const Val& v) const;
    bool operator <(const Val& v) const;

#ifndef NO_QT_SUPPORT
    explicit Val(const QString& str);
    explicit Val(const QColor& color);

    QString toQString() const;
    QColor toQColor() const;

    QVariant toQVariant() const;
    static Val fromQVariant(const QVariant& var);
#endif

private:

    Type valueType() const;

    //! NOTE Should be sync with valueType method
    using Value = std::variant<std::monostate, bool, int, int64_t, double, std::string, ValList, ValMap>;
    Value m_val;
    Type m_type = Type::Undefined;
};
}

#endif // MU_GLOBAL_VAL_H
