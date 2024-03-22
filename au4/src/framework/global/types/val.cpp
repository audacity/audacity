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
#include "val.h"

#include <sstream>
#include <iomanip>

#include "log.h"

using namespace mu;

static const std::string VAL_TRUE("true");
static const std::string VAL_FALSE("false");

Val::Val(bool val)
    : m_val(val), m_type(Type::Bool) {}

Val::Val(int val)
    : m_val(val), m_type(Type::Int) {}

Val::Val(int64_t val)
    : m_val(val), m_type(Type::Int64) {}

Val::Val(double val)
    : m_val(val), m_type(Type::Double) {}

Val::Val(const std::string& str)
    : m_val(str), m_type(Type::String) {}

Val::Val(const char* str)
    : m_val(std::string(str)), m_type(Type::String) {}

Val::Val(const io::path_t& path)
    : m_val(path.toStdString()), m_type(Type::String) {}

Val::Val(const ValList& list)
    : m_val(list), m_type(Type::List) {}

Val::Val(const ValMap& map)
    : m_val(map), m_type(Type::Map) {}

Val::Type Val::valueType() const
{
    // std::monostate, bool, int, int64_t, double, std::string, ValList, ValMap
    switch (m_val.index()) {
    case 0: return Type::Undefined;
    case 1: return Type::Bool;
    case 2: return Type::Int;
    case 3: return Type::Int64;
    case 4: return Type::Double;
    case 5: return Type::String;
    case 6: return Type::List;
    case 7: return Type::Map;
    }
    return Type::Undefined;
}

void Val::setType(Type t)
{
    m_type = t;
}

Val::Type Val::type() const
{
    return m_type;
}

bool Val::isNull() const
{
    return m_val.index() == 0;
}

bool Val::toBool() const
{
    switch (valueType()) {
    case Type::Bool: return std::get<bool>(m_val);
    case Type::Int: return std::get<int>(m_val) > 0;
    case Type::Int64: return std::get<int64_t>(m_val) > 0;
    case Type::Double: return std::get<double>(m_val) > 0.0;
    case Type::String: {
        std::string str = std::get<std::string>(m_val);
        return str == VAL_TRUE;
    }
    default:
        break;
    }
    return isNull() ? false : true;
}

int Val::toInt() const
{
    switch (valueType()) {
    case Type::Bool: return toBool() ? 1 : 0;
    case Type::Int: return std::get<int>(m_val);
    case Type::Int64: return static_cast<int>(toInt64());
    case Type::Double: return static_cast<int>(toDouble());
    case Type::String: {
        std::string str = std::get<std::string>(m_val);
        return std::stoi(str);
    }
    default:
        break;
    }
    return 0;
}

int64_t Val::toInt64() const
{
    switch (valueType()) {
    case Type::Bool: return toBool() ? 1 : 0;
    case Type::Int: return toInt();
    case Type::Int64: return std::get<int64_t>(m_val);
    case Type::Double: return static_cast<int>(toDouble());
    case Type::String: {
        std::string str = std::get<std::string>(m_val);
        return std::stod(str);
    }
    default:
        break;
    }
    return 0;
}

double Val::toDouble() const
{
    switch (valueType()) {
    case Type::Bool: return toBool() ? 1.0 : 0.0;
    case Type::Int: return static_cast<double>(toInt());
    case Type::Int64: return static_cast<double>(toInt64());
    case Type::Double: return std::get<double>(m_val);
    case Type::String: {
        std::string str = std::get<std::string>(m_val);
        return std::stod(str);
    }
    default:
        break;
    }
    return 0.0;
}

float Val::toFloat() const
{
    return static_cast<float>(toDouble());
}

static std::string doubleToString(double n, int prec = 6)
{
    std::stringstream stream;
    stream << std::fixed << std::setprecision(prec) << n;
    std::string s = stream.str();

    // remove extra '0'
    size_t correctedIdx = s.size() - 1;
    if (s.back() == '0') {
        for (int i = static_cast<int>(s.size() - 1); i > 0; --i) {
            if (s.at(i) != '0') {
                correctedIdx = i;
                break;
            }
        }
    }

    if (s.at(correctedIdx) == '.') {
        --correctedIdx;
    }

    return std::string(s.c_str(), correctedIdx + 1);
}

std::string Val::toString() const
{
    switch (valueType()) {
    case Type::Bool: return toBool() ? VAL_TRUE : VAL_FALSE;
    case Type::Int: return std::to_string(toInt());
    case Type::Double: return doubleToString(toDouble());
    case Type::String: return std::get<std::string>(m_val);
    default:
        break;
    }
    return std::string();
}

io::path_t Val::toPath() const
{
    if (valueType() == Type::String) {
        return std::get<std::string>(m_val);
    }
    return std::string();
}

ValList Val::toList() const
{
    if (valueType() == Type::List) {
        return std::get<ValList>(m_val);
    }
    return ValList();
}

ValMap Val::toMap() const
{
    if (valueType() == Type::Map) {
        return std::get<ValMap>(m_val);
    }
    return ValMap();
}

bool Val::operator ==(const Val& v) const
{
    return v.m_type == m_type && v.m_val == m_val;
}

bool Val::operator <(const Val& v) const
{
    if (valueType() != v.valueType()) {
        return false;
    }

    switch (valueType()) {
    case Type::Undefined: return false;
    case Type::Bool: return toBool() < v.toBool();
    case Type::Int: return toInt() < v.toInt();
    case Type::Int64: return toInt64() < v.toInt64();
    case Type::Double: return toDouble() < v.toDouble();
    case Type::String: return toString() < v.toString();
    case Type::List: return toList() < v.toList();
    case Type::Map: return toMap() < v.toMap();
#ifndef NO_QT_SUPPORT
    case Type::Color: return toString() < v.toString();
#endif
    }
    return false;
}

#ifndef NO_QT_SUPPORT
Val::Val(const QString& str)
    : m_val(str.toStdString()), m_type(Type::String) {}

Val::Val(const QColor& color)
    : m_val(color.name().toStdString()), m_type(Type::Color) {}

QString Val::toQString() const
{
    return QString::fromStdString(toString());
}

QColor Val::toQColor() const
{
    std::string str = toString();
    return QColor(str.c_str());
}

QVariant Val::toQVariant() const
{
    switch (m_type) {
    case Val::Type::Undefined: return QVariant();
    case Val::Type::Bool: return QVariant(toBool());
    case Val::Type::Int: return QVariant(toInt());
    case Val::Type::Int64: return QVariant(static_cast<qlonglong>(toInt64()));
    case Val::Type::Double: return QVariant(toDouble());
    case Val::Type::String: return QVariant(toQString());
    case Val::Type::Color: return QVariant(toQColor());
    case Val::Type::List: {
        QVariantList vl;
        ValList l = toList();
        for (const Val& v : l) {
            vl << v.toQVariant();
        }
        return vl;
    }
    case Val::Type::Map: {
        QVariantMap vm;
        ValMap m = toMap();
        for (const auto& p : m) {
            vm.insert(QString::fromStdString(p.first), p.second.toQVariant());
        }
        return vm;
    }
    }
    return QVariant();
}

Val Val::fromQVariant(const QVariant& var)
{
    if (!var.isValid()) {
        return Val();
    }

#ifdef MU_QT5_COMPAT
    switch (var.type()) {
    case QVariant::Bool: return Val(var.toBool());
    case QVariant::Int: return Val(var.toInt());
    case QVariant::UInt: return Val(var.toInt());
    case QVariant::LongLong: return Val(static_cast<int64_t>(var.toLongLong()));
    case QVariant::ULongLong: return Val(static_cast<int64_t>(var.toLongLong()));
    case QVariant::Double: return Val(var.toDouble());
    case QVariant::String: return Val(var.toString().toStdString());
    case QVariant::List: {
        ValList l;
        QVariantList vl = var.toList();
        for (const QVariant& v : vl) {
            l.push_back(fromQVariant(v));
        }
        return Val(l);
    }
    case QVariant::Map: {
        ValMap m;
        QVariantMap vm = var.toMap();
        QVariantMap::const_iterator i = vm.constBegin();
        while (i != vm.constEnd()) {
            m.insert({ i.key().toStdString(), fromQVariant(i.value()) });
            ++i;
        }
        return Val(m);
    }
    case QVariant::Color: return Val(var.value<QColor>());
    default: {
        LOGE() << "Not supported type: " << var.typeName();
        //UNREACHABLE;
        return Val();
    }
    }
#else
    switch (var.typeId()) {
    case QMetaType::Bool: return Val(var.toBool());
    case QMetaType::Int: return Val(var.toInt());
    case QMetaType::UInt: return Val(var.toInt());
    case QMetaType::LongLong: return Val(static_cast<int64_t>(var.toLongLong()));
    case QMetaType::ULongLong: return Val(static_cast<int64_t>(var.toLongLong()));
    case QMetaType::Double: return Val(var.toDouble());
    case QMetaType::QString: return Val(var.toString().toStdString());
    case QMetaType::QVariantList: {
        ValList l;
        QVariantList vl = var.toList();
        for (const QVariant& v : vl) {
            l.push_back(fromQVariant(v));
        }
        return Val(l);
    }
    case QMetaType::QVariantMap: {
        ValMap m;
        QVariantMap vm = var.toMap();
        QVariantMap::const_iterator i = vm.constBegin();
        while (i != vm.constEnd()) {
            m.insert({ i.key().toStdString(), fromQVariant(i.value()) });
            ++i;
        }
        return Val(m);
    }
    case QMetaType::QColor: return Val(var.value<QColor>());
    default: {
        LOGE() << "Not supported type: " << var.typeName();
        //UNREACHABLE;
        return Val();
    }
    }
#endif
}

#endif
