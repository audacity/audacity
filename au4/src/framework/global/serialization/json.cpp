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
#include "json.h"

#include <cmath>
#include <cassert>

#define PICOJSON_USE_LOCALE 1
#define PICOJSON_ASSERT assert
#include "../thirdparty/picojson/picojson.h"

#include "log.h"

using namespace mu;

struct mu::JsonData
{
    picojson::value val;
};

static inline const picojson::value& val_const(const std::shared_ptr<JsonData>& d)
{
    return d->val;
}

static inline picojson::value& val_mut(std::shared_ptr<JsonData>& d)
{
    return d->val;
}

// =======================================
// JsonValue
// =======================================
JsonValue::JsonValue(bool v)
    : m_data(std::make_shared<JsonData>())
{
    val_mut(m_data).set<bool>(v);
}

JsonValue::JsonValue(int v)
    : m_data(std::make_shared<JsonData>())
{
    double d = static_cast<double>(v);
    val_mut(m_data).set<double>(d);
}

JsonValue::JsonValue(double v)
    : m_data(std::make_shared<JsonData>())
{
    val_mut(m_data).set<double>(v);
}

JsonValue::JsonValue(const String& v)
    : m_data(std::make_shared<JsonData>())
{
    val_mut(m_data).set<std::string>(v.toStdString());
}

JsonValue::JsonValue(const std::string& v)
    : m_data(std::make_shared<JsonData>())
{
    val_mut(m_data).set<std::string>(v);
}

JsonValue::JsonValue(const char* v)
    : m_data(std::make_shared<JsonData>())
{
    val_mut(m_data).set<std::string>(std::string(v));
}

JsonValue::JsonValue(std::shared_ptr<JsonData> d)
    : m_data(d)
{
    if (!m_data) {
        m_data = std::make_shared<JsonData>();
    }
}

void JsonValue::detach()
{
    if (!m_data) {
        return;
    }

    if (m_data.use_count() == 1) {
        return;
    }

    m_data = std::make_shared<JsonData>(*m_data);
}

bool JsonValue::isNull() const
{
    return val_const(m_data).is<picojson::null>();
}

void JsonValue::setNull()
{
    detach();
    val_mut(m_data) = picojson::value();
}

bool JsonValue::isBool() const
{
    return val_const(m_data).is<bool>();
}

bool JsonValue::toBool() const
{
    return val_const(m_data).evaluate_as_boolean();
}

JsonValue& JsonValue::operator=(bool v)
{
    detach();
    val_mut(m_data).set<bool>(v);
    return *this;
}

bool JsonValue::isNumber() const
{
    return val_const(m_data).is<double>();
}

int JsonValue::toInt() const
{
    if (!isNumber()) {
        return 0;
    }
    return static_cast<int>(std::round(val_const(m_data).get<double>()));
}

double JsonValue::toDouble() const
{
    if (!isNumber()) {
        return 0.0;
    }
    return val_const(m_data).get<double>();
}

JsonValue& JsonValue::operator=(int v)
{
    detach();
    double d = static_cast<double>(v);
    val_mut(m_data).set<double>(d);
    return *this;
}

JsonValue& JsonValue::operator=(double v)
{
    detach();
    val_mut(m_data).set<double>(v);
    return *this;
}

bool JsonValue::isString() const
{
    return val_const(m_data).is<std::string>();
}

String JsonValue::toString() const
{
    if (!isString()) {
        static String dummy;
        return dummy;
    }
    return String::fromStdString(val_const(m_data).get<std::string>());
}

const std::string& JsonValue::toStdString() const
{
    if (!isString()) {
        static std::string dummy;
        return dummy;
    }
    return val_const(m_data).get<std::string>();
}

JsonValue& JsonValue::operator=(const String& str)
{
    detach();
    val_mut(m_data).set<std::string>(str.toStdString());
    return *this;
}

JsonValue& JsonValue::operator=(const std::string& str)
{
    detach();
    val_mut(m_data).set<std::string>(str);
    return *this;
}

JsonValue& JsonValue::operator=(const char* str)
{
    detach();
    val_mut(m_data).set<std::string>(std::string(str));
    return *this;
}

bool JsonValue::isArray() const
{
    return val_const(m_data).is<picojson::array>();
}

JsonArray JsonValue::toArray() const
{
    return JsonArray(m_data);
}

JsonValue& JsonValue::operator=(const JsonArray& arr)
{
    detach();
    val_mut(m_data) = val_const(arr.m_data);
    return *this;
}

bool JsonValue::isObject() const
{
    return val_const(m_data).is<picojson::object>();
}

JsonObject JsonValue::toObject() const
{
    return JsonObject(m_data);
}

JsonValue& JsonValue::operator=(const JsonObject& obj)
{
    detach();
    val_mut(m_data) = val_const(obj.m_data);
    return *this;
}

// =======================================
// JsonValueRef
// =======================================

JsonValueRef::JsonValueRef(const std::string& key, JsonObject* o)
    : m_key(key), m_object(o)
{
}

JsonValueRef::JsonValueRef(size_t i, JsonArray* a)
    : m_idx(i), m_array(a)
{
}

JsonValueRef& JsonValueRef::operator=(bool v)
{
    if (m_object) {
        m_object->set(m_key, v);
    } else if (m_array) {
        m_array->set(m_idx, v);
    }
    return *this;
}

JsonValueRef& JsonValueRef::operator=(int v)
{
    if (m_object) {
        m_object->set(m_key, v);
    } else if (m_array) {
        m_array->set(m_idx, v);
    }
    return *this;
}

JsonValueRef& JsonValueRef::operator=(double v)
{
    if (m_object) {
        m_object->set(m_key, v);
    } else if (m_array) {
        m_array->set(m_idx, v);
    }
    return *this;
}

JsonValueRef& JsonValueRef::operator=(const String& v)
{
    if (m_object) {
        m_object->set(m_key, v);
    } else if (m_array) {
        m_array->set(m_idx, v);
    }
    return *this;
}

JsonValueRef& JsonValueRef::operator=(const std::string& v)
{
    if (m_object) {
        m_object->set(m_key, v);
    } else if (m_array) {
        m_array->set(m_idx, v);
    }
    return *this;
}

JsonValueRef& JsonValueRef::operator=(const char* v)
{
    if (m_object) {
        m_object->set(m_key, v);
    } else if (m_array) {
        m_array->set(m_idx, v);
    }
    return *this;
}

JsonValueRef& JsonValueRef::operator=(const JsonValue& v)
{
    if (m_object) {
        m_object->set(m_key, v);
    } else if (m_array) {
        m_array->set(m_idx, v);
    }
    return *this;
}

JsonValueRef& JsonValueRef::operator=(const JsonArray& v)
{
    if (m_object) {
        m_object->set(m_key, v);
    } else if (m_array) {
        m_array->set(m_idx, v);
    }
    return *this;
}

JsonValueRef& JsonValueRef::operator=(const JsonObject& v)
{
    if (m_object) {
        m_object->set(m_key, v);
    } else if (m_array) {
        m_array->set(m_idx, v);
    }
    return *this;
}

// =======================================
// JsonArray
// =======================================

static inline const picojson::array& array_const(const std::shared_ptr<JsonData>& d)
{
    return d->val.get<picojson::array>();
}

static inline picojson::array& array_mut(std::shared_ptr<JsonData>& d)
{
    return d->val.get<picojson::array>();
}

JsonArray::JsonArray(std::shared_ptr<JsonData> d)
    : m_data(d)
{
    if (!m_data) {
        m_data = std::make_shared<JsonData>();
        m_data->val = picojson::value(picojson::array());
    }
}

JsonArray::JsonArray(std::initializer_list<JsonValue> args)
{
    m_data = std::make_shared<JsonData>();
    m_data->val = picojson::value(picojson::array());

    array_mut(m_data).reserve(args.size());
    for (const JsonValue& v : args) {
        array_mut(m_data).push_back(v.m_data->val);
    }
}

void JsonArray::detach()
{
    if (!m_data) {
        return;
    }

    if (m_data.use_count() == 1) {
        return;
    }

    m_data = std::make_shared<JsonData>(*m_data);
}

size_t JsonArray::size() const
{
    return array_const(m_data).size();
}

void JsonArray::resize(size_t i)
{
    detach();
    array_mut(m_data).resize(i);
}

JsonValue JsonArray::at(size_t i) const
{
    std::shared_ptr<JsonData> d = std::make_shared<JsonData>();
    d->val = array_const(m_data).at(i);
    return JsonValue(d);
}

JsonArray& JsonArray::set(size_t i, bool v)
{
    detach();
    array_mut(m_data)[i] = picojson::value(v);
    return *this;
}

JsonArray& JsonArray::set(size_t i, int v)
{
    detach();
    array_mut(m_data)[i] = picojson::value(static_cast<double>(v));
    return *this;
}

JsonArray& JsonArray::set(size_t i, double v)
{
    detach();
    array_mut(m_data)[i] = picojson::value(v);
    return *this;
}

JsonArray& JsonArray::set(size_t i, const String& str)
{
    detach();
    array_mut(m_data)[i] = picojson::value(str.toStdString());
    return *this;
}

JsonArray& JsonArray::set(size_t i, const std::string& str)
{
    detach();
    array_mut(m_data)[i] = picojson::value(str);
    return *this;
}

JsonArray& JsonArray::set(size_t i, const char* str)
{
    detach();
    array_mut(m_data)[i] = picojson::value(std::string(str));
    return *this;
}

JsonArray& JsonArray::set(size_t i, const JsonValue& v)
{
    detach();
    array_mut(m_data)[i] = v.m_data->val;
    return *this;
}

JsonArray& JsonArray::set(size_t i, const JsonArray& v)
{
    detach();
    array_mut(m_data)[i] = v.m_data->val;
    return *this;
}

JsonArray& JsonArray::set(size_t i, const JsonObject& v)
{
    detach();
    array_mut(m_data)[i] = v.m_data->val;
    return *this;
}

JsonArray& JsonArray::append(bool v)
{
    detach();
    array_mut(m_data).push_back(picojson::value(v));
    return *this;
}

JsonArray& JsonArray::append(int v)
{
    detach();
    array_mut(m_data).push_back(picojson::value(static_cast<double>(v)));
    return *this;
}

JsonArray& JsonArray::append(double v)
{
    detach();
    array_mut(m_data).push_back(picojson::value(v));
    return *this;
}

JsonArray& JsonArray::append(const String& str)
{
    detach();
    array_mut(m_data).push_back(picojson::value(str.toStdString()));
    return *this;
}

JsonArray& JsonArray::append(const std::string& str)
{
    detach();
    array_mut(m_data).push_back(picojson::value(str));
    return *this;
}

JsonArray& JsonArray::append(const char* str)
{
    detach();
    array_mut(m_data).push_back(picojson::value(std::string(str)));
    return *this;
}

JsonArray& JsonArray::append(const JsonValue& v)
{
    detach();
    array_mut(m_data).push_back(val_const(v.m_data));
    return *this;
}

JsonArray& JsonArray::append(const JsonArray& v)
{
    detach();
    array_mut(m_data).push_back(val_const(v.m_data));
    return *this;
}

JsonArray& JsonArray::append(const JsonObject& v)
{
    detach();
    array_mut(m_data).push_back(val_const(v.m_data));
    return *this;
}

JsonValue JsonArray::operator [](size_t i) const
{
    return at(i);
}

JsonValueRef JsonArray::operator [](size_t i)
{
    return JsonValueRef(i, this);
}

// =======================================
// JsonObject
// =======================================
static const picojson::object& object_const(const std::shared_ptr<JsonData>& d)
{
    return d->val.get<picojson::object>();
}

static picojson::object& object_mut(std::shared_ptr<JsonData>& d)
{
    return d->val.get<picojson::object>();
}

JsonObject::JsonObject(std::shared_ptr<JsonData> d)
    : m_data(d)
{
    if (!m_data) {
        m_data = std::make_shared<JsonData>();
        m_data->val = picojson::value(picojson::object());
    }
}

void JsonObject::detach()
{
    if (!m_data) {
        return;
    }

    if (m_data.use_count() == 1) {
        return;
    }

    m_data = std::make_shared<JsonData>(*m_data);
}

bool JsonObject::isValid() const
{
    return val_const(m_data).is<picojson::object>();
}

bool JsonObject::empty() const
{
    return object_const(m_data).size() == 0;
}

size_t JsonObject::size() const
{
    return object_const(m_data).size();
}

bool JsonObject::contains(const std::string& key) const
{
    const picojson::object& o = object_const(m_data);
    return o.find(key) != o.cend();
}

JsonValue JsonObject::value(const std::string& key, JsonValue def) const
{
    const picojson::object& o = object_const(m_data);
    auto it = o.find(key);
    if (it != o.cend()) {
        std::shared_ptr<JsonData> d = std::make_shared<JsonData>();
        d->val = it->second;
        return JsonValue(d);
    }
    return def;
}

JsonObject& JsonObject::set(const std::string& key, bool v)
{
    detach();
    object_mut(m_data).insert_or_assign(key, picojson::value(v));
    return *this;
}

JsonObject& JsonObject::set(const std::string& key, int v)
{
    detach();
    object_mut(m_data).insert_or_assign(key, picojson::value(static_cast<double>(v)));
    return *this;
}

JsonObject& JsonObject::set(const std::string& key, double v)
{
    detach();
    object_mut(m_data).insert_or_assign(key, picojson::value(v));
    return *this;
}

JsonObject& JsonObject::set(const std::string& key, const std::string& v)
{
    detach();
    object_mut(m_data).insert_or_assign(key, picojson::value(v));
    return *this;
}

JsonObject& JsonObject::set(const std::string& key, const char* str)
{
    detach();
    object_mut(m_data).insert_or_assign(key, picojson::value(std::string(str)));
    return *this;
}

JsonObject& JsonObject::set(const std::string& key, const String& str)
{
    detach();
    object_mut(m_data).insert_or_assign(key, picojson::value(str.toStdString()));
    return *this;
}

JsonObject& JsonObject::set(const std::string& key, const JsonValue& v)
{
    detach();
    object_mut(m_data).insert_or_assign(key, v.m_data->val);
    return *this;
}

JsonObject& JsonObject::set(const std::string& key, const JsonArray& v)
{
    detach();
    object_mut(m_data).insert_or_assign(key, v.m_data->val);
    return *this;
}

JsonObject& JsonObject::set(const std::string& key, const JsonObject& v)
{
    detach();
    object_mut(m_data).insert_or_assign(key, v.m_data->val);
    return *this;
}

JsonValue JsonObject::operator [](const std::string& key) const
{
    return value(key);
}

JsonValueRef JsonObject::operator [](const std::string& key)
{
    return JsonValueRef(key, this);
}

std::vector<std::string> JsonObject::keys() const
{
    std::vector<std::string> result;
    const picojson::object& o = object_const(m_data);
    for (auto&& p : o) {
        result.push_back(p.first);
    }
    return result;
}

// =======================================
// JsonDocument
// =======================================
JsonDocument::JsonDocument(std::shared_ptr<JsonData> d)
    : m_data(d)
{
}

JsonDocument::JsonDocument(JsonObject o)
    : m_data(o.m_data)
{
}

JsonDocument::JsonDocument(JsonArray a)
    : m_data(a.m_data)
{
}

ByteArray JsonDocument::toJson(Format format) const
{
    std::string json = m_data->val.serialize(format == Format::Indented);
    return ByteArray(json.c_str(), json.size());
}

JsonDocument JsonDocument::fromJson(const ByteArray& ba, std::string* err)
{
    std::shared_ptr<JsonData> d = std::make_shared<JsonData>();

    std::string_view json(ba.constChar(), ba.size());

    const char* currentLoc = setlocale(LC_NUMERIC, "C");
    picojson::parse(d->val, json.begin(), json.end(), err);
    setlocale(LC_NUMERIC, currentLoc);

    JsonDocument doc(d);
    return doc;
}

bool JsonDocument::isObject() const
{
    return m_data->val.is<picojson::object>();
}

bool JsonDocument::isArray() const
{
    return m_data->val.is<picojson::array>();
}

JsonObject JsonDocument::rootObject() const
{
    return JsonObject(m_data);
}

JsonArray JsonDocument::rootArray() const
{
    return JsonArray(m_data);
}
