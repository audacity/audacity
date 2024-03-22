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
#ifndef MU_GLOBAL_JSON_H
#define MU_GLOBAL_JSON_H

#include <memory>

#include "types/bytearray.h"
#include "types/string.h"

namespace mu {
struct JsonData;
class JsonArray;
class JsonObject;
class JsonValue
{
public:
    JsonValue(bool v);
    JsonValue(int v);
    JsonValue(double v);
    JsonValue(const String& v);
    JsonValue(const std::string& v);
    JsonValue(const char* v);
    JsonValue(std::shared_ptr<JsonData> d = nullptr);

    bool isNull() const;
    bool isBool() const;
    bool isNumber() const;
    bool isString() const;
    bool isArray() const;
    bool isObject() const;

    bool toBool() const;
    int toInt() const;
    double toDouble() const;
    String toString() const;
    const std::string& toStdString() const;
    JsonArray toArray() const;
    JsonObject toObject() const;

    void setNull();
    JsonValue& operator=(bool v);
    JsonValue& operator=(int v);
    JsonValue& operator=(double v);
    JsonValue& operator=(const String& str);
    JsonValue& operator=(const std::string& str);
    JsonValue& operator=(const char* str);
    JsonValue& operator=(const JsonArray& arr);
    JsonValue& operator=(const JsonObject& obj);

private:
    friend class JsonObject;
    friend class JsonArray;

    void detach();
    std::shared_ptr<JsonData> m_data;
};

class JsonValueRef
{
public:
    JsonValueRef(const std::string& key, JsonObject* o);
    JsonValueRef(size_t i, JsonArray* a);

    JsonValueRef& operator=(bool v);
    JsonValueRef& operator=(int v);
    JsonValueRef& operator=(double v);
    JsonValueRef& operator=(const String& str);
    JsonValueRef& operator=(const std::string& str);
    JsonValueRef& operator=(const char* str);
    JsonValueRef& operator=(const JsonValue& v);
    JsonValueRef& operator=(const JsonArray& arr);
    JsonValueRef& operator=(const JsonObject& obj);

private:
    std::string m_key;
    JsonObject* m_object = nullptr;

    size_t m_idx = 0;
    JsonArray* m_array = nullptr;
};

class JsonArray
{
public:

    JsonArray(std::shared_ptr<JsonData> d = nullptr);
    JsonArray(std::initializer_list<JsonValue> args);

    size_t size() const;
    void resize(size_t i);

    JsonValue at(size_t i) const;

    JsonArray& set(size_t i, bool v);
    JsonArray& set(size_t i, int v);
    JsonArray& set(size_t i, double v);
    JsonArray& set(size_t i, const String& str);
    JsonArray& set(size_t i, const std::string& str);
    JsonArray& set(size_t i, const char* str);
    JsonArray& set(size_t i, const JsonValue& v);
    JsonArray& set(size_t i, const JsonArray& v);
    JsonArray& set(size_t i, const JsonObject& v);

    JsonArray& append(bool v);
    JsonArray& append(int v);
    JsonArray& append(double v);
    JsonArray& append(const String& str);
    JsonArray& append(const std::string& str);
    JsonArray& append(const char* str);
    JsonArray& append(const JsonValue& v);
    JsonArray& append(const JsonArray& v);
    JsonArray& append(const JsonObject& v);

    JsonArray& operator <<(const JsonValue& v) { append(v); return *this; }
    JsonArray& operator <<(const JsonArray& v) { append(v); return *this; }
    JsonArray& operator <<(const JsonObject& v) { append(v); return *this; }

    JsonValue operator [](size_t i) const;
    JsonValueRef operator [](size_t i);

private:
    friend class JsonValue;
    friend class JsonObject;
    friend class JsonDocument;

    void detach();
    std::shared_ptr<JsonData> m_data;
};

class JsonObject
{
public:
    JsonObject(std::shared_ptr<JsonData> d = nullptr);

    bool isValid() const;

    bool empty() const;
    size_t size() const;
    std::vector<std::string> keys() const;
    bool contains(const std::string& key) const;
    JsonValue value(const std::string& key, JsonValue def = JsonValue()) const;

    JsonObject& set(const std::string& key, bool v);
    JsonObject& set(const std::string& key, int v);
    JsonObject& set(const std::string& key, double v);
    JsonObject& set(const std::string& key, const String& str);
    JsonObject& set(const std::string& key, const std::string& str);
    JsonObject& set(const std::string& key, const char* str);
    JsonObject& set(const std::string& key, const JsonValue& v);
    JsonObject& set(const std::string& key, const JsonArray& v);
    JsonObject& set(const std::string& key, const JsonObject& v);

    JsonValue operator [](const std::string& key) const;
    JsonValueRef operator [](const std::string& key);

private:
    friend class JsonValue;
    friend class JsonArray;
    friend class JsonDocument;

    void detach();
    std::shared_ptr<JsonData> m_data;
};

class JsonDocument
{
public:
    JsonDocument(std::shared_ptr<JsonData> d = nullptr);
    JsonDocument(JsonObject o);
    JsonDocument(JsonArray a);

    enum class Format {
        Indented,
        Compact
    };

    bool isObject() const;
    bool isArray() const;
    JsonObject rootObject() const;
    JsonArray rootArray() const;

    ByteArray toJson(Format format = Format::Indented) const;
    static JsonDocument fromJson(const ByteArray& ba, std::string* err = nullptr);

private:
    std::shared_ptr<JsonData> m_data;
};
}

#endif // MU_GLOBAL_JSON_H
