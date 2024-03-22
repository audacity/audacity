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
#include "string.h"

#include <algorithm>
#include <cstring>
#include <cstdlib>
#include <locale>
#include <cctype>
#include <iomanip>

#include "../thirdparty/utfcpp-3.2.1/utf8.h"

#include "log.h"

using namespace mu;

constexpr unsigned char U8_BOM[] = { 239, 187, 191 };
constexpr unsigned char U16LE_BOM[] = { 255, 254 };
constexpr unsigned char U16BE_BOM[] = { 254, 255 };

// Helpers

static long int toInt_helper(const char* str, bool* ok, int base)
{
    if (!str || std::strlen(str) == 0) {
        if (ok) {
            *ok = false;
        }
        return 0;
    }
    const char* currentLoc = setlocale(LC_NUMERIC, "C");
    char* end = nullptr;
    long int v = static_cast<int>(std::strtol(str, &end, base));
    setlocale(LC_NUMERIC, currentLoc);
    bool myOk = std::strlen(end) == 0;
    if (!myOk) {
        v = 0;
    }

    if (ok) {
        *ok = myOk;
    }
    return v;
}

static double toDouble_helper(const char* str, bool* ok)
{
    if (!str) {
        return 0.0;
    }
    const char* currentLoc = setlocale(LC_NUMERIC, "C");
    char* end = nullptr;
    double v = std::strtod(str, &end);
    setlocale(LC_NUMERIC, currentLoc);
    if (ok) {
        size_t sz = std::strlen(end);
        *ok = sz != std::strlen(str);
    }
    return v;
}

static void ltrim_helper(std::u16string& s)
{
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](char16_t ch) {
        return !std::isspace(ch);
    }));
}

static void rtrim_helper(std::u16string& s)
{
    s.erase(std::find_if(s.rbegin(), s.rend(), [](char16_t ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

static void trim_helper(std::u16string& s)
{
    ltrim_helper(s);
    rtrim_helper(s);
}

// ============================
// Char
// ============================
char Char::toAscii(char16_t c, bool* ok)
{
    if (isAscii(c)) {
        if (ok) {
            *ok = true;
        }
        return static_cast<char>(c);
    } else {
        if (ok) {
            *ok = false;
        }
        return '?';
    }
}

bool Char::isLetter(char16_t c)
{
    //! TODO
#ifndef NO_QT_SUPPORT
    return QChar::isLetter(c);
#else
    return std::isalpha(static_cast<unsigned char>(c));
#endif
}

bool Char::isSpace(char16_t c)
{
    //! TODO
#ifndef NO_QT_SUPPORT
    return QChar::isSpace(c);
#else
    return std::isspace(static_cast<unsigned char>(c));
#endif
}

bool Char::isDigit(char16_t c)
{
    return c >= u'0' && c <= u'9';
}

int Char::digitValue() const
{
    if (!isDigit()) {
        return -1;
    }
    return m_ch - '0';
}

bool Char::isPunct(char16_t ch)
{
#ifndef NO_QT_SUPPORT
    return QChar::isPunct(ch);
#else
    return std::ispunct(static_cast<unsigned char>(ch));
#endif
}

char16_t Char::toLower(char16_t ch)
{
    //! TODO
#ifndef NO_QT_SUPPORT
    return QChar::toLower(ch);
#else
    return std::tolower(static_cast<unsigned char>(ch));
#endif
}

char16_t Char::toUpper(char16_t ch)
{
    //! TODO
#ifndef NO_QT_SUPPORT
    return QChar::toUpper(ch);
#else
    return std::toupper(static_cast<unsigned char>(ch));
#endif
}

// ============================
// UtfCodec
// ============================
UtfCodec::Encoding UtfCodec::xmlEncoding(const ByteArray& data)
{
    if (data.size() < 3) {
        return Encoding::Unknown;
    }

    // Check Bom
    if (std::memcmp(data.constChar(), U8_BOM, 3) == 0) {
        return Encoding::UTF_8;
    }

    if (std::memcmp(data.constChar(), U16LE_BOM, 2) == 0) {
        return Encoding::UTF_16LE;
    }

    if (std::memcmp(data.constChar(), U16BE_BOM, 2) == 0) {
        return Encoding::UTF_16BE;
    }

    // Check content
    //! NOTE For XML we know that the content starts with an ascii character '<',
    //! it takes up 8 bits, so the remaining bits will be zero if the encoding is greater than UTF-8
    //! (for other content type this may not be true)
    const uint8_t* d = data.constData();
    if (d[0] != 0 && d[1] != 0) {
        return Encoding::UTF_8;
    }

    if (d[0] != 0 && d[1] == 0) {
        return Encoding::UTF_16LE;
    }

    if (d[0] == 0 && d[1] != 0) {
        return Encoding::UTF_16BE;
    }

    return Encoding::Unknown;
}

void UtfCodec::utf8to16(std::string_view src, std::u16string& dst)
{
    try {
        utf8::utf8to16(src.begin(), src.end(), std::back_inserter(dst));
    } catch (const std::exception& e) {
        LOGE() << e.what();
    }
}

void UtfCodec::utf16to8(std::u16string_view src, std::string& dst)
{
    try {
        utf8::utf16to8(src.begin(), src.end(), std::back_inserter(dst));
    } catch (const std::exception& e) {
        LOGE() << e.what();
    }
}

void UtfCodec::utf8to32(std::string_view src, std::u32string& dst)
{
    try {
        utf8::utf8to32(src.begin(), src.end(), std::back_inserter(dst));
    } catch (const std::exception& e) {
        LOGE() << e.what();
    }
}

void UtfCodec::utf32to8(std::u32string_view src, std::string& dst)
{
    try {
        utf8::utf32to8(src.begin(), src.end(), std::back_inserter(dst));
    } catch (const std::exception& e) {
        LOGE() << e.what();
    }
}

bool UtfCodec::isValidUtf8(const std::string_view& src)
{
    return utf8::is_valid(src.begin(), src.end());
}

// ============================
// String
// ============================

String::String()
{
    m_data = std::make_shared<std::u16string>();
}

String::String(const char16_t* str)
{
    m_data = std::make_shared<std::u16string>(str ? str : u"");
#ifdef STRING_DEBUG_HACK
    updateDebugView();
#endif
}

String::String(const Char& ch)
{
    m_data = std::make_shared<std::u16string>();
    *m_data.get() += ch.unicode();
#ifdef STRING_DEBUG_HACK
    updateDebugView();
#endif
}

String::String(const Char* unicode, size_t size)
{
    if (!unicode) {
        m_data = std::make_shared<std::u16string>();
        return;
    }

    static_assert(sizeof(Char) == sizeof(char16_t));
    const char16_t* str = reinterpret_cast<const char16_t*>(unicode);
    if (size == mu::nidx) {
        m_data = std::make_shared<std::u16string>(str);
    } else {
        m_data = std::make_shared<std::u16string>(str, size);
    }

#ifdef STRING_DEBUG_HACK
    updateDebugView();
#endif
}

#ifdef STRING_DEBUG_HACK
void String::updateDebugView()
{
    try {
        dview = toStdString();
    }  catch (const std::exception& e) {
        dview = "exception: " + std::string(e.what());
    }
}

#endif

const std::u16string& String::constStr() const
{
    return *m_data.get();
}

struct String::Mutator {
    std::u16string& s;
    String* self = nullptr;

    Mutator(std::u16string& s, String* self)
        : s(s), self(self) {}
    ~Mutator()
    {
#ifdef STRING_DEBUG_HACK
        self->updateDebugView();
#endif
    }

    operator std::u16string& () {
        return s;
    }

    void reserve(size_t n) { s.reserve(n); }
    void resize(size_t n) { s.resize(n); }
    void clear() { s.clear(); }
    void push_back(char16_t c) { s.push_back(c); }
    void insert(size_t p, const std::u16string& v) { s.insert(p, v); }
    void erase(size_t p, size_t n) { s.erase(p, n); }

    std::u16string& operator=(const std::u16string& v) { return s.operator=(v); }
    std::u16string& operator=(const char16_t* v) { return s.operator=(v); }
    std::u16string& operator=(const char16_t v) { return s.operator=(v); }

    std::u16string& operator+=(const std::u16string& v) { return s.operator+=(v); }
    std::u16string& operator+=(const char16_t* v) { return s.operator+=(v); }
    std::u16string& operator+=(const char16_t v) { return s.operator+=(v); }
    char16_t& operator[](size_t i) { return s.operator[](i); }
};

String::Mutator String::mutStr(bool do_detach)
{
    if (do_detach) {
        detach();
    }
    return Mutator(*m_data.get(), this);
}

void String::reserve(size_t i)
{
    mutStr().reserve(i);
}

bool String::operator ==(const AsciiStringView& s) const
{
    if (size() != s.size()) {
        return false;
    }

    for (size_t i = 0; i < s.size(); ++i) {
        if (at(i).toAscii() != s.at(i).ascii()) {
            return false;
        }
    }
    return true;
}

bool String::operator ==(const char* s) const
{
    if (!s) {
        return false;
    }

    std::string_view v(s);
    if (size() != v.size()) {
        return false;
    }

    for (size_t i = 0; i < v.size(); ++i) {
        IF_ASSERT_FAILED(Char::isAscii(v.at(i))) {
        }
        if (at(i).toAscii() != v.at(i)) {
            return false;
        }
    }
    return true;
}

void String::detach()
{
    if (!m_data) {
        return;
    }

    if (m_data.use_count() == 1) {
        return;
    }

    m_data = std::make_shared<std::u16string>(*m_data);
}

String& String::operator=(const char16_t* str)
{
    mutStr() = str;
    return *this;
}

String& String::operator +=(const char16_t* s)
{
    mutStr() += s;
    return *this;
}

char16_t String::operator [](size_t i) const
{
    return constStr()[i];
}

char16_t& String::operator [](size_t i)
{
    return mutStr()[i];
}

String& String::append(Char ch)
{
    mutStr() += ch.unicode();
    return *this;
}

String& String::append(const String& s)
{
    mutStr() += s.constStr();
    return *this;
}

String& String::prepend(Char ch)
{
    mutStr() = ch.unicode() + constStr();
    return *this;
}

String& String::prepend(const String& s)
{
    mutStr() = s.constStr() + constStr();
    return *this;
}

String String::fromUtf16LE(const ByteArray& data)
{
    //make sure len is divisible by 2
    size_t len = data.size();
    if (len % 2) {
        len--;
    }

    if (len < 2) {
        return String();
    }

    String u16;
    u16.reserve(len / 2);
    String::Mutator mut = u16.mutStr();

    const uint8_t* d = data.constData();
    size_t start = 0;
    if (std::memcmp(d, U16LE_BOM, 2) == 0) {
        start += 2;
    }

    for (size_t i = start; i < len;) {
        //little-endian
        int lo = d[i++] & 0xFF;
        int hi = d[i++] & 0xFF;
        mut.push_back(hi << 8 | lo);
    }

    return u16;
}

String String::fromUtf8(const char* str)
{
    if (!str) {
        return String();
    }
    String s;
    UtfCodec::utf8to16(std::string_view(str), s.mutStr());
    return s;
}

String String::fromUtf8(const ByteArray& data)
{
    if (data.empty()) {
        return String();
    }
    String s;
    UtfCodec::utf8to16(std::string_view(data.constChar(), data.size()), s.mutStr());
    return s;
}

ByteArray String::toUtf8() const
{
    ByteArray ba;
    std::u16string_view v(constStr());
    try {
        utf8::utf16to8(v.begin(), v.end(), std::back_inserter(ba));
    } catch (const std::exception& e) {
        LOGE() << e.what();
    }
    return ba;
}

String String::fromAscii(const char* str, size_t size)
{
    if (!str) {
        return String();
    }

    size = (size == mu::nidx) ? std::strlen(str) : size;
    String s;
    std::u16string& data = s.mutStr();
    data.resize(size);
    for (size_t i = 0; i < size; ++i) {
        data[i] = Char::fromAscii(str[i]).unicode();
    }
    return s;
}

ByteArray String::toAscii(bool* ok) const
{
    ByteArray ba;
    ba.resize(size());

    if (ok) {
        *ok = true;
    }

    for (size_t i = 0; i < size(); ++i) {
        bool cok = false;
        char ch = Char::toAscii(constStr().at(i), &cok);
        if (!cok && ok) {
            *ok = false;
        }
        ba[i] = static_cast<uint8_t>(ch);
    }
    return ba;
}

String String::fromStdString(const std::string& str)
{
    String s;
    UtfCodec::utf8to16(std::string_view(str), s.mutStr());
    return s;
}

std::string String::toStdString() const
{
    std::string s;
    UtfCodec::utf16to8(std::u16string_view(constStr()), s);
    return s;
}

std::u16string String::toStdU16String() const
{
    return constStr();
}

String String::fromUcs4(const char32_t* str, size_t size)
{
    std::u32string_view v32;
    if (size == mu::nidx) {
        v32 = std::u32string_view(str);
    } else {
        v32 = std::u32string_view(str, size);
    }

    std::string s8;
    UtfCodec::utf32to8(v32, s8);

    String s;
    UtfCodec::utf8to16(s8, s.mutStr());
    return s;
}

String String::fromUcs4(char32_t chr)
{
    return fromUcs4(&chr, 1);
}

std::u32string String::toStdU32String() const
{
    std::string s;
    UtfCodec::utf16to8(constStr(), s);
    std::u32string s32;
    UtfCodec::utf8to32(s, s32);
    return s32;
}

#ifndef NO_QT_SUPPORT
String String::fromQString(const QString& str)
{
    const QChar* qu = str.unicode();
    static_assert(sizeof(QChar) == sizeof(char16_t));
    const char16_t* u = reinterpret_cast<const char16_t*>(qu);

    String s;
    s.mutStr() = std::u16string(u, u + str.size());
    return s;
}

QString String::toQString() const
{
    static_assert(sizeof(QChar) == sizeof(char16_t));
    return QString(reinterpret_cast<const QChar*>(constStr().data()), static_cast<int>(size()));
}

#endif

size_t String::size() const
{
    return constStr().size();
}

bool String::empty() const
{
    return constStr().empty();
}

void String::clear()
{
    mutStr().clear();
}

Char String::at(size_t i) const
{
    IF_ASSERT_FAILED(i < size()) {
        return Char();
    }
    return Char(constStr().at(i));
}

bool String::contains(const Char& ch) const
{
    return constStr().find(ch.unicode()) != std::u16string::npos;
}

bool String::contains(const String& str, CaseSensitivity cs) const
{
    if (cs == CaseSensitivity::CaseSensitive) {
        return constStr().find(str.constStr()) != std::u16string::npos;
    } else {
        std::u16string self = constStr();
        std::transform(self.begin(), self.end(), self.begin(), [](char16_t c){ return Char::toLower(c); });
        std::u16string other = str.constStr();
        std::transform(other.begin(), other.end(), other.begin(), [](char16_t c){ return Char::toLower(c); });
        return self.find(other) != std::u16string::npos;
    }
}

bool String::contains(const std::wregex& re) const
{
    const std::u16string& u16 = constStr();
    std::wstring ws;
    ws.resize(u16.size());

    static_assert(sizeof(wchar_t) >= sizeof(char16_t));

    for (size_t i = 0; i < ws.size(); ++i) {
        ws[i] = static_cast<wchar_t>(u16.at(i));
    }

    auto words_begin = std::wsregex_iterator(ws.begin(), ws.end(), re);
    if (words_begin != std::wsregex_iterator()) {
        return true;
    }
    return false;
}

int String::count(const Char& ch) const
{
    int count = 0;
    for (size_t i = 0; i < constStr().size(); ++i) {
        if (constStr().at(i) == ch.unicode()) {
            ++count;
        }
    }
    return count;
}

size_t String::indexOf(const Char& ch, size_t from) const
{
    for (size_t i = from; i < constStr().size(); ++i) {
        if (constStr().at(i) == ch.unicode()) {
            return i;
        }
    }
    return mu::nidx;
}

size_t String::indexOf(const String& str, size_t from) const
{
    return constStr().find(str.constStr(), from);
}

size_t String::indexOf(const char16_t* str, size_t from) const
{
    return constStr().find(str, from);
}

size_t String::lastIndexOf(const Char& ch, size_t from) const
{
    from = std::min(from, constStr().size() - 1);

    for (int i = static_cast<int>(from); i >= 0; --i) {
        if (constStr().at(i) == ch.unicode()) {
            return i;
        }
    }
    return mu::nidx;
}

bool String::startsWith(const String& str, CaseSensitivity cs) const
{
    if (str.size() > size()) {
        return false;
    }

    for (size_t i = 0; i < str.size(); ++i) {
        if (Char(constStr().at(i)) == str.at(i)) {
            continue;
        }

        if (cs == CaseInsensitive) {
            if (Char::toLower(constStr().at(i)) == Char::toLower(str.constStr().at(i))) {
                continue;
            }
        }

        return false;
    }

    return true;
}

bool String::startsWith(char16_t ch, CaseSensitivity cs) const
{
    if (empty()) {
        return false;
    }

    if (constStr().front() == ch) {
        return true;
    }

    if (cs == CaseInsensitive) {
        if (Char::toLower(constStr().front()) == Char::toLower(ch)) {
            return true;
        }
    }

    return false;
}

bool String::endsWith(const String& str, CaseSensitivity cs) const
{
    if (str.size() > size()) {
        return false;
    }

    size_t start = size() - str.size();
    for (size_t i = 0; i < str.size(); ++i) {
        if (constStr().at(start + i) == str.at(i)) {
            continue;
        }

        if (cs == CaseInsensitive) {
            if (Char::toLower(constStr().at(start + i)) == Char::toLower(str.constStr().at(i))) {
                continue;
            }
        }

        return false;
    }

    return true;
}

bool String::endsWith(char16_t ch, CaseSensitivity cs) const
{
    if (empty()) {
        return false;
    }

    if (constStr().back() == ch) {
        return true;
    }

    if (cs == CaseInsensitive) {
        if (Char::toLower(constStr().back()) == Char::toLower(ch)) {
            return true;
        }
    }

    return false;
}

StringList String::split(const Char& ch, SplitBehavior behavior) const
{
    StringList out;
    std::size_t current, previous = 0;
    current = constStr().find(ch.unicode());
    while (current != std::string::npos) {
        String sub = mid(previous, current - previous);
        if (behavior == SplitBehavior::SkipEmptyParts && sub.empty()) {
            // skip
        } else {
            out.push_back(std::move(sub));
        }
        previous = current + 1;
        current = constStr().find(ch.unicode(), previous);
    }
    String sub = mid(previous, current - previous);
    if (behavior == SplitBehavior::SkipEmptyParts && sub.empty()) {
        // skip
    } else {
        out.push_back(std::move(sub));
    }

    return out;
}

StringList String::split(const String& str, SplitBehavior behavior) const
{
    StringList out;
    std::size_t current, previous = 0;
    current = constStr().find(str.constStr());
    while (current != std::string::npos) {
        String sub = mid(previous, current - previous);
        if (behavior == SplitBehavior::SkipEmptyParts && sub.empty()) {
            // skip
        } else {
            out.push_back(std::move(sub));
        }
        previous = current + str.size();
        current = constStr().find(str.constStr(), previous);
    }
    String sub = mid(previous, current - previous);
    if (behavior == SplitBehavior::SkipEmptyParts && sub.empty()) {
        // skip
    } else {
        out.push_back(std::move(sub));
    }

    return out;
}

StringList String::split(const std::regex& re, SplitBehavior behavior) const
{
    std::string originU8;
    UtfCodec::utf16to8(std::u16string_view(constStr()), originU8);
    std::sregex_token_iterator iter(originU8.begin(), originU8.end(), re, -1);
    std::sregex_token_iterator end;
    std::vector<std::string> vec = { iter, end };

    StringList out;
    for (const std::string& s : vec) {
        if (behavior == SplitBehavior::SkipEmptyParts && s.empty()) {
            // skip
            continue;
        }

        String sub;
        UtfCodec::utf8to16(s, sub.mutStr());
        out.push_back(std::move(sub));
    }

    return out;
}

StringList String::search(const std::regex& re, std::initializer_list<int> matches, SplitBehavior behavior) const
{
    std::string originU8;
    UtfCodec::utf16to8(std::u16string_view(constStr()), originU8);
    std::sregex_token_iterator iter(originU8.begin(), originU8.end(), re, matches);
    std::sregex_token_iterator end;
    std::vector<std::string> vec = { iter, end };

    StringList out;
    for (const std::string& s : vec) {
        if (behavior == SplitBehavior::SkipEmptyParts && s.empty()) {
            // skip
            continue;
        }
        String sub;
        UtfCodec::utf8to16(s, sub.mutStr());
        out.push_back(std::move(sub));
    }

    return out;
}

String& String::replace(const String& before, const String& after)
{
    if (before == after) {
        return *this;
    }
    Mutator h = mutStr();
    std::u16string& str = h.s;
    size_t start_pos = 0;
    while ((start_pos = str.find(before.constStr(), start_pos)) != std::string::npos) {
        str.replace(start_pos, before.size(), after.constStr());
        start_pos += after.size(); // Handles case where 'after' is a substring of 'before'
    }
    return *this;
}

String& String::replace(char16_t before, char16_t after)
{
    Mutator h = mutStr();
    std::u16string& str = h.s;
    for (size_t i = 0; i < str.size(); ++i) {
        if (str.at(i) == before) {
            str[i] = after;
        }
    }
    return *this;
}

String& String::replace(const std::regex& re, const String& after)
{
    std::string afterU8;
    UtfCodec::utf16to8(std::u16string_view(after.constStr()), afterU8);

    std::string originU8;
    UtfCodec::utf16to8(std::u16string_view(constStr()), originU8);
    std::string replasedU8 = std::regex_replace(originU8, re, afterU8);

    Mutator h = mutStr();
    std::u16string& sefl = h.s;
    sefl.clear();
    UtfCodec::utf8to16(std::string_view(replasedU8), sefl);
    return *this;
}

String& String::insert(size_t position, const String& str)
{
    mutStr().insert(position, str.constStr());
    return *this;
}

String& String::remove(const Char& ch)
{
    return remove(ch.unicode());
}

String& String::remove(char16_t ch)
{
    auto it = constStr().find(ch);
    if (it != std::u16string::npos) {
        mutStr().erase(it, 1);
    }
    return *this;
}

String& String::remove(size_t position, size_t n)
{
    mutStr().erase(position, n);
    return *this;
}

void String::chop(size_t n)
{
    if (n >= size()) {
        n = size();
    }

    remove(size() - n);
}

void String::truncate(size_t position)
{
    mutStr().resize(position);
}

static constexpr bool is1To9(char16_t chr)
{
    return u'1' <= chr && chr <= '9';
}

void String::doArgs(std::u16string& out, const std::vector<std::u16string_view>& args) const
{
    struct Part {
        std::u16string_view substr;
        size_t argIdxToInsertAfter = mu::nidx;
    };

    const std::u16string& str = constStr();
    const std::u16string_view view(str);
    std::vector<Part> parts;

    {
        std::size_t currentPercentIdx = view.find(u'%'), partStartIdx = 0;

        while (currentPercentIdx != std::string::npos) {
            std::u16string_view sub = view.substr(partStartIdx, currentPercentIdx - partStartIdx);
            std::size_t nextCharIdx = currentPercentIdx + 1;
            if (nextCharIdx < view.size() && is1To9(view.at(nextCharIdx))) {
                size_t argIdx = view.at(nextCharIdx) - u'1';
                parts.push_back({ std::move(sub), argIdx });
                partStartIdx = nextCharIdx + 1;
            }
            currentPercentIdx = view.find(u'%', nextCharIdx);
        }

        std::u16string_view sub = view.substr(partStartIdx);
        parts.push_back({ std::move(sub) });
    }

    {
        for (const auto& [substr, argIdxToInsertAfter] : parts) {
            if (!substr.empty()) {
                out += substr;
            }

            if (argIdxToInsertAfter != mu::nidx) {
                if (argIdxToInsertAfter < args.size()) {
                    out += args.at(argIdxToInsertAfter);
                } else {
                    // When there are 5 args, %6 becomes %1
                    out.push_back(u'%');
                    out.push_back(u'1' + static_cast<char16_t>(argIdxToInsertAfter - args.size()));
                }
            }
        }
    }
}

String String::arg(const String& val) const
{
    String s;
    doArgs(s.mutStr(), { std::u16string_view(val.constStr()) });
    return s;
}

String String::arg(const String& val1, const String& val2) const
{
    String s;
    doArgs(s.mutStr(), { std::u16string_view(val1.constStr()), std::u16string_view(val2.constStr()) });
    return s;
}

String String::arg(const String& val1, const String& val2, const String& val3) const
{
    String s;
    doArgs(s.mutStr(), { std::u16string_view(val1.constStr()),
                         std::u16string_view(val2.constStr()),
                         std::u16string_view(val3.constStr()) });
    return s;
}

String String::arg(const String& val1, const String& val2, const String& val3, const String& val4) const
{
    String s;
    doArgs(s.mutStr(), { std::u16string_view(val1.constStr()),
                         std::u16string_view(val2.constStr()),
                         std::u16string_view(val3.constStr()),
                         std::u16string_view(val4.constStr()) });
    return s;
}

String String::arg(const String& val1, const String& val2, const String& val3, const String& val4, const String& val5) const
{
    String s;
    doArgs(s.mutStr(), { std::u16string_view(val1.constStr()),
                         std::u16string_view(val2.constStr()),
                         std::u16string_view(val3.constStr()),
                         std::u16string_view(val4.constStr()),
                         std::u16string_view(val5.constStr()) });
    return s;
}

String String::mid(size_t pos, size_t count) const
{
    String s;
    if (pos > size()) {
        return s;
    }
    s.mutStr() = constStr().substr(pos, count);
    return s;
}

String String::left(size_t n) const
{
    return mid(0, n);
}

String String::right(size_t n) const
{
    return mid(size() - n);
}

String String::trimmed() const
{
    String s = *this;
    trim_helper(s.mutStr());
    return s;
}

String String::simplified() const
{
    //! TODO
#ifndef NO_QT_SUPPORT
    return String::fromQString(toQString().simplified());
#else
    return *this;
#endif
}

String String::toXmlEscaped(char16_t c)
{
    switch (c) {
    case u'<':
        return String(u"&lt;");
    case u'>':
        return String(u"&gt;");
    case u'&':
        return String(u"&amp;");
    case u'\"':
        return String(u"&quot;");
    default:
        // ignore invalid characters in xml 1.0
        if ((c < 0x0020 && c != 0x0009 && c != 0x000A && c != 0x000D)) {
            return String();
        }
        return String(Char(c));
    }
}

String String::toXmlEscaped(const String& s)
{
    String escaped;
    escaped.reserve(s.size());
    for (size_t i = 0; i < s.size(); ++i) {
        char16_t c = s.at(i).unicode();
        escaped += toXmlEscaped(c);
    }
    return escaped;
}

String String::toXmlEscaped() const
{
    return toXmlEscaped(*this);
}

String String::decodeXmlEntities(const String& src_)
{
    std::string src = src_.toStdString();
    String ret = src_;
    static const std::regex re("&#([0-9]+);");

    auto begin = std::sregex_iterator(src.begin(), src.end(), re);
    auto end = std::sregex_iterator();
    for (auto it = begin; it != end; ++it) {
        std::smatch match = *it;
        std::string str0 = match[0];
        std::string str1 = match[1];
        ret.replace(String::fromStdString(str0), String(Char(std::stoi(str1))));
    }
    return ret;
}

String String::toLower() const
{
    //! TODO
#ifndef NO_QT_SUPPORT
    QString qs = toQString();
    return String::fromQString(qs.toLower());
#else
    String s = *this;
    std::u16string& us = s.mutStr();
    std::transform(us.begin(), us.end(), us.begin(), [](char16_t c){ return Char::toLower(c); });
    return s;
#endif
}

String String::toUpper() const
{
    //! TODO
#ifndef NO_QT_SUPPORT
    QString qs = toQString();
    return String::fromQString(qs.toUpper());
#else
    String s = *this;
    std::u16string& us = s.mutStr();
    std::transform(us.begin(), us.end(), us.begin(), [](char16_t c){ return Char::toUpper(c); });
    return s;
#endif
}

int String::toInt(bool* ok, int base) const
{
    ByteArray ba = toUtf8();
    return static_cast<int>(toInt_helper(ba.constChar(), ok, base));
}

unsigned int String::toUInt(bool* ok, int base) const
{
    ByteArray ba = toUtf8();
    return static_cast<unsigned int>(toInt_helper(ba.constChar(), ok, base));
}

String String::number(int n, int base)
{
    std::stringstream stream;
    if (base == 16) {
        stream << std::hex;
    }
    stream << n;
    std::string s = stream.str();
    return fromAscii(s.c_str(), s.size());
}

String String::number(int64_t n)
{
    std::string s = std::to_string(n);
    return fromAscii(s.c_str());
}

String String::number(size_t n)
{
    std::string s = std::to_string(n);
    return fromAscii(s.c_str());
}

String String::number(double n, int prec)
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

    return fromAscii(s.c_str(), correctedIdx + 1);
}

float String::toFloat(bool* ok) const
{
    return static_cast<float>(toDouble(ok));
}

double String::toDouble(bool* ok) const
{
    ByteArray ba = toUtf8();
    return toDouble_helper(ba.constChar(), ok);
}

// ============================
// StringList
// ============================
StringList& StringList::append(const StringList& l)
{
    for (const String& s : l) {
        push_back(s);
    }
    return *this;
}

StringList StringList::filter(const String& str) const
{
    StringList result;
    for (const String& s : *this) {
        if (s.contains(str)) {
            result << s;
        }
    }
    return result;
}

String StringList::join(const String& sep) const
{
    String res;
    for (size_t i = 0; i < size(); ++i) {
        if (i) {
            res += sep;
        }
        res += at(i);
    }
    return res;
}

void StringList::insert(size_t idx, const String& str)
{
    std::vector<String>::insert(begin() + idx, str);
}

void StringList::replace(size_t idx, const String& str)
{
    this->operator [](idx) = str;
}

bool StringList::removeAll(const String& str)
{
    size_t origSize = size();
    erase(std::remove(begin(), end(), str), end());
    return origSize != size();
}

void StringList::removeAt(size_t i)
{
    erase(begin() + i);
}

#ifndef NO_QT_SUPPORT
StringList::StringList(const QStringList& l)
{
    reserve(l.size());
    for (const QString& s : l) {
        push_back(String::fromQString(s));
    }
}

QStringList StringList::toQStringList() const
{
    QStringList l;
    l.reserve(static_cast<int>(size()));
    for (size_t i = 0; i < size(); ++i) {
        l << at(i).toQString();
    }
    return l;
}

#endif
// ============================
// AsciiChar
// ============================

char AsciiChar::toLower(char ch)
{
    return static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
}

char AsciiChar::toUpper(char ch)
{
    return static_cast<char>(std::toupper(static_cast<unsigned char>(ch)));
}

// ============================
// AsciiStringView
// ============================
const char* AsciiStringView::ascii() const
{
    return m_data;
}

size_t AsciiStringView::size() const
{
    return m_size;
}

bool AsciiStringView::empty() const
{
    return m_size == 0;
}

AsciiChar AsciiStringView::at(size_t i) const
{
    IF_ASSERT_FAILED(i < size()) {
        return AsciiChar();
    }
    return AsciiChar(m_data[i]);
}

bool AsciiStringView::contains(char ch) const
{
    return indexOf(ch) != mu::nidx;
}

size_t AsciiStringView::indexOf(char ch) const
{
    for (size_t i = 0; i < m_size; ++i) {
        if (m_data[i] == ch) {
            return i;
        }
    }
    return mu::nidx;
}

int AsciiStringView::toInt(bool* ok, int base) const
{
    return toInt_helper(m_data, ok, base);
}

double AsciiStringView::toDouble(bool* ok) const
{
    return toDouble_helper(m_data, ok);
}
