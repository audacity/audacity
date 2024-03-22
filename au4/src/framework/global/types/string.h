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
#ifndef MU_GLOBAL_STRING_H
#define MU_GLOBAL_STRING_H

#include <memory>
#include <cstring>
#include <vector>
#include <string>
#include <string_view>
#include <regex>

#include "containers.h"
#include "bytearray.h"
#include "global/logstream.h"

#ifndef NO_QT_SUPPORT
#include <QString>
#include <QStringList>
#endif

namespace mu {
enum CaseSensitivity {
    CaseInsensitive = 0,
    CaseSensitive = 1
};

enum SplitBehavior {
    KeepEmptyParts = 0,
    SkipEmptyParts = 1,
};

// ============================
// AsciiChar (ASCII)
// ============================
struct AsciiChar
{
public:
    constexpr AsciiChar() = default;
    constexpr explicit AsciiChar(char c)
        : m_ch(c) {}

    constexpr char ascii() const noexcept { return m_ch; }
    constexpr char16_t unicode() const noexcept { return char16_t(m_ch); }
    inline char toLower() const { return toLower(m_ch); }
    inline char toUpper() const { return toUpper(m_ch); }

    static char toLower(char ch);
    static char toUpper(char ch);

private:
    char m_ch = 0;
};

// ============================
// Char (UTF-16)
// ============================
class Char
{
public:

    enum SpecialCharacter {
        Null = 0x0000,
        Tabulation = 0x0009,
        LineFeed = 0x000a,
        FormFeed = 0x000c,
        CarriageReturn = 0x000d,
        Space = 0x0020,
        Nbsp = 0x00a0,
        SoftHyphen = 0x00ad,
        ReplacementCharacter = 0xfffd,
        ObjectReplacementCharacter = 0xfffc,
        ByteOrderMark = 0xfeff,
        ByteOrderSwapped = 0xfffe,
        ParagraphSeparator = 0x2029,
        LineSeparator = 0x2028,
        LastValidCodePoint = 0x10ffff
    };

    constexpr Char() = default;
    constexpr Char(char16_t c)
        : m_ch(c) {}
    Char(AsciiChar c)
        : m_ch(c.unicode()) {}

#ifndef NO_QT_SUPPORT
    constexpr Char(QChar c)
        : m_ch(c.unicode()) {}
    constexpr operator QChar() const {
        return QChar(m_ch);
    }
#endif

    constexpr bool operator ==(Char c) const { return m_ch == c.m_ch; }
    constexpr bool operator !=(Char c) const { return !operator ==(c); }
    constexpr bool operator ==(char16_t c) const { return m_ch == c; }
    constexpr bool operator !=(char16_t c) const { return !operator ==(c); }
    constexpr bool operator ==(AsciiChar c) const { return m_ch == c.unicode(); }
    constexpr bool operator !=(AsciiChar c) const { return !operator ==(c); }

    constexpr bool operator >(Char c) const { return m_ch > c.m_ch; }
    constexpr bool operator >(char16_t c) const { return m_ch > c; }
    constexpr bool operator <(Char c) const { return m_ch < c.m_ch; }
    constexpr bool operator <(char16_t c) const { return m_ch < c; }

    constexpr bool operator >=(Char c) const { return m_ch >= c.m_ch; }
    constexpr bool operator >=(char16_t c) const { return m_ch >= c; }
    constexpr bool operator <=(Char c) const { return m_ch <= c.m_ch; }
    constexpr bool operator <=(char16_t c) const { return m_ch <= c; }

    constexpr char16_t unicode() const { return m_ch; }

    inline bool isNull() const { return m_ch == 0; }

    constexpr bool isAscii() const { return isAscii(m_ch); }
    static constexpr bool isAscii(char16_t c) { return c <= 0x7f; }
    inline char toAscii(bool* ok = nullptr) const { return toAscii(m_ch, ok); }
    static char toAscii(char16_t c, bool* ok = nullptr);
    static inline Char fromAscii(char c) { return static_cast<char16_t>(c); }

    inline bool isLetter() const { return isLetter(m_ch); }
    static bool isLetter(char16_t c);
    inline bool isSpace() const { return isSpace(m_ch); }
    static bool isSpace(char16_t c);
    inline bool isDigit() const { return isDigit(m_ch); }
    static bool isDigit(char16_t c);
    int digitValue() const;
    inline bool isPunct() const { return isPunct(m_ch); }
    static bool isPunct(char16_t c);

    inline bool isLower() const { return toLower() == m_ch; }
    inline Char toLower() const { return toLower(m_ch); }
    static char16_t toLower(char16_t ch);
    inline bool isUpper() const { return toUpper() == m_ch; }
    inline Char toUpper() const { return toUpper(m_ch); }
    static char16_t toUpper(char16_t ch);

    inline bool isHighSurrogate() const { return isHighSurrogate(m_ch); }
    inline bool isLowSurrogate() const { return isLowSurrogate(m_ch); }
    inline bool isSurrogate() const { return isSurrogate(m_ch); }

    static constexpr char32_t surrogateToUcs4(char16_t high, char16_t low) { return (char32_t(high) << 10) + low - 0x35fdc00; }
    static constexpr char32_t surrogateToUcs4(Char high, Char low) { return surrogateToUcs4(high.m_ch, low.m_ch); }
    static constexpr bool isHighSurrogate(char32_t ucs4) { return (ucs4 & 0xfffffc00) == 0xd800; }
    static constexpr bool isLowSurrogate(char32_t ucs4) { return (ucs4 & 0xfffffc00) == 0xdc00; }
    static constexpr bool isSurrogate(char32_t ucs4) { return ucs4 - 0xd800u < 2048u; }
    static constexpr char16_t highSurrogate(char32_t ucs4) { return char16_t((ucs4 >> 10) + 0xd7c0); }
    static constexpr char16_t lowSurrogate(char32_t ucs4) { return char16_t(ucs4 % 0x400 + 0xdc00); }
    static constexpr bool requiresSurrogates(char32_t ucs4) { return ucs4 >= 0x10000; }

private:
    char16_t m_ch = 0;
};

// ============================
// UtfCodec
// ============================
class UtfCodec
{
public:
    enum class Encoding {
        Unknown,
        UTF_8,
        UTF_16LE,
        UTF_16BE,
    };

    static Encoding xmlEncoding(const ByteArray& data);

    static void utf8to16(std::string_view src, std::u16string& dst);
    static void utf16to8(std::u16string_view src, std::string& dst);
    static void utf8to32(std::string_view src, std::u32string& dst);
    static void utf32to8(std::u32string_view src, std::string& dst);
    static bool isValidUtf8(const std::string_view& src);
};

// ============================
// String (UTF-16)
// ============================
class StringList;
class AsciiStringView;
class String
{
public:

    String();
    String(const char16_t* str);
    String(const Char& ch);
    String(const Char* unicode, size_t size = mu::nidx);

#ifndef NO_QT_SUPPORT
    String(const QString& str) { *this = fromQString(str); }
    operator QString() const {
        return this->toQString();
    }

    String& operator=(const QString& str) { *this = fromQString(str); return *this; }
    static String fromQString(const QString& str);
    QString toQString() const;

    inline bool operator ==(const QString& s) const { return toQString() == s; }
    inline bool operator !=(const QString& s) const { return !operator ==(s); }
#endif

    String& operator=(const char16_t* str);
    void reserve(size_t i);

    inline bool operator ==(const String& s) const { return constStr() == s.constStr(); }
    inline bool operator !=(const String& s) const { return !operator ==(s); }

    bool operator ==(const AsciiStringView& s) const;
    inline bool operator !=(const AsciiStringView& s) const { return !operator ==(s); }
    inline bool operator ==(const char16_t* s) const { return constStr() == s; }
    inline bool operator !=(const char16_t* s) const { return !operator ==(s); }
    bool operator ==(const char* s) const;
    inline bool operator !=(const char* s) const { return !operator ==(s); }
    inline bool operator <(const String& s) const { return constStr() < s.constStr(); }
    inline bool operator >(const String& s) const { return constStr() > s.constStr(); }
    inline bool operator <=(const String& s) const { return constStr() <= s.constStr(); }
    inline bool operator >=(const String& s) const { return constStr() >= s.constStr(); }

    inline String& operator +=(const String& s) { return append(s); }
    String& operator +=(const char16_t* s);
    inline String& operator +=(char16_t s) { return append(s); }

    inline String operator+(const mu::String& s) const { String t(*this); t += s; return t; }
    inline String operator+(const char16_t* s) const { String t(*this); t += s; return t; }
    inline String operator+(char16_t s) const { String t(*this); t += s; return t; }

    char16_t operator [](size_t i) const;
    char16_t& operator [](size_t i);

    String& append(Char ch);
    String& append(const String& s);
    String& prepend(Char ch);
    String& prepend(const String& s);

    static String fromUtf16LE(const ByteArray& data);

    static String fromUtf8(const char* str);
    static String fromUtf8(const ByteArray& data);
    ByteArray toUtf8() const;

    static String fromAscii(const char* str, size_t size = mu::nidx);
    ByteArray toAscii(bool* ok = nullptr) const;

    static String fromStdString(const std::string& str);
    std::string toStdString() const;
    std::u16string toStdU16String() const;

    static String fromUcs4(const char32_t* str, size_t size = mu::nidx);
    static String fromUcs4(char32_t chr);
    std::u32string toStdU32String() const;

    size_t size() const;
    bool empty() const;
    inline bool isEmpty() const { return empty(); }
    void clear();
    Char at(size_t i) const;
    Char front() const { return at(0); }
    Char back() const { return at(size() - 1); }
    bool contains(const Char& ch) const;
    bool contains(const String& str, CaseSensitivity cs = CaseSensitive) const;
    bool contains(const std::wregex& re) const;
    int count(const Char& ch) const;
    size_t indexOf(const Char& ch, size_t from = 0) const;
    size_t indexOf(const String& str, size_t from = 0) const;
    size_t indexOf(const char16_t* str, size_t from = 0) const;
    size_t lastIndexOf(const Char& ch, size_t from = mu::nidx) const;

    //! NOTE Now implemented only compare with ASCII
    bool startsWith(const String& str, CaseSensitivity cs = CaseSensitive) const;
    bool startsWith(char16_t ch, CaseSensitivity cs = CaseSensitive) const;
    bool endsWith(const String& str, CaseSensitivity cs = CaseSensitive) const;
    bool endsWith(char16_t ch, CaseSensitivity cs = CaseSensitive) const;

    StringList split(const Char& ch, SplitBehavior behavior = KeepEmptyParts) const;
    StringList split(const String& str, SplitBehavior behavior = KeepEmptyParts) const;
    StringList split(const std::regex& re, SplitBehavior behavior = KeepEmptyParts) const;
    StringList search(const std::regex& re, std::initializer_list<int> matches, SplitBehavior behavior = KeepEmptyParts) const;
    String& replace(const String& before, const String& after);
    String& replace(char16_t before, char16_t after);
    String& replace(const std::regex& re, const String& after);
    String& insert(size_t position, const String& str);
    String& remove(const String& str) { return replace(str, String()); }
    String& remove(const std::regex& rx) { return replace(rx, String()); }
    String& remove(const Char& ch);
    String& remove(char16_t ch);
    String& remove(size_t position, size_t n = mu::nidx);
    void chop(size_t n);
    void truncate(size_t position);

    String arg(const String& val) const;
    String arg(const String& val1, const String& val2) const;
    String arg(const String& val1, const String& val2, const String& val3) const;
    String arg(const String& val1, const String& val2, const String& val3, const String& val4) const;
    String arg(const String& val1, const String& val2, const String& val3, const String& val4, const String& val5) const;

    String arg(int val) const { return arg(number(val)); }
    String arg(int val1, int val2) const { return arg(number(val1), number(val2)); }
    String arg(int val1, int val2, int val3) const { return arg(number(val1), number(val2), number(val3)); }

    String arg(int64_t val) const { return arg(number(val)); }
    String arg(int64_t val1, int64_t val2) const { return arg(number(val1), number(val2)); }
    String arg(int64_t val1, int64_t val2, int64_t val3) const { return arg(number(val1), number(val2), number(val3)); }

    String arg(size_t val) const { return arg(number(val)); }
    String arg(size_t val1, size_t val2) const { return arg(number(val1), number(val2)); }
    String arg(size_t val1, size_t val2, size_t val3) const { return arg(number(val1), number(val2), number(val3)); }

    String arg(double val) const { return arg(number(val)); }
    String arg(double val1, double val2) const { return arg(number(val1), number(val2)); }
    String arg(double val1, double val2, double val3) const { return arg(number(val1), number(val2), number(val3)); }

    String mid(size_t pos, size_t count = mu::nidx) const;
    String left(size_t n) const;
    String right(size_t n) const;

    String trimmed() const;
    String simplified() const;
    String toXmlEscaped() const;
    static String toXmlEscaped(const String& str);
    static String toXmlEscaped(char16_t c);
    static String decodeXmlEntities(const String& src);

    String toLower() const;
    String toUpper() const;

    int toInt(bool* ok = nullptr, int base = 10) const;
    unsigned int toUInt(bool* ok = nullptr, int base = 10) const;
    float toFloat(bool* ok = nullptr) const;
    double toDouble(bool* ok = nullptr) const;

    static String number(int n, int base = 10);
    static String number(int64_t n);
    static String number(size_t n);
    static String number(double n, int prec = 6);

    inline size_t hash() const { return std::hash<std::u16string> {}(constStr()); }

private:
    struct Mutator;
    const std::u16string& constStr() const;
    Mutator mutStr(bool do_detach = true);
    void detach();
    void doArgs(std::u16string& out, const std::vector<std::u16string_view>& args) const;

    std::shared_ptr<std::u16string> m_data;

#ifdef STRING_DEBUG_HACK
    //! HACK On MacOS with clang there are problems with debugging - the value of the std::u16string is not visible.
    //! This is hack for debugging on MacOS
    void updateDebugView();
    std::string dview;
#endif
};

class StringList : public std::vector<String>
{
public:
    StringList() = default;
    StringList(std::initializer_list<String> l)
        : std::vector<String>(l) {}

    StringList& operator <<(const String& s) { return append(s); }
    StringList& append(const String& s) { push_back(s); return *this; }

    StringList& operator <<(const StringList& l) { return append(l); }
    StringList& append(const StringList& l);

    size_t indexOf(const String& s) const { return mu::indexOf(*this, s); }
    bool contains(const String& s) const { return mu::contains(*this, s); }
    StringList filter(const String& str) const;
    String join(const String& sep) const;

    void insert(size_t idx, const String& str);
    void replace(size_t idx, const String& str);
    bool removeAll(const String& str);
    void removeAt(size_t i);

#ifndef NO_QT_SUPPORT
    StringList(const QStringList& l);
    QStringList toQStringList() const;
#endif
};

// ============================
// AsciiStringView (ASCII)
// Be carefully!!, this class just hold pointer to string (no copy), so source string should be present, while use view
// ============================
class AsciiStringView
{
public:

    constexpr AsciiStringView() = default;
    constexpr AsciiStringView(const char* str)
        : m_size(str ? std::char_traits<char>::length(str) : 0), m_data(str) {}
    constexpr AsciiStringView(const char* str, size_t size)
        : m_size(size), m_data(str) {}

    AsciiStringView(const std::string& str)
        : m_size(str.size()), m_data(str.c_str()) {}

#ifndef NO_QT_SUPPORT
    static AsciiStringView fromQLatin1String(const QLatin1String& str) { return AsciiStringView(str.latin1(), str.size()); }
    QLatin1String toQLatin1String() const { return QLatin1String(m_data, static_cast<int>(m_size)); }
#endif

    operator std::string_view() const {
        return std::string_view(m_data, m_size);
    }

    inline bool operator ==(const AsciiStringView& s) const { return m_size == s.m_size && std::memcmp(m_data, s.m_data, m_size) == 0; }
    inline bool operator !=(const AsciiStringView& s) const { return !this->operator ==(s); }
    inline bool operator ==(const char* s) const
    {
        size_t sz = (s ? std::char_traits<char>::length(s) : 0);
        return m_size == sz && (s ? std::memcmp(m_data, s, m_size) == 0 : true);
    }

    inline bool operator !=(const char* s) const { return !this->operator ==(s); }

    inline bool operator <(const AsciiStringView& s) const
    {
        if (m_size != s.m_size) {
            return m_size < s.m_size;
        }
        return std::memcmp(m_data, s.m_data, m_size) < 0;
    }

    const char* ascii() const;
    size_t size() const;
    bool empty() const;
    AsciiChar at(size_t i) const;
    bool contains(char ch) const;
    size_t indexOf(char ch) const;

    int toInt(bool* ok = nullptr, int base = 10) const;
    double toDouble(bool* ok = nullptr) const;

private:
    size_t m_size = 0;
    const char* m_data = nullptr;
};

inline String operator+(char16_t s1, const String& s2) { String t(s1); t += s2; return t; }
inline String operator+(const char16_t* s1, const String& s2) { String t(s1); t += s2; return t; }
}

// ============================
// Char (UTF-16)
// ============================
inline bool operator ==(const char16_t c1, const mu::Char c2) { return c2 == c1; }
inline bool operator !=(const char16_t c1, const mu::Char c2) { return c2 != c1; }

// ============================
// String (UTF-16)
// ============================
inline bool operator ==(const char16_t* s1, const mu::String& s2) { return s2 == s1; }
inline bool operator !=(const char16_t* s1, const mu::String& s2) { return s2 != s1; }

template<>
struct std::hash<mu::String>
{
    std::size_t operator()(const mu::String& s) const noexcept { return s.hash(); }
};

inline mu::logger::Stream& operator<<(mu::logger::Stream& s, const mu::String& str)
{
    s << str.toUtf8().constChar();
    return s;
}

// ============================
// AsciiStringView (ASCII)
// ============================
inline bool operator ==(const char* s1, const mu::AsciiStringView& s2) { return s2 == s1; }
inline bool operator !=(const char* s1, const mu::AsciiStringView& s2) { return s2 != s1; }

inline mu::logger::Stream& operator<<(mu::logger::Stream& s, const mu::AsciiStringView& str)
{
    s << str.ascii();
    return s;
}

#ifndef muPrintable
#  define muPrintable(string) string.toUtf8().constChar()
#endif

#endif // MU_GLOBAL_STRING_H
