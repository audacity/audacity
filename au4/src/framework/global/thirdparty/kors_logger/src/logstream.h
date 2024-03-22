/*
MIT License

Copyright (c) 2020 Igor Korsukov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
#ifndef KORS_LOGSTREAM_H
#define KORS_LOGSTREAM_H

#include <sstream>
#include <thread>
#include <vector>

#ifdef KORS_LOGGER_QT_SUPPORT
#include <QDebug>
#include <QVariant>
#endif

//! NOTE Example add operator for custom types
/*
1) Recommended add own log stream type in your project
file: app/src/global/logstream.h

#ifndef APP_LOGSTREAM_H
#define APP_LOGSTREAM_H

#include "thirdparty/kors_logger/src/logstream.h"
namespace app::logger {
using Stream = kors::logger::Stream;
}

#endif // APP_LOGSTREAM_H

2) Add stream operator for your type
file: app/src/.../sometype.h

inline app::logger::Stream& operator<<(app::logger::Stream& s, const SomeType& v)
{
    s << v.toStdString();
    return s;
}
*/

namespace kors::logger {
class Stream
{
public:
    Stream() = default;

    inline Stream& operator<<(bool t) { m_ss << t; return *this; }
    inline Stream& operator<<(char t) { m_ss << t; return *this; }
    inline Stream& operator<<(signed short t) { m_ss << t; return *this; }
    inline Stream& operator<<(unsigned short t) { m_ss << t; return *this; }
    inline Stream& operator<<(char16_t t) { m_ss << static_cast<uint16_t>(t); return *this; }
    inline Stream& operator<<(char32_t t) { m_ss << static_cast<uint32_t>(t); return *this; }
    inline Stream& operator<<(signed int t) { m_ss << t; return *this; }
    inline Stream& operator<<(unsigned int t) { m_ss << t; return *this; }
    inline Stream& operator<<(signed long t) { m_ss << t; return *this; }
    inline Stream& operator<<(unsigned long t) { m_ss << t; return *this; }
    inline Stream& operator<<(signed long long t) { m_ss << t; return *this; }
    inline Stream& operator<<(unsigned long long t) { m_ss << t; return *this; }
    inline Stream& operator<<(float t) { m_ss << t; return *this; }
    inline Stream& operator<<(double t) { m_ss << t; return *this; }
    inline Stream& operator<<(const void* t) { m_ss << t; return *this; }
    inline Stream& operator<<(const char* t) { m_ss << t; return *this; }

    inline Stream& operator<<(const std::string& t) { m_ss << t; return *this; }
    inline Stream& operator<<(const std::string_view& t) { m_ss << t; return *this; }
    inline Stream& operator<<(const std::thread::id& t) { m_ss << t; return *this; }

    template<typename T>
    inline Stream& operator<<(const std::vector<T>& t)
    {
        m_ss << '[';
        for (size_t i = 0; i < t.size(); ++i) {
            *this << t.at(i);
            if (i < t.size() - 1) {
                m_ss << ',';
            }
        }
        m_ss << ']';
        return *this;
    }

#ifdef KORS_LOGGER_QT_SUPPORT
    inline Stream& operator<<(QChar t) { qt_to_ss(t); return *this; }
    inline Stream& operator<<(const QString& t) { qt_to_ss(t); return *this; }
    inline Stream& operator<<(QStringView t) { qt_to_ss(t); return *this; }
    inline Stream& operator<<(QLatin1String t) { qt_to_ss(t); return *this; }
    inline Stream& operator<<(const QByteArray& t) { qt_to_ss(t); return *this; }
    inline Stream& operator<<(const QVariant& t) { qt_to_ss(t); return *this; }
#endif

    inline std::string str() const { return m_ss.str(); }

private:

#ifdef KORS_LOGGER_QT_SUPPORT
    template<typename T>
    inline void qt_to_ss(const T& t)
    {
        QString str;
        QDebug q(&str);
        q << t;
        m_ss << str.toStdString();
    }

#endif

    std::stringstream m_ss;
};
}

#endif //KORS_LOGSTREAM_H
