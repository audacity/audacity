/**********************************************************************

 Audacity: A Digital Audio Editor

 @file TranslatableString.h

 Qt-native deferred-translation string. Lookup goes through
 QCoreApplication::translate; the Qt catalogue pipeline
 (lupdate/.ts/lrelease/.qm + QTranslator) is the single source of truth.

 wxString interop lives in WxMuseStringCompat.h, included at the end.
 Audacity-i18n helpers (au3::trc, au3::argDecay, au3::wxToQt, au3::qtToWx,
 au3::untranslatable(wxString)) stay in the au3:: namespace; the class
 itself is at global scope.

 **********************************************************************/

#ifndef __AUDACITY_TRANSLATABLE_STRING__
#define __AUDACITY_TRANSLATABLE_STRING__

#include <QCoreApplication>
#include <QString>

#include <algorithm>
#include <cstring>
#include <functional>
#include <memory>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

class wxString;
class TranslatableString;

namespace au3 {

//! Forward decl of the wxString overload (defined in WxMuseStringCompat.h)
//! so it's visible at template definition time -- two-phase lookup in
//! TranslatableString::arg() would otherwise freeze on the primary template
//! below and never see the wxString specialisation.
QString argDecay(const wxString& s);

inline std::string trc(const char* context, const char* key,
                       const char* disambiguation = nullptr, int n = -1)
{
    return QCoreApplication::translate(context, key, disambiguation, n).toUtf8().toStdString();
}

template<class T>
inline auto argDecay(const T& t)
{
    if constexpr (std::is_array_v<T> && std::is_same_v<std::remove_extent_t<T>, char>) {
        return QString::fromUtf8(static_cast<const char*>(t));
    } else if constexpr (std::is_array_v<T> && std::is_same_v<std::remove_extent_t<T>, wchar_t>) {
        return QString::fromWCharArray(static_cast<const wchar_t*>(t));
    } else if constexpr (std::is_same_v<T, const char*> || std::is_same_v<T, char*>) {
        return QString::fromUtf8(t);
    } else if constexpr (std::is_same_v<T, const wchar_t*> || std::is_same_v<T, wchar_t*>) {
        return QString::fromWCharArray(t);
    } else if constexpr (std::is_same_v<T, std::string>) {
        return QString::fromStdString(t);
    } else if constexpr (std::is_same_v<T, std::wstring>) {
        return QString::fromStdWString(t);
    } else {
        return t;
    }
}

} // namespace au3

class TranslatableString
{
public:
    enum StripOptions : unsigned {
        StripMenuCodes = 0x1,
        StripEllipses  = 0x2,
    };

    TranslatableString() = default;

    TranslatableString(const char* context, const char* source,
                       const char* disambiguation = nullptr, int n = -1)
        : m_context(context),
          m_source(QString::fromUtf8(source)),
          m_disambiguation(disambiguation),
          m_n(n) {}

    TranslatableString(const char* context, QString source,
                       const char* disambiguation = nullptr, int n = -1)
        : m_context(context),
          m_source(std::move(source)),
          m_disambiguation(disambiguation),
          m_n(n) {}

    static TranslatableString untranslatable(const char* s)
    { return TranslatableString(nullptr, s); }
    static TranslatableString untranslatable(const QString& s)
    { return TranslatableString(nullptr, s); }
    //! wxString overload lives in WxMuseStringCompat.h.

    bool isEmpty()        const { return m_source.isEmpty(); }
    bool empty()          const { return isEmpty(); }
    bool isTranslatable() const { return m_context && m_context[0]; }

    const QString& msgid() const { return m_source; }

    QString translated() const { return translated(m_n); }

    QString translated(int n) const
    {
        if (isEmpty()) {
            return {};
        }
        QString res = isTranslatable()
            ? QCoreApplication::translate(m_context, m_source.toUtf8().constData(),
                                          m_disambiguation, n)
            : m_source;
        for (const auto& a : m_args) {
            a->apply(res);
        }
        applyStrip(res);
        for (const auto& j : m_joined) {
            res += j.sep;
            if (j.other) {
                res += j.other->translated();
            }
        }
        return res;
    }

    QString qTranslated() const { return translated(); }
    QString qTranslated(int n) const { return translated(n); }

    //! UTF-8-correct wxString; defined in WxMuseStringCompat.h where wxString
    //! is complete. Returning std::string here would route every legacy call
    //! site through wxString's locale-based implicit ctor and mangle non-ASCII.
    wxString Translation() const;

    //! Source with args applied, no catalogue lookup. For diagnostics and
    //! internal pattern matching where translation would be wrong.
    QString debugStr() const
    {
        if (isEmpty()) {
            return {};
        }
        QString res = m_source;
        for (const auto& a : m_args) {
            a->apply(res);
        }
        return res;
    }

    TranslatableString stripped(unsigned opts = StripMenuCodes) const
    {
        TranslatableString res = *this;
        res.m_strip |= opts;
        return res;
    }

    TranslatableString Join(const TranslatableString& other, const QString& sep = {}) const
    {
        TranslatableString res = *this;
        res.m_joined.push_back({ std::make_shared<TranslatableString>(other), sep });
        return res;
    }

    TranslatableString Join(const TranslatableString& other, const char* sep) const
    {
        return Join(other, QString::fromUtf8(sep));
    }

    TranslatableString operator+(const TranslatableString& rhs) const
    {
        return Join(rhs, QString());
    }

    TranslatableString& operator+=(const TranslatableString& rhs)
    {
        m_joined.push_back({ std::make_shared<TranslatableString>(rhs), QString() });
        return *this;
    }

    //! Inline (not out-of-line) because GCC rejects the qualified
    //! `struct ::TranslatableString::Arg` declarator that Clang accepts.
    template<class... Args>
    TranslatableString arg(const Args&... a) const
    {
        // au3::argDecay qualified so two-phase lookup picks up the wxString
        // overload from WxMuseStringCompat.h; unqualified would freeze on
        // the primary template above.
        TranslatableString res = *this;
        res.m_args.push_back(
            std::make_shared<Arg<decltype(au3::argDecay(a))...> >(au3::argDecay(a)...));
        return res;
    }

    template<class... Args>
    TranslatableString Format(const Args&... a) const
    {
        TranslatableString res = *this;
        ((res = res.arg(a)), ...);
        return res;
    }

    bool operator==(const TranslatableString& other) const
    {
        return (m_context == other.m_context
                || (m_context && other.m_context
                    && std::strcmp(m_context, other.m_context) == 0))
            && m_source == other.m_source
            && (m_disambiguation == other.m_disambiguation
                || (m_disambiguation && other.m_disambiguation
                    && std::strcmp(m_disambiguation, other.m_disambiguation) == 0))
            && m_n == other.m_n
            && m_strip == other.m_strip
            // vector::operator== would compare shared_ptr identity, not IArg::equals().
            && m_args.size() == other.m_args.size()
            && std::equal(m_args.begin(), m_args.end(), other.m_args.begin(),
                          [](const std::shared_ptr<const IArg>& a,
                             const std::shared_ptr<const IArg>& b) {
                              return a == b || (a && b && a->equals(*b));
                          });
    }

    bool operator!=(const TranslatableString& other) const { return !operator==(other); }

    //! Public so the wxString Arg specialisation in WxMuseStringCompat.h
    //! can plug in. Treat as private otherwise.
    struct IArg {
        virtual ~IArg() = default;
        virtual void apply(QString& res) const = 0;
        virtual bool equals(const IArg& other) const = 0;
        bool operator==(const IArg& other) const { return equals(other); }
    };

    template<class... Args>
    struct Arg : public IArg
    {
        std::tuple<Args...> args;

        Arg(const Args&... a) : args(a...) {}

        void apply(QString& res) const override
        {
            res = std::apply(
                [&](const Args&... a) { return res.arg(makeQArg(a)...); },
                args);
        }

        template<class T>
        static auto makeQArg(const T& t)
        {
            if constexpr (std::is_same_v<T, TranslatableString>) {
                return t.translated();
            } else {
                return t;
            }
        }

        bool equals(const IArg& other) const override
        {
            auto p = dynamic_cast<const Arg<Args...>*>(&other);
            return p && p->args == args;
        }
    };

private:
    void applyStrip(QString& res) const
    {
        if (!m_strip) {
            return;
        }
        if (m_strip & StripMenuCodes) {
            res = stripMenuCodes(res);
        }
        if (m_strip & StripEllipses) {
            if (res.endsWith(QLatin1String("..."))) {
                res.chop(3);
            } else if (res.endsWith(QChar(0x2026))) {
                res.chop(1);
            }
        }
    }

    static QString stripMenuCodes(const QString& in)
    {
        QString out;
        out.reserve(in.size());
        for (int i = 0; i < in.size(); ++i) {
            QChar c = in.at(i);
            if (c == QLatin1Char('\t')) {
                break;
            }
            if (c == QLatin1Char('&')) {
                ++i;
                if (i >= in.size()) {
                    break;
                }
                out += in.at(i);
                continue;
            }
            out += c;
        }
        return out;
    }

    struct Joined {
        std::shared_ptr<const TranslatableString> other;
        QString sep;
    };

    const char* m_context        = nullptr;
    QString     m_source;
    const char* m_disambiguation = nullptr;
    int         m_n              = -1;

    std::vector<std::shared_ptr<const IArg> > m_args;
    std::vector<Joined>                       m_joined;
    unsigned                                  m_strip = 0;
};

using TranslatableStrings = std::vector<::TranslatableString>;

namespace std {
template<> struct hash<::TranslatableString> {
    size_t operator()(const ::TranslatableString& s) const noexcept
    {
        return qHash(s.msgid());
    }
};
} // namespace std

inline bool TranslationLess(const ::TranslatableString& a, const ::TranslatableString& b)
{
    return a.translated() < b.translated();
}

#include "WxMuseStringCompat.h"

#endif
