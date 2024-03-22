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
#ifndef MU_GLOBAL_TRANSLATABLESTRING_H
#define MU_GLOBAL_TRANSLATABLESTRING_H

#include "translation.h"

#include "types/string.h"

#ifndef NO_QT_SUPPORT
#include <QString>
#endif

namespace mu {
//! Note: in order to make the string visible for `lupdate`,
//! you must write TranslatableString(...) explicitly.
class TranslatableString
{
public:
    const char* context = nullptr;
    String str;
    const char* disambiguation = nullptr;

    TranslatableString() = default;
    TranslatableString(const char* context, const char* str, const char* disambiguation = nullptr)
        : context(context), str(String::fromUtf8(str)), disambiguation(disambiguation) {}
    TranslatableString(const char* context, const String& str, const char* disambiguation = nullptr)
        : context(context), str(str), disambiguation(disambiguation) {}

    static TranslatableString untranslatable(const char* str)
    {
        return TranslatableString(nullptr, str, nullptr);
    }

    static TranslatableString untranslatable(const String& str)
    {
        return TranslatableString(nullptr, str, nullptr);
    }

    inline bool isEmpty() const
    {
        return str.empty();
    }

    inline bool isTranslatable() const
    {
        return context && context[0];
    }

    inline String translated(int n = -1) const
    {
        if (isEmpty()) {
            return {};
        }

        String res = isTranslatable() ? mtrc(context, str, disambiguation, n) : str;

        for (const auto& arg : args) {
            arg->apply(res);
        }

        return res;
    }

#ifndef NO_QT_SUPPORT
    inline QString qTranslated(int n = -1) const
    {
        if (isEmpty()) {
            return {};
        }

        QString res = isTranslatable() ? qtrc(context, str, disambiguation, n) : str.toQString();

        for (const auto& arg : args) {
            arg->apply(res);
        }

        return res;
    }

#endif

    template<typename ... Args>
    inline TranslatableString arg(const Args& ...) const;

    bool operator ==(const TranslatableString& other) const
    {
        return (context == other.context || (context && other.context && strcmp(context, other.context) == 0))
               && str == other.str
               && (disambiguation == other.disambiguation
                   || (disambiguation && other.disambiguation && strcmp(disambiguation, other.disambiguation) == 0))
               && args == other.args;
    }

    bool operator !=(const TranslatableString& other) const
    {
        return !operator==(other);
    }

private:
    struct IArg {
        virtual ~IArg() = default;

        virtual void apply(String& res) const = 0;
#ifndef NO_QT_SUPPORT
        virtual void apply(QString& res) const = 0;
#endif
    };

    template<typename ... Args>
    struct Arg;

    std::vector<std::shared_ptr<const IArg> > args;
};

template<typename ... Args>
struct TranslatableString::Arg : public TranslatableString::IArg
{
    std::tuple<Args...> args;

    Arg(const Args&... args)
        : args(args ...) {}

    void apply(String& res) const override
    {
        res = std::apply([&](const Args&... args) { return res.arg(makeArg(args)...); }, args);
    }

    template<typename T>
    static inline auto makeArg(const T& t)
    {
        if constexpr (std::is_same_v<T, TranslatableString>) {
            return t.translated();
        } else {
            return t;
        }
    }

#ifndef NO_QT_SUPPORT
    void apply(QString& res) const override
    {
        res = std::apply([&](const Args&... args) { return res.arg(makeQArg(args)...); }, args);
    }

    template<typename T>
    static inline auto makeQArg(const T& t)
    {
        if constexpr (std::is_same_v<T, TranslatableString>) {
            return t.qTranslated();
// *INDENT-OFF* // Uncrustify doesn't understand `else if constexpr`
        } else if constexpr (std::is_same_v<T, String>) {
// *INDENT-ON*
            return t.toQString();
        } else {
            return t;
        }
    }

#endif
};

template<typename Int>
struct TranslatableString::Arg<Int> : public TranslatableString::IArg
{
    const Int arg;

    Arg(Int arg) : arg(arg) {
    }

    void apply(String& res) const override
    {
        res = res.arg(arg);
    }

#ifndef NO_QT_SUPPORT
    void apply(QString& res) const override
    {
        res = res.arg(arg);
    }

#endif
};

template<>
struct TranslatableString::Arg<String> : public TranslatableString::IArg
{
    const String arg;

    template<typename ... T>
    Arg(const T& ... arg) : arg(arg ...) {
    }

    void apply(String& res) const override
    {
        res = res.arg(arg);
    }

#ifndef NO_QT_SUPPORT
    void apply(QString& res) const override
    {
        res = res.arg(arg.toQString());
    }

#endif
};

template<>
struct TranslatableString::Arg<TranslatableString> : public TranslatableString::IArg
{
    const TranslatableString arg;

    Arg(const TranslatableString& arg) : arg(arg) {
    }

    void apply(String& res) const override
    {
        res = res.arg(arg.translated());
    }

#ifndef NO_QT_SUPPORT
    void apply(QString& res) const override
    {
        res = res.arg(arg.qTranslated());
    }

#endif
};

template<typename ... Args>
TranslatableString TranslatableString::arg(const Args& ... args) const
{
    TranslatableString res = *this;
    res.args.push_back(std::make_shared<Arg<Args...> >(args ...));
    return res;
}
}

#endif // MU_GLOBAL_TRANSLATABLESTRING_H
