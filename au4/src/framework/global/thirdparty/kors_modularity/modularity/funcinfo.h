/*
MIT License

Copyright (c) 2023 Igor Korsukov

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

#ifndef KORS__FI_H
#define KORS__FI_H

#include <string_view>
#include <algorithm>

#ifndef FUNC_SIG
#if defined(_MSC_VER)
#define FUNC_SIG __FUNCSIG__
#else
#define FUNC_SIG __PRETTY_FUNCTION__
#endif
#endif

#if defined(__GNUC__) && !defined(__clang__)
#if (__GNUC__ < 11)
#define _KORS_NO_STRINGVIEW_CONSTEXPR_METHODS
#endif
#endif

#define FUNCNAME kors::funcinfo::funcNameBySig(FUNC_SIG)
#define CLASSNAME kors::funcinfo::classNameBySig(FUNC_SIG)
#define CLASSFUNC kors::funcinfo::classFuncBySig(FUNC_SIG)
#define MODULENAME kors::funcinfo::moduleNameBySig(FUNC_SIG)

namespace kors::funcinfo {
#ifndef _KORS_NO_STRINGVIEW_CONSTEXPR_METHODS

//! NOTE Signature maybe like:
//! * ReturnType funcName(...)
//! * ReturnType some_ns::maybesub::funcName(...)
//! * ReturnType Class::funcName(...)
//! * ReturnType some_ns::maybesub::Class::funcName(...)
constexpr std::string_view funcNameBySig(const std::string_view& sig)
{
    constexpr std::string_view Colon("::");
    constexpr std::string_view ArgBegin("(");
    constexpr std::string_view Space(" ");

    std::size_t endFunc = sig.find_first_of(ArgBegin);
    if (endFunc == std::string_view::npos) {
        return sig;
    }

    std::size_t beginFunc = sig.find_last_of(Colon, endFunc);
    if (beginFunc == std::string_view::npos) {
        beginFunc = sig.find_last_of(Space, endFunc);
    }

    if (beginFunc == std::string_view::npos) {
        beginFunc = 0;
    } else {
        beginFunc += 1;
    }

    return sig.substr(beginFunc, (endFunc - beginFunc));
}

//! NOTE Signature maybe like:
//! * ReturnType Class::funcName(...)
//! * ReturnType some_ns::maybesub::Class::funcName(...)
constexpr std::string_view classNameBySig(const std::string_view& sig)
{
    constexpr std::string_view Colon("::");
    constexpr std::string_view ArgBegin("(");
    constexpr std::string_view Space(" ");

    std::size_t endFunc = sig.find_first_of(ArgBegin);
    if (endFunc == std::string_view::npos) {
        return sig;
    }

    std::size_t beginFunc = sig.find_last_of(Colon, endFunc);
    if (beginFunc == std::string_view::npos) {
        return std::string_view();
    }

    std::size_t beginClassColon = sig.find_last_of(Colon, beginFunc - 2);
    std::size_t beginClassSpace = sig.find_last_of(Space, beginFunc - 2);

    std::size_t beginClass = std::string_view::npos;
    if (beginClassColon == std::string_view::npos) {
        beginClass = beginClassSpace;
    } else if (beginClassSpace == std::string_view::npos) {
        beginClass = beginClassColon;
    } else {
        beginClass = std::max(beginClassColon, beginClassSpace);
    }

    if (beginClass == std::string_view::npos) {
        beginClass = 0;
    } else {
        beginClass += 1;
    }

    return sig.substr(beginClass, (beginFunc - 1 - beginClass));
}

//! NOTE Signature maybe like:
//! * ReturnType Class::funcName(...)
//! * ReturnType some_ns::maybesub::Class::funcName(...)
constexpr std::string_view classFuncBySig(const std::string_view& sig)
{
    constexpr std::string_view Colon("::");
    constexpr std::string_view ArgBegin("(");
    constexpr std::string_view Space(" ");

    std::size_t endFunc = sig.find_first_of(ArgBegin);
    if (endFunc == std::string_view::npos) {
        return sig;
    }

    std::size_t beginFunc = sig.find_last_of(Colon, endFunc);
    if (beginFunc == std::string_view::npos) {
        return funcNameBySig(sig);
    }

    std::size_t beginClassColon = sig.find_last_of(Colon, beginFunc - 2);
    std::size_t beginClassSpace = sig.find_last_of(Space, beginFunc - 2);

    std::size_t beginClass = std::string_view::npos;
    if (beginClassColon == std::string_view::npos) {
        beginClass = beginClassSpace;
    } else if (beginClassSpace == std::string_view::npos) {
        beginClass = beginClassColon;
    } else {
        beginClass = std::max(beginClassColon, beginClassSpace);
    }

    if (beginClass == std::string_view::npos) {
        beginClass = 0;
    } else {
        beginClass += 1;
    }

    return sig.substr(beginClass, (endFunc - beginClass));
}

//! NOTE Signature should be like
//! ReturnType xxx::modulename::maybesub::ClassName::methodName()
constexpr std::string_view moduleNameBySig(const std::string_view& sig)
{
    constexpr std::string_view ArgBegin("(");
    constexpr std::string_view Space(" ");
    constexpr std::string_view Colon("::");

    std::size_t endFunc = sig.find_first_of(ArgBegin);
    if (endFunc == std::string_view::npos) {
        return sig;
    }

    std::size_t beginFunc = sig.find_last_of(Space, endFunc);
    if (beginFunc == std::string_view::npos) {
        return std::string_view();
    }

    size_t beginModule = sig.find_first_of(Colon, beginFunc) + 2;
    if (beginModule == std::string_view::npos) {
        return std::string_view();
    }

    size_t endModule = sig.find_first_of(Colon, beginModule);
    if (endModule == std::string_view::npos) {
        return std::string_view();
    }

    return sig.substr(beginModule, (endModule - beginModule));
}

#else
//! NOTE Signature maybe like:
//! * ReturnType funcName(...)
//! * ReturnType some_ns::maybesub::funcName(...)
//! * ReturnType Class::funcName(...)
//! * ReturnType some_ns::maybesub::Class::funcName(...)
inline std::string_view funcNameBySig(const std::string_view& sig)
{
    static const std::string_view Colon("::");
    static const std::string_view ArgBegin("(");
    static const std::string_view Space(" ");

    std::size_t endFunc = sig.find_first_of(ArgBegin);
    if (endFunc == std::string_view::npos) {
        return sig;
    }

    std::size_t beginFunc = sig.find_last_of(Colon, endFunc);
    if (beginFunc == std::string_view::npos) {
        beginFunc = sig.find_last_of(Space, endFunc);
    }

    if (beginFunc == std::string_view::npos) {
        beginFunc = 0;
    } else {
        beginFunc += 1;
    }

    return sig.substr(beginFunc, (endFunc - beginFunc));
}

//! NOTE Signature maybe like:
//! * ReturnType Class::funcName(...)
//! * ReturnType some_ns::maybesub::Class::funcName(...)
inline std::string_view classNameBySig(const std::string_view& sig)
{
    static const std::string_view Colon("::");
    static const std::string_view ArgBegin("(");
    static const std::string_view Space(" ");

    std::size_t endFunc = sig.find_first_of(ArgBegin);
    if (endFunc == std::string_view::npos) {
        return sig;
    }

    std::size_t beginFunc = sig.find_last_of(Colon, endFunc);
    if (beginFunc == std::string_view::npos) {
        return std::string_view();
    }

    std::size_t beginClassColon = sig.find_last_of(Colon, beginFunc - 2);
    std::size_t beginClassSpace = sig.find_last_of(Space, beginFunc - 2);

    std::size_t beginClass = std::string_view::npos;
    if (beginClassColon == std::string_view::npos) {
        beginClass = beginClassSpace;
    } else if (beginClassSpace == std::string_view::npos) {
        beginClass = beginClassColon;
    } else {
        beginClass = std::max(beginClassColon, beginClassSpace);
    }

    if (beginClass == std::string_view::npos) {
        beginClass = 0;
    } else {
        beginClass += 1;
    }

    return sig.substr(beginClass, (beginFunc - 1 - beginClass));
}

//! NOTE Signature maybe like:
//! * ReturnType Class::funcName(...)
//! * ReturnType some_ns::maybesub::Class::funcName(...)
inline std::string_view classFuncBySig(const std::string_view& sig)
{
    static const std::string_view Colon("::");
    static const std::string_view ArgBegin("(");
    static const std::string_view Space(" ");

    std::size_t endFunc = sig.find_first_of(ArgBegin);
    if (endFunc == std::string_view::npos) {
        return sig;
    }

    std::size_t beginFunc = sig.find_last_of(Colon, endFunc);
    if (beginFunc == std::string_view::npos) {
        return funcNameBySig(sig);
    }

    std::size_t beginClassColon = sig.find_last_of(Colon, beginFunc - 2);
    std::size_t beginClassSpace = sig.find_last_of(Space, beginFunc - 2);

    std::size_t beginClass = std::string_view::npos;
    if (beginClassColon == std::string_view::npos) {
        beginClass = beginClassSpace;
    } else if (beginClassSpace == std::string_view::npos) {
        beginClass = beginClassColon;
    } else {
        beginClass = std::max(beginClassColon, beginClassSpace);
    }

    if (beginClass == std::string_view::npos) {
        beginClass = 0;
    } else {
        beginClass += 1;
    }

    return sig.substr(beginClass, (endFunc - beginClass));
}

//! NOTE Signature should be like
//! ReturnType xxx::modulename::maybesub::ClassName::methodName()
inline std::string_view moduleNameBySig(const std::string_view& sig)
{
    static const std::string_view ArgBegin("(");
    static const std::string_view Space(" ");
    static const std::string_view Colon("::");

    std::size_t endFunc = sig.find_first_of(ArgBegin);
    if (endFunc == std::string_view::npos) {
        return sig;
    }

    std::size_t beginFunc = sig.find_last_of(Space, endFunc);
    if (beginFunc == std::string_view::npos) {
        return std::string_view();
    }

    size_t beginModule = sig.find_first_of(Colon, beginFunc) + 2;
    if (beginModule == std::string_view::npos) {
        return std::string_view();
    }

    size_t endModule = sig.find_first_of(Colon, beginModule);
    if (endModule == std::string_view::npos) {
        return std::string_view();
    }

    return sig.substr(beginModule, (endModule - beginModule));
}

#endif // _KORS_NO_STRINGVIEW_CONSTEXPR_METHODS
}

#endif // KORS__FI_H
