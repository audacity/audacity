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

#ifndef KORS_LOG_BASE_H
#define KORS_LOG_BASE_H

#include <cassert> // IWYU pragma: export

#include "funcinfo.h" // IWYU pragma: export
#include "logger.h" // IWYU pragma: export

//! Log

#ifndef LOG_TAG
#define LOG_TAG CLASSFUNC
#endif

#define IF_LOGLEVEL(level)  if (kors::logger::Logger::instance()->isLevel(level))

#define LOG_STREAM(type, tag, color) kors::logger::LogInput(type, tag, color).stream

#define LOGE_T(tag) IF_LOGLEVEL(kors::logger::Level::Normal) LOG_STREAM(kors::logger::Logger::ERRR, tag, kors::logger::Color::Red)
#define LOGW_T(tag) IF_LOGLEVEL(kors::logger::Level::Normal) LOG_STREAM(kors::logger::Logger::WARN, tag, kors::logger::Color::Yellow)
#define LOGI_T(tag) IF_LOGLEVEL(kors::logger::Level::Normal) LOG_STREAM(kors::logger::Logger::INFO, tag, kors::logger::Color::Green)
#define LOGD_T(tag) IF_LOGLEVEL(kors::logger::Level::Debug) LOG_STREAM(kors::logger::Logger::DEBG, tag, kors::logger::Color::None)
#define LOGDA_T(tag) IF_LOGLEVEL(kors::logger::Level::Debug) LOG_STREAM(kors::logger::Logger::DEBG, tag, kors::logger::Color::Cyan)

#define LOGE LOGE_T(LOG_TAG)
#define LOGW LOGW_T(LOG_TAG)
#define LOGI LOGI_T(LOG_TAG)
#define LOGD LOGD_T(LOG_TAG)
#define LOGDA LOGDA_T(LOG_TAG)      // active debug
#define LOGN if (0) LOGD_T(LOG_TAG) // compiling, but no output

//! Useful macros
#define DO_ASSERT_X(cond, msg) \
    if (!(cond)) { \
        LOGE() << "\"ASSERT FAILED!\": " << msg << ", file: " << __FILE__ << ", line: " << __LINE__; \
        assert(cond); \
    } \

#define DO_ASSERT(cond) DO_ASSERT_X(cond, #cond)
#define ASSERT_X(msg) DO_ASSERT_X(false, msg)

#define IF_ASSERT_FAILED_X(cond, msg) \
    DO_ASSERT_X(cond, msg) \
    if (!(cond)) \

#define IF_ASSERT_FAILED(cond) IF_ASSERT_FAILED_X(cond, #cond)

#define IF_FAILED(cond) \
    if (!(cond)) { \
        LOGE() << "\"FAILED!\": " << #cond << ", file: " << __FILE__ << ", line: " << __LINE__; \
    } \
    if (!(cond)) \

#define UNUSED(x) (void)x;

#define UNREACHABLE \
    LOGE() << "\"UNREACHABLE!\": " << ", file: " << __FILE__ << ", line: " << __LINE__; \
    ASSERT_X("UNREACHABLE was reached"); \

#define DEPRECATED LOGD() << "This function deprecated!!"
#define DEPRECATED_USE(use) LOGD() << "This function deprecated!! Use:" << use
#define NOT_IMPLEMENTED LOGW() << "Not implemented!!"
#define NOT_IMPL_RETURN NOT_IMPLEMENTED return
#define NOT_SUPPORTED LOGW() << "Not supported!!"
#define NOT_SUPPORTED_USE(use) LOGW() << "Not supported!! Use:" << use

#if __has_cpp_attribute(fallthrough)
#undef FALLTHROUGH
#define FALLTHROUGH [[fallthrough]]
#else
#define FALLTHROUGH (void)0
#endif

#endif // KORS_LOG_BASE_H
