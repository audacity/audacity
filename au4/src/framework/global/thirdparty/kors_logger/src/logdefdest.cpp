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
#include "logdefdest.h"

#include <iostream>

#ifdef _WIN32
#include <Windows.h>
#endif

using namespace kors::logger;

MemLogDest::MemLogDest(const LogLayout& l)
    : LogDest(l)
{
}

std::string MemLogDest::name() const
{
    return "MemLogDest";
}

void MemLogDest::write(const LogMsg& logMsg)
{
    m_stream << m_layout.output(logMsg) << std::endl;
}

std::string MemLogDest::content() const
{
    return m_stream.str();
}

// FileLogDest
FileLogDest::FileLogDest(const std::string& filePath, const LogLayout& l)
    : LogDest(l), m_filePath(filePath)
{
    m_file.open(m_filePath, std::ios_base::out | std::ios_base::app);
    if (!m_file.is_open()) {
        std::clog << "failed open log file: " << m_filePath << std::endl;
    }
}

FileLogDest::~FileLogDest()
{
    if (m_file.is_open()) {
        m_file.close();
    }
}

std::string FileLogDest::name() const
{
    return "FileLogDest";
}

void FileLogDest::write(const LogMsg& logMsg)
{
    m_file << m_layout.output(logMsg) << std::endl;
    m_file.flush();
}

// OutputDest
ConsoleLogDest::ConsoleLogDest(const LogLayout& l)
    : LogDest(l)
{
}

std::string ConsoleLogDest::name() const
{
    return "ConsoleLogDest";
}

void ConsoleLogDest::write(const LogMsg& logMsg)
{
    auto colorCode = [](Color c){
        switch (c) {
        case Color::None:    return "\033[0m";
        case Color::Black:   return "\033[1;30m";
        case Color::Red:     return "\033[1;31m";
        case Color::Green:   return "\033[1;32m";
        case Color::Yellow:  return "\033[1;33m";
        case Color::Blue:    return "\033[1;34m";
        case Color::Magenta: return "\033[1;35m";
        case Color::Cyan:    return "\033[1;36m";
        case Color::White:   return "\033[1;37m";
        }
        return "\033[0m";
    };

    std::string msg;
    msg.reserve(100);
    msg.append(colorCode(logMsg.color));
    std::string log = m_layout.output(logMsg);
    msg.append(log);
    msg.append("\033[0m");
#ifdef _WIN32
    std::wstring str = std::wstring(msg.begin(), msg.end());
    size_t preview = 0;
    for (size_t i = 0; i < str.size(); ++i) {
        if (str.at(i) == L'\n') {
            str[i] = L'\0';
            OutputDebugString(&str[preview]);
            OutputDebugString(L"\n");
            preview = i + 1;
        }
    }

    OutputDebugString(&str[preview]);
    OutputDebugString(L"\n");

#else
    std::cout << msg << std::endl;
#endif
}
