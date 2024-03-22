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
#ifndef KORS_DEFLOGDEST_H
#define KORS_DEFLOGDEST_H

#include <fstream>

#include "logger.h"

namespace kors::logger {
class MemLogDest : public LogDest
{
public:
    explicit MemLogDest(const LogLayout& l);

    std::string name() const;
    void write(const LogMsg& logMsg);

    std::string content() const;

private:
    std::stringstream m_stream;
};

class FileLogDest : public LogDest
{
public:
    FileLogDest(const std::string& filePath, const LogLayout& l);
    ~FileLogDest();

    std::string name() const;
    void write(const LogMsg& logMsg);

private:
    std::ofstream m_file;
    std::string m_filePath;
};

class ConsoleLogDest : public LogDest
{
public:
    explicit ConsoleLogDest(const LogLayout& l);

    std::string name() const;
    void write(const LogMsg& logMsg);
};
}

#endif // KORS_DEFLOGDEST_H
