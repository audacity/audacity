/**********************************************************************

 Audacity: A Digital Audio Editor

 @file CommandLineArgs.cpp

 Paul Licameli

**********************************************************************/
#include "CommandLineArgs.h"

int CommandLineArgs::argc;
const char* const* CommandLineArgs::argv;

#ifdef _WIN32
#include <windows.h>

#include <locale>
#include <codecvt>

namespace CommandLineArgs {
MSWParser::MSWParser()
{
    wideArgv = ::CommandLineToArgvW(::GetCommandLineW(), &argc);
    for (size_t ii = 0; ii < argc; ++ii) {
        auto begin = wideArgv[ii];
        auto end = begin + wcslen(begin);

        narrowArgv.emplace_back(
            std::wstring_convert<std::codecvt_utf8<wchar_t> >().to_bytes(
                begin, end));
    }

    for (const auto& arg : narrowArgv) {
        argv.push_back(arg.c_str());
    }

    argv.push_back(nullptr);
}

MSWParser::~MSWParser()
{
    if (wideArgv) {
        ::LocalFree(wideArgv);
    }
}
} // namespace CommandLineArgs
#endif
