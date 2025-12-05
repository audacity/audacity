#pragma once

#include <locale>
#include <codecvt>

template<typename StringType>
typename std::enable_if<std::is_same<StringType, std::string>::value, StringType>::type
makeFilePath(const std::string& pathUTF8)
{
    return pathUTF8;
}

template<typename StringType>
typename std::enable_if<!std::is_same<StringType, std::string>::value, StringType>::type
makeFilePath(const std::string& pathUTF8)
{
    return std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>, wchar_t> {}.from_bytes(pathUTF8);
}
