/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: StringUtils.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "StringUtils.h"

#include "CodeConversions.h"

std::string ToLower(const std::string& str)
{
    return audacity::ToUTF8(ToLower(audacity::ToWXString(str)));
}

std::string ToLower(const std::string_view& str)
{
    return audacity::ToUTF8(ToLower(audacity::ToWXString(str)));
}

std::string ToLower(const char* str)
{
    return audacity::ToUTF8(ToLower(audacity::ToWXString(str)));
}

std::wstring ToLower(const std::wstring& str)
{
    return audacity::ToWString(ToLower(audacity::ToWXString(str)));
}

std::wstring ToLower(const std::wstring_view& str)
{
    return audacity::ToWString(ToLower(audacity::ToWXString(str)));
}

std::wstring ToLower(const wchar_t* str)
{
    return audacity::ToWString(ToLower(audacity::ToWXString(str)));
}

wxString ToLower(const wxString& str)
{
    return str.Lower();
}

std::string ToUpper(const std::string& str)
{
    return audacity::ToUTF8(ToUpper(audacity::ToWXString(str)));
}

std::string ToUpper(const std::string_view& str)
{
    return audacity::ToUTF8(ToUpper(audacity::ToWXString(str)));
}

std::string ToUpper(const char* str)
{
    return audacity::ToUTF8(ToUpper(audacity::ToWXString(str)));
}

std::wstring ToUpper(const std::wstring& str)
{
    return audacity::ToWString(ToUpper(audacity::ToWXString(str)));
}

std::wstring ToUpper(const std::wstring_view& str)
{
    return audacity::ToWString(ToUpper(audacity::ToWXString(str)));
}

std::wstring ToUpper(const wchar_t* str)
{
    return audacity::ToWString(ToUpper(audacity::ToWXString(str)));
}

wxString ToUpper(const wxString& str)
{
    return str.Upper();
}
