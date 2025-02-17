/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file UrlEncode.cpp
 @brief Define a function to perform URL encoding of a string.

 Dmitry Vedenko
 **********************************************************************/

#include "UrlEncode.h"

namespace audacity {
std::string UrlEncode(const std::string& url)
{
    std::string escaped;

    for (char c : url) {
        if (('0' <= c && c <= '9')
            || ('A' <= c && c <= 'Z')
            || ('a' <= c && c <= 'z')
            || (c == '~' || c == '-' || c == '_' || c == '.')
            ) {
            escaped.push_back(c);
        } else {
            static const char symbolLookup[] = "0123456789ABCDEF";

            escaped.push_back('%');

            escaped.push_back(symbolLookup[(c & 0xF0) >> 4]);
            escaped.push_back(symbolLookup[(c & 0x0F) >> 0]);
        }
    }

    return escaped;
}
}
