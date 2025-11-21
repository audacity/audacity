/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file UrlDecode.cpp
 @brief Define a function to decode an URL encode string.

 Dmitry Vedenko
 **********************************************************************/

#include "UrlDecode.h"

#include "HexHelpers.h"

namespace audacity {
std::string UrlDecode(const std::string& url)
{
    std::string result;

    const size_t length = url.length();

    for (auto it = url.begin(), end = url.end(); it != end; ++it) {
        const char c = *it;

        if (c != '%') {
            result.push_back(c);
        } else {
            if (++it == url.end()) {
                break; // Malformed input string
            }
            const char c1 = *it;

            if (++it == url.end()) {
                break; // Malformed input string
            }
            const char c2 = *it;

            result.push_back(HexCharToNum(c1) << 4 | HexCharToNum(c2));
        }
    }

    return result;
}
}
