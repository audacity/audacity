/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file UrlEncode.h
 @brief Declare a function to perform URL encoding of a string.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <string>

namespace audacity {
STRING_UTILS_API std::string UrlEncode(const std::string& url);
}
