/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CurlStringList.h
 @brief Declare a RAII wrapper for the curl_slist.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <string>

struct curl_slist;

namespace audacity {
namespace network_manager {
class CurlStringList final
{
public:
    CurlStringList () = default;
    CurlStringList (CurlStringList&& rhs) noexcept;

    ~CurlStringList () noexcept;

    CurlStringList& operator =(CurlStringList&& rhs) noexcept;

    void append(const std::string& string) noexcept;
    void append(const char* string) noexcept;

    curl_slist* getCurlList() const noexcept;

private:
    curl_slist* mList { nullptr };
};
}
}
