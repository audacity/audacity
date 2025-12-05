/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CurlStringList.cpp
 @brief Define a RAII wrapper for the curl_slist.

 Dmitry Vedenko
 **********************************************************************/

#include "CurlStringList.h"

#include <curl/curl.h>

namespace audacity {
namespace network_manager {
CurlStringList::CurlStringList (CurlStringList&& rhs) noexcept
    : mList(rhs.mList)
{
    rhs.mList = nullptr;
}

CurlStringList::~CurlStringList () noexcept
{
    curl_slist_free_all(mList);
}

CurlStringList& CurlStringList::operator=(CurlStringList&& rhs) noexcept
{
    std::swap(mList, rhs.mList);
    return *this;
}

void CurlStringList::append(const std::string& string) noexcept
{
    mList = curl_slist_append(mList, string.c_str());
}

void CurlStringList::append(const char* string) noexcept
{
    mList = curl_slist_append(mList, string);
}

curl_slist* CurlStringList::getCurlList() const noexcept
{
    return mList;
}
}
}
