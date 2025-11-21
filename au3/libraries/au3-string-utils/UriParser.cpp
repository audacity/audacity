/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: UriParser.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "UriParser.h"

UriFields ParseUri(std::string_view uri) noexcept
{
    UriFields result;
    auto schemeEnd = uri.find("://");
    if (schemeEnd != std::string_view::npos) {
        result.Scheme = uri.substr(0, schemeEnd);
        uri.remove_prefix(schemeEnd + 3);
    }

    auto fragmentStart = uri.find('#');
    if (fragmentStart != std::string_view::npos) {
        result.Fragment = uri.substr(fragmentStart + 1);
        uri.remove_suffix(uri.size() - fragmentStart);
    }

    auto queryStart = uri.find('?');
    if (queryStart != std::string_view::npos) {
        result.Query = uri.substr(queryStart + 1);
        uri.remove_suffix(uri.size() - queryStart);
    }

    auto pathStart = uri.find('/');
    if (pathStart != std::string_view::npos) {
        result.Path = uri.substr(pathStart + 1);
        uri.remove_suffix(uri.size() - pathStart);
    }

    auto userInfoEnd = uri.find('@');
    if (userInfoEnd != std::string_view::npos) {
        result.UserInfo = uri.substr(0, userInfoEnd);
        uri.remove_prefix(userInfoEnd + 1);
    }

    auto portStart = uri.find(':');
    if (portStart != std::string_view::npos) {
        result.Port = uri.substr(portStart + 1);
        uri.remove_suffix(uri.size() - portStart);
    }

    result.Host = uri;

    return result;
}

QueryFields ParseUriQuery(std::string_view query, std::string_view delimiter) noexcept
{
    QueryFields result;

    while (!query.empty())
    {
        const auto queryItemEnd = query.find(delimiter);
        const auto queryItem = query.substr(0, queryItemEnd);

        query.remove_prefix(
            queryItemEnd != std::string_view::npos ? queryItemEnd + 1
            : query.size());

        const auto queryItemValueStart = queryItem.find('=');

        const auto queryItemValue = queryItem.substr(
            queryItemValueStart != std::string_view::npos
            ? queryItemValueStart + 1
            : queryItem.size());

        const auto queryItemKey = queryItem.substr(
            0, queryItemValueStart != std::string_view::npos
            ? queryItemValueStart
            : queryItem.size());

        result.emplace(queryItemKey, queryItemValue);
    }

    return result;
}
