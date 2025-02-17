/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: UriParser.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <string_view>
#include <unordered_map>

struct UriFields final {
    std::string_view Scheme;
    std::string_view UserInfo;
    std::string_view Host;
    std::string_view Port;
    std::string_view Path;
    std::string_view Query;
    std::string_view Fragment;
};

//! Parses URI and returns UriFields structure with parsed fields.
//! {Scheme}://[{UserInfo}@]{Host}[:{Port}][/{Path}][?{Query}][#{Fragment}]
STRING_UTILS_API UriFields ParseUri(std::string_view uri) noexcept;

using QueryFields = std::unordered_map<std::string_view, std::string_view>;

//! Parses URI query and returns QueryFields structure with parsed fields.
STRING_UTILS_API QueryFields ParseUriQuery(
    std::string_view query, std::string_view delimiter = "&") noexcept;
