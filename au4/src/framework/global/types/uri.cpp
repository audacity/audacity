/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "uri.h"

#include "../stringutils.h"

#include "log.h"

using namespace mu;

const Uri::Scheme Uri::MuseScore("musescore");
const Uri::Scheme Uri::Http("http");
const Uri::Scheme Uri::Https("https");

static const std::string URI_VAL_TRUE("true");
static const std::string URI_VAL_FALSE("false");

// musescore://module/target/name

Uri::Uri(const std::string& str)
{
    auto schemePos = str.find(':');
    if (schemePos != std::string::npos) {
        m_scheme = str.substr(0, schemePos);
    }

    auto paramsPos = str.find('?');
    auto pathPos= (schemePos != std::string::npos) ? (schemePos + 3) : 0;
    size_t pathN = (paramsPos != std::string::npos) ? (paramsPos - pathPos) : std::string::npos;

    m_path = str.substr(pathPos, pathN);
}

bool Uri::isValid() const
{
    if (m_scheme.empty()) {
        return false;
    }

    if (m_path.empty()) {
        return false;
    }

    return true;
}

Uri::Scheme Uri::scheme() const
{
    return m_scheme;
}

std::string Uri::path() const
{
    return m_path;
}

std::string Uri::toString() const
{
    return m_scheme + "://" + m_path;
}

// musescore://module/target/name?param1=value1&paramn=valuen

UriQuery::UriQuery(const std::string& str)
    : m_uri(str)
{
    parceParams(str, m_params);
}

UriQuery::UriQuery(const Uri& uri)
    : m_uri(uri)
{
}

void UriQuery::parceParams(const std::string& uri, Params& out) const
{
    auto paramsPos = uri.find('?');
    if (paramsPos == std::string::npos) {
        return;
    }

    std::string paramsStr = uri.substr(paramsPos + 1);

    strings::trim(paramsStr);

    std::map<std::string, std::string> placeholders;
    std::vector<std::string> quotesStrings;
    extractQuotedStrings(paramsStr, quotesStrings);
    for (size_t i = 0; i < quotesStrings.size(); ++i) {
        std::string key = "s" + std::to_string(i);
        const std::string& val = quotesStrings.at(i);

        strings::replace(paramsStr, val, key);
        placeholders[key] = val;
    }

    std::vector<std::string> paramsPairs;
    strings::split(paramsStr, paramsPairs, "&");

    for (const std::string& pair : paramsPairs) {
        std::vector<std::string> param;
        strings::split(pair, param, "=");
        if (param.size() != 2) {
            LOGE() << "Invalid param: " << pair << ", in uri: " << uri;
            continue;
        }
        std::string key = param.at(0);
        strings::trim(key);

        std::string val = param.at(1);

        //! NOTE Val is bool?
        if (URI_VAL_TRUE == val || URI_VAL_FALSE == val) {
            out[key] = Val(val == URI_VAL_TRUE);
            continue;
        }

        auto it = placeholders.find(val);
        if (it != placeholders.end()) {
            val = it->second;
        }

        strings::trim(val);

        if (val.size() > 2 && val.at(0) == '\'' && val.at(val.size() - 1) == '\'') {
            val = val.substr(1, val.size() - 2);
        }

        out[key] = Val(val);
    }
}

void UriQuery::extractQuotedStrings(const std::string& str, std::vector<std::string>& out) const
{
    //! NOTE It is necessary to get substrings limited to single quotes from a string
    //! Example - "path='path/to/file.jpg'"

    int bi = -1;
    for (size_t i = 0; i < str.size(); ++i) {
        if (str.at(i) == u'\'') {
            if (bi == -1) { // begin quotes string
                bi = int(i);
            } else {  // end quotes string
                out.push_back(str.substr(bi, i - bi + 1));
                bi = -1;
            }
        }
    }
}

std::string UriQuery::toString() const
{
    std::string str = m_uri.toString();
    if (!m_params.empty()) {
        str += "?";
        for (auto it = m_params.cbegin(); it != m_params.cend(); ++it) {
            str += it->first + "=" + it->second.toString() + "&";
        }

        str.erase(str.size() - 1);
    }
    return str;
}

const Uri& UriQuery::uri() const
{
    return m_uri;
}

bool UriQuery::isValid() const
{
    return m_uri.isValid();
}

const UriQuery::Params& UriQuery::params() const
{
    return m_params;
}

mu::Val UriQuery::param(const std::string& key, const Val& def) const
{
    auto it = m_params.find(key);
    if (it == m_params.end()) {
        return def;
    }
    return it->second;
}

void UriQuery::addParam(const std::string& key, const Val& val)
{
    m_params[key] = val;
}

UriQuery UriQuery::addingParam(const std::string& key, const Val& val) const
{
    UriQuery copy(*this);
    copy.addParam(key, val);
    return copy;
}

bool UriQuery::contains(const std::string& key) const
{
    return m_params.count(key) > 0;
}

bool UriQuery::operator==(const UriQuery& query) const
{
    return m_uri == query.m_uri && m_params == query.m_params;
}

bool UriQuery::operator!=(const UriQuery& query) const
{
    return !(*this == query);
}
