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
#ifndef MU_GLOBAL_URI_H
#define MU_GLOBAL_URI_H

#include <string>
#include <vector>
#include <map>

#include "val.h"

namespace mu {
class Uri
{
public:
    Uri() = default;
    explicit Uri(const std::string& str);

    using Scheme = std::string;
    static const Scheme MuseScore;
    static const Scheme Http;
    static const Scheme Https;

    bool isValid() const;

    Scheme scheme() const;
    std::string path() const;

    inline bool operator==(const Uri& uri) const { return m_path == uri.m_path && m_scheme == uri.m_scheme; }
    inline bool operator!=(const Uri& uri) const { return !(*this == uri); }
    inline bool operator <(const Uri& uri) const
    {
        if (m_scheme != uri.m_scheme) {
            return m_scheme < uri.m_scheme;
        }
        return m_path < uri.m_path;
    }

    std::string toString() const;

private:

    Scheme m_scheme;
    std::string m_path;
};

class UriQuery
{
public:

    using Params = std::map<std::string /*key*/, Val>;

    UriQuery() = default;
    explicit UriQuery(const std::string& str);
    explicit UriQuery(const Uri& uri);

    const Uri& uri() const;
    bool isValid() const;

    const Params& params() const;
    Val param(const std::string& key, const Val& def = Val()) const;
    void addParam(const std::string& key, const Val& val);
    UriQuery addingParam(const std::string& key, const Val& val) const;
    bool contains(const std::string& key) const;

    std::string toString() const;

    bool operator==(const UriQuery& query) const;
    bool operator!=(const UriQuery& query) const;

private:

    void parceParams(const std::string& str, Params& out) const;
    void extractQuotedStrings(const std::string& str, std::vector<std::string>& out) const;

    Uri m_uri;
    Params m_params;
};
}

#endif // MU_GLOBAL_URI_H
