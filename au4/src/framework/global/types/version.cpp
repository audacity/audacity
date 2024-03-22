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
#include "version.h"

#include <array>
#include "log.h"

static const mu::Char SUFFIX_DELIMITER = '-';

using namespace mu;

static std::array<int, 3> parseVersion(const mu::String& versionString, bool& ok)
{
    std::array<int, 3> result { 0, 0, 0 };

    if (versionString.isEmpty()) {
        ok = false;
        return result;
    }

    size_t componentIdx = 0;
    int curNum = 0;

    for (size_t i = 0; i < versionString.size(); ++i) {
        char ch = versionString[i];
        if (ch == '.' || ch == '\0') {
            result.at(componentIdx++) = curNum;
            curNum = 0;
        } else if ('0' <= ch && ch <= '9') {
            curNum = curNum * 10 + (ch - '0');
        } else {
            ok = false;
            return result;
        }
    }

    result.at(componentIdx) = curNum;

    ok = true;
    return result;
}

static std::pair<mu::String, int> parseVersionSuffix(const mu::String& suffix, bool& ok)
{
    if (suffix.isEmpty()) {
        ok = false;
        return std::make_pair(mu::String(), 0);
    }

    mu::StringList suffixComponents = suffix.split('.');

    ok = true;
    return std::make_pair(suffixComponents.front(), (suffixComponents.size() > 1 ? suffixComponents[1].toInt() : 0));
}

Version::Version(int major, int minor, int patch, const String& suffix, int suffixVersion)
    : m_major(major), m_minor(minor), m_patch(patch), m_suffix(suffix), m_suffixVersion(suffixVersion)
{
}

Version::Version(const mu::String& versionStr)
{
    String version = versionStr.left(versionStr.indexOf(SUFFIX_DELIMITER));

    bool ok = true;
    std::array<int, 3> versionComponents = parseVersion(version, ok);
    if (!ok) {
        return;
    }

    m_major = versionComponents[0];
    m_minor = versionComponents[1];
    m_patch = versionComponents[2];

    if (!versionStr.contains(SUFFIX_DELIMITER)) {
        return;
    }

    setSuffix(versionStr.right(versionStr.size() - versionStr.indexOf(SUFFIX_DELIMITER) - 1));
}

Version::Version(const std::string& versionStr)
    : Version(mu::String::fromStdString(versionStr))
{
}

int Version::majorVersion() const
{
    return m_major;
}

int Version::minorVersion() const
{
    return m_minor;
}

int Version::patchVersion() const
{
    return m_patch;
}

mu::String Version::suffix() const
{
    return m_suffix;
}

int Version::suffixVersion() const
{
    return m_suffixVersion;
}

void Version::setSuffix(const String& suffix)
{
    bool ok = true;
    std::pair<String, int> versionSuffix = parseVersionSuffix(suffix, ok);
    if (!ok) {
        return;
    }

    m_suffix = versionSuffix.first;
    m_suffixVersion = versionSuffix.second;
}

bool Version::preRelease() const
{
    return !suffix().isEmpty();
}

mu::String Version::toString()
{
    String res = String(u"%1.%2.%3").arg(m_major, m_minor, m_patch);

    if (!m_suffix.isEmpty()) {
        res.append(SUFFIX_DELIMITER);
        res.append(m_suffix + (m_suffixVersion > 0 ? u"." + String::number(m_suffixVersion) : u""));
    }

    return res;
}

bool Version::operator <(const Version& other) const
{
    if (m_major > other.majorVersion()) {
        return false;
    } else if (m_major == other.majorVersion()) {
        if (m_minor > other.minorVersion()) {
            return false;
        } else if (m_minor == other.minorVersion()) {
            if (m_patch > other.patchVersion()) {
                return false;
            } else if (m_patch == other.patchVersion()) {
                if (m_suffix.isEmpty()) {
                    return false;
                }

                if (other.suffix().isEmpty()) {
                    return true;
                }

                static mu::StringList suffixes {
                    u"dev",
                    u"alpha",
                    u"beta",
                    u"rc"
                };

                auto currentIt = std::find_if(suffixes.cbegin(), suffixes.cend(), [suffix=m_suffix](const String& s) {
                    return s.startsWith(suffix);
                });

                auto updateIt = std::find_if(suffixes.cbegin(), suffixes.cend(), [suffix=other.suffix()](const String& s) {
                    return s.startsWith(suffix);
                });

                if (currentIt == suffixes.cend() || updateIt == suffixes.cend()) {
                    LOGE() << "Invalid version suffix; current " << m_suffix << ", update " << other.suffix();
                    return true;
                }

                if (currentIt < updateIt) {
                    return true;
                }

                if (currentIt == updateIt) {
                    return m_suffixVersion < other.suffixVersion();
                }

                return false;
            }
        }
    }

    return true;
}

bool Version::operator ==(const Version& other) const
{
    return m_major == other.majorVersion()
           && m_minor == other.minorVersion()
           && m_patch == other.patchVersion()
           && m_suffix == other.suffix()
           && m_suffixVersion == other.suffixVersion();
}

bool Version::operator <=(const Version& other) const
{
    if (operator ==(other)) {
        return true;
    }

    return operator <(other);
}
