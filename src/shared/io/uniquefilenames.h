/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <set>
#include <string>

namespace au::shared {
class UniqueFileNames
{
public:
    std::string registerName(const std::string& name);

private:
    std::set<std::string> m_foldedNames;
};
}
