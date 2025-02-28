/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginInterface.cpp

**********************************************************************/
#include "PluginInterface.h"
#include <algorithm>

PluginManagerInterface::~PluginManagerInterface() = default;

namespace {
std::vector<long> Split(const PluginRegistryVersion& regver)
{
    std::vector<long> result;
    auto strings = wxSplit(regver, '.');
    std::transform(strings.begin(), strings.end(), std::back_inserter(result),
                   [](const wxString& string) {
        long value;
        string.ToLong(&value);
        return value;
    });
    return result;
}
}

bool Regver_eq(
    const PluginRegistryVersion& regver1, const PluginRegistryVersion& regver2)
{
    auto numbers1 = Split(regver1),
         numbers2 = Split(regver2);
    return std::equal(
        regver1.begin(), regver1.end(), regver2.begin(), regver2.end());
}

bool Regver_lt(
    const PluginRegistryVersion& regver1, const PluginRegistryVersion& regver2)
{
    auto numbers1 = Split(regver1),
         numbers2 = Split(regver2);
    return std::lexicographical_compare(
        regver1.begin(), regver1.end(), regver2.begin(), regver2.end());
}
