// SPDX-License-Identifier: GPL-3.0-only
// MuseScore-CLA-applies

#include "projectpathutils.h"

#include <algorithm>
#include <cctype>

namespace {
std::string suffix(const std::string& path)
{
    const size_t separator = path.find_last_of("/\\");
    const size_t dot = path.find_last_of('.');
    const size_t filenameStart = separator == std::string::npos ? 0 : separator + 1;
    if (dot == std::string::npos || dot == filenameStart || dot + 1 == path.size()
        || dot < filenameStart) {
        return {};
    }

    std::string result = path.substr(dot + 1);
    std::transform(result.begin(), result.end(), result.begin(), [](unsigned char c) { return std::tolower(c); });
    return result;
}
}

std::string au::project::forceAup4Extension(const std::string& path)
{
    std::string correctedPath = path;
    std::string currentSuffix = suffix(correctedPath);

    if (!currentSuffix.empty()) {
        correctedPath.resize(correctedPath.size() - currentSuffix.size() - 1);
    }

    // Some save dialogs append the selected extension even when the suggested
    // filename already has it. Remove any remaining copy before adding the
    // canonical extension below.
    while (suffix(correctedPath) == "aup4") {
        currentSuffix = suffix(correctedPath);
        correctedPath.resize(correctedPath.size() - currentSuffix.size() - 1);
    }

    if (!correctedPath.empty() && correctedPath.back() != '.') {
        correctedPath += ".";
    }
    correctedPath += "aup4";

    return correctedPath;
}

std::string au::project::aup4SaveFilter(const std::string& label)
{
    return label + " (*.aup4)";
}
