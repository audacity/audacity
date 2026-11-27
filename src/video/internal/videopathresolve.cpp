/*
* Audacity: A Digital Audio Editor
*/
#include "videopathresolve.h"

#include <filesystem>

using namespace au::video;

namespace {
std::filesystem::path toPath(const std::string& s)
{
    return std::filesystem::u8path(s);
}

std::string fromPath(const std::filesystem::path& p)
{
    const auto u8 = p.u8string();
    return std::string(u8.begin(), u8.end());
}
}

std::string au::video::makeRelativeVideoPath(const std::string& projectDir,
                                             const std::string& absolute)
{
    if (projectDir.empty() || absolute.empty()) {
        return {};
    }

    std::error_code ec;
    const std::filesystem::path relative = std::filesystem::relative(toPath(absolute), toPath(projectDir), ec);

    if (ec || relative.empty()) {
        // Different roots, most often a different drive on Windows. There is
        // no relative form, so only the absolute path can be stored.
        return {};
    }

    return fromPath(relative);
}

std::string au::video::resolveVideoPath(const std::string& absolute,
                                        const std::string& relative,
                                        const std::string& projectDir,
                                        const std::function<bool(const std::string&)>& exists)
{
    if (!exists) {
        return {};
    }

    // Relative first: a project and its media moved together is both the
    // common case and the one an absolute path gets wrong.
    if (!relative.empty() && !projectDir.empty()) {
        const std::string candidate = fromPath((toPath(projectDir) / toPath(relative)).lexically_normal());

        if (exists(candidate)) {
            return candidate;
        }
    }

    if (!absolute.empty() && exists(absolute)) {
        return absolute;
    }

    return {};
}
