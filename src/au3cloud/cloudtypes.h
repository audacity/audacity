#pragma once

#include <variant>
#include <string>

namespace au::au3cloud {
struct NotAuthorized {
    std::string error;

    NotAuthorized() = default;
    explicit NotAuthorized(std::string err)
        : error(std::move(err)) {}
};

struct Authorizing {};

struct Authorized {};

using AuthState = std::variant<NotAuthorized, Authorizing, Authorized>;

struct AccountInfo {
    std::string id;
    std::string userSlug;
    std::string displayName;
    std::string avatarPath;
};
}
