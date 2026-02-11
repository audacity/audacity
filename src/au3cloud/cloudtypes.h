#pragma once

#include <variant>
#include <string>
#include <vector>

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

struct ProjectList {
    struct Item {
        std::string id;
        std::string username;
        std::string authorName;
        std::string slug;
        std::string name;
        std::string details;
        std::string lastSyncedSnapshotId;
        int64_t fileSize;

        int64_t created {};
        int64_t updated {};
    };

    std::vector<Item> items;

    struct Meta {
        int total = 0;
        int batches = 0;
        int thisBatchNumber = 0;
        int itemsPerBatch = 0;
    } meta;
};
struct AudioList {
    struct Item {
        std::string id;
        std::string username;
        std::string authorName;
        std::string slug;
        std::string title;
        std::vector<std::string> tags;

        int64_t created {};
    };

    std::vector<Item> items;

    struct Meta {
        int total = 0;
        int batches = 0;
        int thisBatchNumber = 0;
        int itemsPerBatch = 0;
    } meta;
};
}
