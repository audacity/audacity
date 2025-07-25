/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <fstream>
#include <string>
#include <iostream>

#if defined(_WIN32)
    #include <windows.h>
#else
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <unistd.h>
#endif

namespace testtools {
inline bool copyFile(const std::string& from, const std::string& to)
{
    std::ifstream src(from, std::ios::binary);
    std::ofstream dst(to, std::ios::binary);

    if (!src.is_open()) {
        std::cerr << "copyFile: failed to open source file: " << from << "\n";
        return false;
    }
    if (!dst.is_open()) {
        std::cerr << "copyFile: failed to open destination file: " << to << "\n";
        return false;
    }

    dst << src.rdbuf();
    if (!dst.good() || !src.good()) {
        std::cerr << "copyFile: I/O error while copying from '" << from << "' to '" << to << "'\n";
        return false;
    }

    return true;
}

#if defined(_WIN32)
inline bool removePermissionBits(const std::string& file, int bitsToRemove)
{
    // No-op on Windows; permissions require ACL manipulation
    (void)file;
    (void)bitsToRemove;
    return true;
}

#else
inline bool removePermissionBits(const std::string& file, mode_t bitsToRemove)
{
    struct stat fileStat {};
    if (stat(file.c_str(), &fileStat) != 0) {
        std::cerr << "removePermissionBits: stat failed for file: " << file << "\n";
        return false;
    }

    const mode_t newMode = fileStat.st_mode & ~bitsToRemove;
    if (chmod(file.c_str(), newMode) != 0) {
        std::cerr << "removePermissionBits: chmod failed for file: " << file << "\n";
        return false;
    }

    return true;
}

#endif

inline bool removeReadPermission(const std::string& file)
{
#if defined(_WIN32)
    // Windows doesn't have a direct equivalent to Unix read permissions
    // Using FILE_ATTRIBUTE_HIDDEN as a workaround for testing purposes
    return SetFileAttributesA(file.c_str(), FILE_ATTRIBUTE_HIDDEN) != 0;
#else
    return removePermissionBits(file, S_IRUSR);
#endif
}

inline bool removeWritePermission(const std::string& file)
{
#if defined(_WIN32)
    DWORD attrs = GetFileAttributesA(file.c_str());
    if (attrs == INVALID_FILE_ATTRIBUTES) {
        std::cerr << "removeWritePermission: GetFileAttributesA failed for: " << file << "\n";
        return false;
    }

    // Add the read-only attribute
    if (!SetFileAttributesA(file.c_str(), attrs | FILE_ATTRIBUTE_READONLY)) {
        std::cerr << "removeWritePermission: SetFileAttributesA failed for: " << file << "\n";
        return false;
    }

    return true;
#else
    return removePermissionBits(file, S_IWUSR);
#endif
}

inline bool fileExists(const std::string& path)
{
#if defined(_WIN32)
    return GetFileAttributesA(path.c_str()) != INVALID_FILE_ATTRIBUTES;
#else
    return access(path.c_str(), F_OK) == 0;
#endif
}

inline bool removeIfExists(const std::string& path)
{
    if (!fileExists(path)) {
        return true; // File does not exist, nothing to remove
    }
    if (std::remove(path.c_str()) != 0) {
        std::cerr << "removeIfExists: failed to remove file: '" << path << "'\n";
        return false;
    }

    return true;
}

inline bool prepareDestinationFile(const std::string& from, const std::string& to)
{
    return (from == to) || removeIfExists(to);
}

inline bool copyFileAndRestrictRead(const std::string& from, const std::string& to)
{
    return prepareDestinationFile(from, to)
           && copyFile(from, to)
           && removeReadPermission(to);
}

inline bool copyFileAndRestrictWrite(const std::string& from, const std::string& to)
{
    return prepareDestinationFile(from, to)
           && copyFile(from, to)
           && removeWritePermission(to);
}
} // namespace testtools
