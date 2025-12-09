/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: SHA256.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <cstdint>
#include <cstddef>

#include <string>

namespace crypto {
class CRYPTO_API SHA256 final
{
public:
    static constexpr std::size_t HASH_SIZE = 32;
    static constexpr std::size_t BLOCK_SIZE = 64;

    SHA256();

    SHA256(const SHA256&) = delete;
    SHA256(SHA256&&) = delete;
    SHA256& operator=(const SHA256&) = delete;
    SHA256& operator=(SHA256&&) = delete;

    void Update(const void* data, std::size_t size);
    void Update(const char* zString);

    template<typename T>
    void Update(const T& data)
    {
        Update(data.data(), data.size());
    }

    std::string Finalize();

    void Reset();

private:
    uint64_t mBitLength;
    uint32_t mState[8];
    uint8_t mBuffer[BLOCK_SIZE];
    uint32_t mBufferLength;
}; // class SHA256

template<typename T>
std::string sha256(const T& data)
{
    SHA256 hasher;
    hasher.Update(data);
    return hasher.Finalize();
}
} // namespace crypto
