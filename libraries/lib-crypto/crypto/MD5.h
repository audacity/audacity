/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: MD5.h
 *
 * Based on a public domain implementation of the MD5 algorithm.
 */

#pragma once

#include <array>
#include <cstddef>
#include <cstdint>

#include <string>

namespace crypto {
class CRYPTO_API MD5 final
{
public:
    static constexpr std::size_t HASH_SIZE = 16;
    static constexpr std::size_t BLOCK_SIZE = 64;

    MD5();

    MD5(const MD5&) = delete;
    MD5(MD5&&) = delete;
    MD5& operator=(const MD5&) = delete;
    MD5& operator=(MD5&&) = delete;

    void Update(const void* data, std::size_t size);
    void Update(const char* zString);

    template<typename T>
    void Update(const T& data)
    {
        Update(data.data(), data.size());
    }

    std::array<uint8_t, HASH_SIZE> Finalize();
    std::string FinalizeHex();

    void Reset();

private:
    void Transform(const uint8_t block[BLOCK_SIZE]);

    uint64_t mBitLength;
    uint32_t mState[4];
    uint8_t mBuffer[BLOCK_SIZE];
    uint32_t mBufferLength;
}; // class MD5

std::string md5Hex(const void* data, std::size_t size);
} // namespace crypto