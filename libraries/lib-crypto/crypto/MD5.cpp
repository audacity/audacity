/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: MD5.cpp
 *
 * Based on a public domain implementation of the MD5 algorithm.
 */

#include "MD5.h"

#include <algorithm>
#include <cassert>
#include <cstring>

namespace crypto {
namespace {
constexpr uint32_t S[] = {
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
};

constexpr uint32_t K[] = {
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
    0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
    0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
    0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
    0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
    0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
};

inline uint32_t F(uint32_t x, uint32_t y, uint32_t z)
{
    return (x & y) | (~x & z);
}

inline uint32_t G(uint32_t x, uint32_t y, uint32_t z)
{
    return (x & z) | (y & ~z);
}

inline uint32_t H(uint32_t x, uint32_t y, uint32_t z)
{
    return x ^ y ^ z;
}

inline uint32_t I(uint32_t x, uint32_t y, uint32_t z)
{
    return y ^ (x | ~z);
}

inline uint32_t LeftRotate(uint32_t value, uint32_t bits)
{
    return (value << bits) | (value >> (32 - bits));
}

} // namespace

MD5::MD5()
{
    Reset();
}

void MD5::Transform(const uint8_t block[BLOCK_SIZE])
{
    uint32_t m[16];

    for (uint32_t i = 0, j = 0; i < 16; ++i, j += 4) {
        m[i] = static_cast<uint32_t>(block[j])
               | (static_cast<uint32_t>(block[j + 1]) << 8)
               | (static_cast<uint32_t>(block[j + 2]) << 16)
               | (static_cast<uint32_t>(block[j + 3]) << 24);
    }

    uint32_t a = mState[0];
    uint32_t b = mState[1];
    uint32_t c = mState[2];
    uint32_t d = mState[3];

    for (uint32_t i = 0; i < 64; ++i) {
        uint32_t f;
        uint32_t g;

        if (i < 16) {
            f = F(b, c, d);
            g = i;
        } else if (i < 32) {
            f = G(b, c, d);
            g = (5 * i + 1) & 0x0F;
        } else if (i < 48) {
            f = H(b, c, d);
            g = (3 * i + 5) & 0x0F;
        } else {
            f = I(b, c, d);
            g = (7 * i) & 0x0F;
        }

        const uint32_t temp = d;
        d = c;
        c = b;
        b = b + LeftRotate(a + f + K[i] + m[g], S[i]);
        a = temp;
    }

    mState[0] += a;
    mState[1] += b;
    mState[2] += c;
    mState[3] += d;
}

void MD5::Update(const void* data, std::size_t size)
{
    const auto* dataPtr = static_cast<const uint8_t*>(data);

    while (size > 0) {
        const std::size_t blockSize
            =std::min<std::size_t>(size, BLOCK_SIZE - mBufferLength);

        std::memcpy(mBuffer + mBufferLength, dataPtr, blockSize);

        mBufferLength += blockSize;
        dataPtr += blockSize;
        size -= blockSize;

        if (mBufferLength == BLOCK_SIZE) {
            Transform(mBuffer);
            mBitLength += BLOCK_SIZE * 8;
            mBufferLength = 0;
        }
    }
}

void MD5::Update(const char* zString)
{
    Update(zString, std::strlen(zString));
}

std::array<uint8_t, MD5::HASH_SIZE> MD5::Finalize()
{
    assert(mBufferLength < BLOCK_SIZE);

    mBitLength += mBufferLength * 8;

    mBuffer[mBufferLength++] = 0x80;

    if (mBufferLength > 56) {
        std::memset(mBuffer + mBufferLength, 0, BLOCK_SIZE - mBufferLength);
        Transform(mBuffer);
        mBufferLength = 0;
    }

    std::memset(mBuffer + mBufferLength, 0, 56 - mBufferLength);

    const uint64_t bitLength = mBitLength;
    for (uint32_t i = 0; i < 8; ++i) {
        mBuffer[56 + i] = static_cast<uint8_t>((bitLength >> (i * 8)) & 0xFF);
    }

    Transform(mBuffer);

    std::array<uint8_t, HASH_SIZE> result {};

    for (uint32_t i = 0; i < 4; ++i) {
        result[i * 4 + 0] = static_cast<uint8_t>((mState[i] >> 0) & 0xFF);
        result[i * 4 + 1] = static_cast<uint8_t>((mState[i] >> 8) & 0xFF);
        result[i * 4 + 2] = static_cast<uint8_t>((mState[i] >> 16) & 0xFF);
        result[i * 4 + 3] = static_cast<uint8_t>((mState[i] >> 24) & 0xFF);
    }

    Reset();

    return result;
}

std::string MD5::FinalizeHex()
{
    const auto digest = Finalize();

    constexpr char hexChars[] = "0123456789abcdef";
    std::string result;
    result.resize(HASH_SIZE * 2);

    for (std::size_t i = 0; i < HASH_SIZE; ++i) {
        result[i * 2 + 0] = hexChars[(digest[i] >> 4) & 0x0F];
        result[i * 2 + 1] = hexChars[digest[i] & 0x0F];
    }

    return result;
}

void MD5::Reset()
{
    mBitLength = 0;

    mState[0] = 0x67452301;
    mState[1] = 0xefcdab89;
    mState[2] = 0x98badcfe;
    mState[3] = 0x10325476;

    std::memset(mBuffer, 0, sizeof(mBuffer));
    mBufferLength = 0;
}

std::string md5Hex(const void* data, std::size_t size)
{
    MD5 hasher;
    hasher.Update(data, size);
    return hasher.FinalizeHex();
}
} // namespace crypto