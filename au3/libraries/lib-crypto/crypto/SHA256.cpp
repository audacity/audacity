/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: SHA256.h
 * SPDX-FileContributor: Dmitry Vedenko
 *
 * Based on a public domain code by Brad Conte.
 */

#include "SHA256.h"

#include <cassert>
#include <cstring>

namespace crypto {
namespace {
constexpr uint32_t K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
    0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
    0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

#define ROTLEFT(a, b) (((a) << (b)) | ((a) >> (32 - (b))))
#define ROTRIGHT(a, b) (((a) >> (b)) | ((a) << (32 - (b))))

#define CH(x, y, z) (((x) & (y)) ^ (~(x) & (z)))
#define MAJ(x, y, z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define EP0(x) (ROTRIGHT(x, 2) ^ ROTRIGHT(x, 13) ^ ROTRIGHT(x, 22))
#define EP1(x) (ROTRIGHT(x, 6) ^ ROTRIGHT(x, 11) ^ ROTRIGHT(x, 25))
#define SIG0(x) (ROTRIGHT(x, 7) ^ ROTRIGHT(x, 18) ^ ((x) >> 3))
#define SIG1(x) (ROTRIGHT(x, 17) ^ ROTRIGHT(x, 19) ^ ((x) >> 10))

void sha256_transform(uint32_t state[8], const uint8_t data[64])
{
    uint32_t m[SHA256::BLOCK_SIZE];

    int i = 0;
    int j = 0;

    for (; i < 16; ++i, j += 4) {
        m[i] = (data[j] << 24) | (data[j + 1] << 16) | (data[j + 2] << 8)
               | (data[j + 3]);
    }

    for (; i < 64; ++i) {
        m[i] = SIG1(m[i - 2]) + m[i - 7] + SIG0(m[i - 15]) + m[i - 16];
    }

    uint32_t a = state[0];
    uint32_t b = state[1];
    uint32_t c = state[2];
    uint32_t d = state[3];
    uint32_t e = state[4];
    uint32_t f = state[5];
    uint32_t g = state[6];
    uint32_t h = state[7];

    for (i = 0; i < SHA256::BLOCK_SIZE; ++i) {
        const uint32_t t1 = h + EP1(e) + CH(e, f, g) + K[i] + m[i];
        const uint32_t t2 = EP0(a) + MAJ(a, b, c);

        h = g;
        g = f;
        f = e;
        e = d + t1;
        d = c;
        c = b;
        b = a;
        a = t1 + t2;
    }

    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
    state[4] += e;
    state[5] += f;
    state[6] += g;
    state[7] += h;
}
} // namespace

SHA256::SHA256()
{
    Reset();
}

void SHA256::Update(const void* data, std::size_t size)
{
    const uint8_t* dataPtr = static_cast<const uint8_t*>(data);

    while (size > 0)
    {
        std::size_t blockSize
            =std::min<size_t>(size, SHA256::BLOCK_SIZE - mBufferLength);

        std::memcpy(mBuffer + mBufferLength, dataPtr, blockSize);

        mBufferLength += blockSize;
        dataPtr += blockSize;
        size -= blockSize;

        if (mBufferLength == SHA256::BLOCK_SIZE) {
            sha256_transform(mState, mBuffer);
            mBitLength += 512;
            mBufferLength = 0;
        }
    }
}

void SHA256::Update(const char* zString)
{
    Update(zString, std::strlen(zString));
}

std::string SHA256::Finalize()
{
    uint8_t pad[SHA256::BLOCK_SIZE];
    std::size_t padLength;

    // `mBufferLength` is always less than SHA256::BLOCK_SIZE. See `Update`
    // method.
    assert(mBufferLength < SHA256::BLOCK_SIZE);

    mBitLength += mBufferLength * 8;

    if (mBufferLength < 56) {
        mBuffer[mBufferLength++] = 0x80;
        std::memset(mBuffer + mBufferLength, 0, 56 - mBufferLength);
    } else {
        mBuffer[mBufferLength++] = 0x80;
        std::memset(
            mBuffer + mBufferLength, 0, SHA256::BLOCK_SIZE - mBufferLength);
        sha256_transform(mState, mBuffer);
        std::memset(mBuffer, 0, 56);
    }

    mBuffer[56] = (mBitLength >> 56) & 0xff;
    mBuffer[57] = (mBitLength >> 48) & 0xff;
    mBuffer[58] = (mBitLength >> 40) & 0xff;
    mBuffer[59] = (mBitLength >> 32) & 0xff;
    mBuffer[60] = (mBitLength >> 24) & 0xff;
    mBuffer[61] = (mBitLength >> 16) & 0xff;
    mBuffer[62] = (mBitLength >> 8) & 0xff;
    mBuffer[63] = (mBitLength >> 0) & 0xff;

    sha256_transform(mState, mBuffer);

    uint8_t result[SHA256::HASH_SIZE];

    for (int i = 0; i < 8; ++i) {
        result[i * 4 + 0] = (mState[i] >> 24) & 0xff;
        result[i * 4 + 1] = (mState[i] >> 16) & 0xff;
        result[i * 4 + 2] = (mState[i] >> 8) & 0xff;
        result[i * 4 + 3] = (mState[i] >> 0) & 0xff;
    }

    Reset();

    // Convert to hex string
    constexpr char hexChars[] = "0123456789ABCDEF";
    std::string resultStr;
    resultStr.resize(HASH_SIZE * 2);

    for (int i = 0; i < SHA256::HASH_SIZE; ++i) {
        resultStr[i * 2 + 0] = hexChars[(result[i] >> 4) & 0xf];
        resultStr[i * 2 + 1] = hexChars[result[i] & 0xf];
    }

    return resultStr;
}

void SHA256::Reset()
{
    mBitLength = 0;

    mState[0] = 0x6a09e667;
    mState[1] = 0xbb67ae85;
    mState[2] = 0x3c6ef372;
    mState[3] = 0xa54ff53a;
    mState[4] = 0x510e527f;
    mState[5] = 0x9b05688c;
    mState[6] = 0x1f83d9ab;
    mState[7] = 0x5be0cd19;

    std::memset(mBuffer, 0, sizeof(mBuffer));
    mBufferLength = 0;
}
} // namespace crypto
