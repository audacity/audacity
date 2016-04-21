#include "RngSupport.h"

#if defined NYQ_USE_RANDOM_HEADER

#include <algorithm>
#include <atomic>
#include <chrono>
#include <random>

//#define USE_RDRAND

#ifdef USE_RDRAND
#include <immintrin.h>
#endif

using namespace std;

using namespace Nyq;

namespace {
#ifdef USE_RDRAND
bool HasRdRandImpl()
{
    int output[4];

    __cpuid(output, 0);

    auto isIntel = 0 == memcmp("Genu" "ntel" "ineI", &output[1], 12);

    if (isIntel)
    {
        __cpuid(output, 1);

        const int IntelRdRandFlag = 1 << 30;

        return 0 != (output[2] & IntelRdRandFlag);
    }

    auto isAmd = 0 == memcmp("Auth" "cAMD" "enti", &output[1], 12);

    if (isAmd)
    {
        const int AmdRdRandFlag = 1 << 30;

        __cpuid(output, 0x80000005);

        return 0 != (output[2] & AmdRdRandFlag);
    }

    return false;
}

bool HasRdRand()
{
    static const bool hasRdRand = HasRdRandImpl();

    return hasRdRand;
}

template<class Container>
bool PushBackFromRdRand(Container& container, int count)
{
    if (!HasRdRand())
        return false;

    if (count < 1)
        return true;

    auto required = container.size() + count;

    if (required > container.capacity())
        container.reserve(required);

    while (count--)
    {
        int retry = 10;

        for (; ; --retry)
        {
            if (retry <= 0)
                return false;

            unsigned int n;

            if (_rdrand32_step(&n))
            {
                container.push_back(n);

                break;
            }
        }
    }

    return true;
}

#endif
} // unnamed namespace


namespace Nyq
{
vector<unsigned int> RngSupportBase::CreateRootSeedVector()
{
    random_device rd;

    vector<unsigned int> seed_data(nyq_generator_state_size);

    generate(begin(seed_data), end(seed_data), ref(rd));

    // Protect against a broken random_device
    auto timestamp = chrono::high_resolution_clock::now().time_since_epoch().count();

    seed_data.push_back(static_cast<unsigned int>(timestamp));
    seed_data.push_back(static_cast<unsigned int>(timestamp >> 32));

    static atomic<int> counter;

    auto x = ++counter;

    seed_data.push_back(x);

#ifdef USE_RDRAND
    PushBackFromRdRand(seed_data, nyq_generator_state_size);
#endif

    return seed_data;
}
} // namespace Nyq


extern "C"
void RandomFillUniformFloat(float* p, size_t count, float low, float high)
{
    RngSupport<>::RandomFillUniform(p, p + count, low, high);
}

extern "C"
void RandomFillNormalFloat(float* p, size_t count, float mean, float stDev)
{
    RngSupport<>::RandomFillNormal(p, p + count, mean, stDev);
}

extern "C"
int RandomFillClampedNormalFloat(float* p, size_t count, float mean, float stDev, float low, float high)
{
    return RngSupport<>::RandomFillClampedNormal(p, p + count, mean, stDev, low, high);
}

extern "C"
float RandomUniformFloat(float low, float high)
{
    return RngSupport<>::RandomUniformReal(low, high);
}

extern "C"
void RandomFillUniformDouble(double* p, size_t count, double low, double high)
{
    RngSupport<>::RandomFillUniform(p, p + count, low, high);
}

extern "C"
void RandomFillNormalDouble(double* p, size_t count, double mean, double stDev)
{
    RngSupport<>::RandomFillNormal(p, p + count, mean, stDev);
}

extern "C"
int RandomFillClampedNormalDouble(double* p, size_t count, double mean, double stDev, double low, double high)
{
    return RngSupport<>::RandomFillClampedNormal(p, p + count, mean, stDev, low, high);
}

extern "C"
double RandomUniformDouble(double low, double high)
{
    return RngSupport<>::RandomUniformReal(low, high);
}

extern "C"
int RandomUniformInt(int lowInclusive, int highExclusive)
{
    return RngSupport<>::RandomUniformInt(lowInclusive, highExclusive);
}

#else // NYQ_USE_RANDOM_HEADER

#include <stdlib.h>
#include <math.h>

static const int NYQ_RAND_MAX = RAND_MAX;
static int NyqRand()
{
    return rand();
}

const float fRandScale = 1.f / NYQ_RAND_MAX;
const float f2RandScale = 2.f / NYQ_RAND_MAX;

static inline void TwoNormalFloats(float& a, float& b)
{
    float u, v, d2;

    do {
        u = f2RandScale * NyqRand() - 1;
        v = f2RandScale * NyqRand() - 1;
        d2 = u * u + v * v;
    } while (d2 >= 1 || d2 == 0);

    float scale = static_cast<float>(sqrt(-2. * log(d2) / d2));

    a = v * scale;
    b = u * scale;
}

extern "C"
void RandomFillUniformFloat(float* p, size_t count, float low, float high)
{
    if (count < 1)
        return;

    const float scale = fRandScale * (high - low);

    while (count--)
        *p++ = low + scale * NyqRand();
}

extern "C"
void RandomFillNormalFloat(float* p, size_t count, float mean, float stDev)
{
    if (count < 1)
        return;

    for (; count >= 2; count -= 2)
    {
        float a, b;

        TwoNormalFloats(a, b);

        *p++ = mean + stDev * a;
        *p++ = mean + stDev * b;
    }

    if (count > 1)
    {
        float a, b;

        TwoNormalFloats(a, b);

        *p++ = mean + stDev * a;
    }
}

extern "C"
int RandomFillClampedNormalFloat(float* p, size_t count, float mean, float stDev, float low, float high)
{
    if (count < 1)
        return 1;

    int reject = 0;
    const int max_reject = 5;

    for (int i = 0; i < count; )
    {
        float a, b;

        TwoNormalFloats(a, b);

        a = mean + stDev * a;

        if (a >= low && a <= high)
        {
            ++i;
            *p++ = a;
            reject = 0;
        }
        else
        {
            if (++reject >= max_reject)
                return 0;
        }

        if (i < count)
        {
            b = mean + stDev * b;

            if (b >= low && b <= high)
            {
                ++i;
                *p++ = b;
                reject = 0;
            }
            else
            {
                if (++reject >= max_reject)
                    return 0;
            }
        }
    }

    if (count > 1)
    {
        float a, b;

        TwoNormalFloats(a, b);

        *p++ = mean + stDev * a;
    }

    return 1;
}

extern "C"
float RandomUniformFloat(float low, float high)
{
    return low + (high - low) * fRandScale * NyqRand();
}

extern "C"
int RandomUniformInt(int lowInclusive, int highExclusive)
{
    const int range = highExclusive - lowInclusive;

    if (range < 1 || range > NYQ_RAND_MAX)
        return lowInclusive;

    const int whole = NYQ_RAND_MAX / range;
    const int max_uniform = whole * range;

    if (max_uniform < 1)
        return lowInclusive;

    for (;;)
    {
        int r = NyqRand();

        if (r <= max_uniform)
            return r % range + lowInclusive;
    }
}

const double dRandScale = 1. / NYQ_RAND_MAX;
const double d2RandScale = 2. / NYQ_RAND_MAX;

static inline void TwoNormalDoubles(double& a, double& b)
{
    double u, v, d2;

    do {
        u = d2RandScale * NyqRand() - 1;
        v = d2RandScale * NyqRand() - 1;
        d2 = u * u + v * v;
    } while (d2 >= 1 || d2 == 0);

    double scale = sqrt(-2. * log(d2) / d2);

    a = v * scale;
    b = u * scale;
}
extern "C"
void RandomFillUniformDouble(double* p, size_t count, double low, double high)
{
    if (count < 1)
        return;

    const double scale = dRandScale * (high - low);

    while (count--)
        *p++ = low + scale * NyqRand();
}

extern "C"
void RandomFillNormalDouble(double* p, size_t count, double mean, double stDev)
{
    if (count < 1)
        return;

    for (; count >= 2; count -= 2)
    {
        double a, b;

        TwoNormalDoubles(a, b);

        *p++ = mean + stDev * a;
        *p++ = mean + stDev * b;
    }

    if (count > 1)
    {
        double a, b;

        TwoNormalDoubles(a, b);

        *p++ = mean + stDev * a;
    }
}

extern "C"
int RandomFillClampedNormalDouble(double* p, size_t count, double mean, double stDev, double low, double high)
{
    if (count < 1)
        return 1;

    int reject = 0;
    const int max_reject = 5;

    for (int i = 0; i < count; )
    {
        double a, b;

        TwoNormalDoubles(a, b);

        a = mean + stDev * a;

        if (a >= low && a <= high)
        {
            ++i;
            *p++ = a;
            reject = 0;
        }
        else
        {
            if (++reject >= max_reject)
                return 0;
        }

        if (i < count)
        {
            b = mean + stDev * b;

            if (b >= low && b <= high)
            {
                ++i;
                *p++ = b;
                reject = 0;
            }
            else
            {
                if (++reject >= max_reject)
                    return 0;
            }
        }
    }

    if (count > 1)
    {
        double a, b;

        TwoNormalDoubles(a, b);

        *p++ = mean + stDev * a;
    }

    return 1;
}

extern "C"
double RandomUniformDouble(double low, double high)
{
    return low + (high - low) * dRandScale * NyqRand();
}

#endif // NYQ_USE_RANDOM_HEADER
