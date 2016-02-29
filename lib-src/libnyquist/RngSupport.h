#ifndef RNGSUPPORT_H
#define RNGSUPPORT_H

#ifdef __cplusplus

#if __cplusplus >= 201103L || (defined _MSC_VER && _MSC_VER >= 1600)
#define NYQ_USE_RANDOM_HEADER
#endif

#if defined NYQ_USE_RANDOM_HEADER

#include <vector>
#include <random>

namespace Nyq
{
typedef std::mt19937 nyq_generator;
const int nyq_generator_state_size = nyq_generator::state_size;
typedef std::seed_seq nyq_seed_seq;
typedef std::uniform_real_distribution<float> nyq_uniform_float_distribution;
typedef std::normal_distribution<float> nyq_normal_float_distribution;
typedef std::uniform_real_distribution<double> nyq_uniform_double_distribution;
typedef std::normal_distribution<double> nyq_normal_double_distribution;
typedef std::uniform_int_distribution<int> nyq_uniform_int_distribution;

class RngSupportBase
{
protected:
    static std::vector<unsigned int> CreateRootSeedVector();
};

template <class RNG = nyq_generator>
class RngSupport : RngSupportBase
{
private:
    friend std::vector<unsigned int> CreateRootSeedVector();

    static RNG CreateRootGenerator()
    {
        auto seed_data = CreateRootSeedVector();

        std::seed_seq seed(begin(seed_data), end(seed_data));

        RNG generator(seed);

        return generator;
    }

    static RNG& GetRootGenerator()
    {
        static thread_local auto generator = CreateRootGenerator();

        return generator;
    }

public:
    template<class RealFwdIt, class Real>
    static void RandomFillUniform(RealFwdIt first, RealFwdIt last, Real low, Real high)
    {
        RNG& generator = GetRootGenerator();

        std::uniform_real_distribution<Real> uniform{ low, high };

        for (; first != last; ++first)
            *first = uniform(generator);
    }

    template<class RealFwdIt, class Real>
    static void RandomFillNormal(RealFwdIt first, RealFwdIt last, Real mean, Real stDev)
    {
        RNG& generator = GetRootGenerator();

        std::normal_distribution<Real> uniform{ mean, stDev };

        for (; first != last; ++first)
            *first = uniform(generator);
    }

    template<class RealFwdIt, class Real>
    static bool RandomFillClampedNormal(RealFwdIt first, RealFwdIt last, Real mean, Real stDev, Real low, Real high)
    {
        RNG& generator = GetRootGenerator();

        std::normal_distribution<Real> uniform{ mean, stDev };

        for (; first != last; ++first)
        {
            int retry = 10;

            for (;;)
            {
                Real x = uniform(generator);

                if (x <= high && x >= low)
                {
                    *first = x;
                    break;
                }

                if (--retry <= 0)
                    return false;
            }
        }

        return true;
    }

    template<class Real>
    static Real RandomUniformReal(Real low, Real high)
    {
        auto& generator = GetRootGenerator();

        std::uniform_real_distribution<Real> uniform{ low, high };

        return uniform(generator);
    }

    template<class Int>
    static Int RandomUniformInt(Int low, Int high)
    {
        auto& generator = GetRootGenerator();

        std::uniform_int_distribution<Int> uniform{ low, high };

        return uniform(generator);
    }

    static std::vector<unsigned int> CreateSeedVector(std::vector<unsigned int>::size_type size)
    {
        std::vector<unsigned int> seed;

        seed.reserve(size);

        auto rng = GetRootGenerator();

        for (; size > 0; --size)
            seed.push_back(rng());

        return seed;
    }

}; // class RngSupport

template <class RNG = nyq_generator>
static RNG CreateGenerator(int size = 32)
{
    auto seed_data = RngSupport<RNG>::CreateSeedVector(size);

    nyq_seed_seq seq(begin(seed_data), end(seed_data));

    RNG generator(seq);

    return generator;
}

template <class RNG = nyq_generator>
static void ReseedGenerator(RNG& generator, int size = 32)
{
    auto seed_data = RngSupport<RNG>::CreateSeedVector(size);

    nyq_seed_seq seq(begin(seed_data), end(seed_data));

    generator.seed(seq);
}

template <class RNG = nyq_generator>
class NyqEngine : public RNG
{
public:
    explicit NyqEngine(int size = 32) : RNG(CreateGenerator(size))
    { }
};

} // namespace Nyq

#endif // NYQ_USE_RANDOM_HEADER

extern "C" {
#endif // __cplusplus

    void RandomFillUniformFloat(float* p, size_t count, float low, float high);
    void RandomFillNormalFloat(float* p, size_t count, float mean, float stDev);
    int RandomFillClampedNormalFloat(float* p, size_t count, float mean, float stDev, float low, float high);
    float RandomUniformFloat(float low, float high);

    void RandomFillUniformDouble(double* p, size_t count, double low, double high);
    void RandomFillNormalDouble(double* p, size_t count, double mean, double stDev);
    int RandomFillClampedNormalDouble(double* p, size_t count, double mean, double stDev, double low, double high);
    double RandomUniformDouble(double low, double high);

    int RandomUniformInt(int lowInclusive, int highExclusive);

#ifdef __cplusplus
} //   extern "C"
#endif

#endif // RNGSUPPORT_H
