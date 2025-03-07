/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "log.h"
#include <fstream>

using namespace muse;

constexpr auto K = 32;
constexpr std::array<double, 2 * K + 1> win  {
    0.9403,
    0.9439,
    0.9474,
    0.9508,
    0.9541,
    0.9573,
    0.9604,
    0.9633,
    0.9662,
    0.9689,
    0.9716,
    0.9741,
    0.9765,
    0.9787,
    0.9809,
    0.9830,
    0.9849,
    0.9867,
    0.9884,
    0.9900,
    0.9915,
    0.9928,
    0.9941,
    0.9952,
    0.9962,
    0.9971,
    0.9979,
    0.9985,
    0.9991,
    0.9995,
    0.9998,
    0.9999,
    1.0000,
    0.9999,
    0.9998,
    0.9995,
    0.9991,
    0.9985,
    0.9979,
    0.9971,
    0.9962,
    0.9952,
    0.9941,
    0.9928,
    0.9915,
    0.9900,
    0.9884,
    0.9867,
    0.9849,
    0.9830,
    0.9809,
    0.9787,
    0.9765,
    0.9741,
    0.9716,
    0.9689,
    0.9662,
    0.9633,
    0.9604,
    0.9573,
    0.9541,
    0.9508,
    0.9474,
    0.9439,
    0.9403,
};

// Function to compute the sinc value at x (normalized sinc function)
double sinc(double x)
{
    if (x == 0.0) {
        return 1.0;
    }
    return sin(M_PI * x) / (M_PI * x);
}

auto  sinc_interpolate(const std::vector<double>& x, const std::vector<double>& tx, std::vector<double>& u)
{
    const double T = tx[1] - tx[0];
    const int Nx = static_cast<int>(x.size());
    const int Ny = Nx * K;

    std::vector<double> z(Ny);
    u.reserve(Ny);

    for (int nx = 0; nx < Nx; ++nx) {
        const auto ny0 = nx * K;
        for (auto ny = ny0; ny < ny0 + K; ++ny) {
            auto sum = 0.0;
            const auto k0 = std::max(nx - K, 0) - nx;
            const auto kEnd = std::min(nx + K, Nx) - nx;
            const auto ty = tx[0] + ny * T / K;
            u.push_back(ty);
            for (auto k = k0; k < kEnd; ++k) {
                const auto nx2 = nx + k;
                const auto _s = sinc((ty - tx[nx2]) / T);
                const auto _w = win[K + k];
                const auto _yi = x[nx2];
                sum += _yi * _w * _s;
            }
            z[ny] = sum;
        }
    }

    return z;
}

void generateSawtooth(std::vector<double>& t, std::vector<double>& x)
{
    constexpr auto numCycles = 2;
    const auto T = t.size() / numCycles;
    for (int i = 0; i < t.size(); ++i) {
        t[i] =            i / 10.;
        x[i] =  2.0 * (i % T) / T - 1.0;
    }
}

void generateSine(std::vector<double>& t, std::vector<double>& x)
{
    constexpr auto numCycles = 2;
    for (int i = 0; i < t.size(); ++i) {
        t[i] = i / 10.;
        x[i] = sin(2 * M_PI * i * numCycles / t.size());
    }
}

TEST(ProjectScene_SampleTests, Sample)
{
    // Generate 100 samples of a triangle wave of period 10
    std::vector<double> t(100);
    std::vector<double> x(100);
    // generateSawtooth(t, x);
    generateSine(t, x);
    std::vector<double> u;

    const auto y = sinc_interpolate(x, t, u);

    // Print to python-readable lists
    std::ofstream file("sinc_interpolate.py");
    file << "t = [";
    for (int i = 0; i < 100; ++i) {
        file << t[i] << ", ";
    }
    file << "]\n";
    file << "u = [";
    for (int i = 0; i < 1600; ++i) {
        file << u[i] << ", ";
    }
    file << "]\n";
    file << "x = [";
    for (int i = 0; i < 100; ++i) {
        file << x[i] << ", ";
    }
    file << "]\n";
    file << "y = [";
    for (int i = 0; i < 1600; ++i) {
        file << y[i] << ", ";
    }
    file << "]\n";
}
