/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

#include "maths/MathUtilities.h"

#include <cmath>
#include <iostream>

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN

#include <boost/test/unit_test.hpp>

using namespace std;

BOOST_AUTO_TEST_SUITE(TestMathUtilities)

BOOST_AUTO_TEST_CASE(round)
{
    BOOST_CHECK_EQUAL(MathUtilities::round(0.5), 1.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(0.49), 0.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(0.99), 1.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(0.01), 0.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(0.0), 0.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(100.0), 100.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(-0.2), 0.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(-0.5), -1.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(-0.99), -1.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(-1.0), -1.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(-1.1), -1.0);
    BOOST_CHECK_EQUAL(MathUtilities::round(-1.5), -2.0);
}

BOOST_AUTO_TEST_CASE(mean)
{
    BOOST_CHECK_EQUAL(MathUtilities::mean(0, 0), 0);
    double d0[] = { 0, 4, 3, -1 };
    BOOST_CHECK_EQUAL(MathUtilities::mean(d0, 4), 1.5);
    double d1[] = { -2.6 };
    BOOST_CHECK_EQUAL(MathUtilities::mean(d1, 1), -2.6);
    vector<double> v;
    v.push_back(0);
    v.push_back(4);
    v.push_back(3);
    v.push_back(-1);
    BOOST_CHECK_EQUAL(MathUtilities::mean(v, 0, 4), 1.5);
    BOOST_CHECK_EQUAL(MathUtilities::mean(v, 1, 2), 3.5);
    BOOST_CHECK_EQUAL(MathUtilities::mean(v, 3, 1), -1);
    BOOST_CHECK_EQUAL(MathUtilities::mean(v, 3, 0), 0);
}

BOOST_AUTO_TEST_CASE(sum)
{
    BOOST_CHECK_EQUAL(MathUtilities::sum(0, 0), 0);
    double d0[] = { 0, 4, 3, -1 };
    BOOST_CHECK_EQUAL(MathUtilities::sum(d0, 4), 6);
    double d1[] = { -2.6 };
    BOOST_CHECK_EQUAL(MathUtilities::sum(d1, 1), -2.6);
}

BOOST_AUTO_TEST_CASE(median)
{
    BOOST_CHECK_EQUAL(MathUtilities::median(0, 0), 0);
    double d0[] = { 0, 4, 3, -1 };
    BOOST_CHECK_EQUAL(MathUtilities::median(d0, 4), 1.5);
    double d1[] = { 0, 4, 3, -1, -1 };
    BOOST_CHECK_EQUAL(MathUtilities::median(d1, 5), 0);
    double d2[] = { 1.0, -2.0 };
    BOOST_CHECK_EQUAL(MathUtilities::median(d2, 2), -0.5);
    double d3[] = { -2.6 };
    BOOST_CHECK_EQUAL(MathUtilities::median(d3, 1), -2.6);
}

BOOST_AUTO_TEST_CASE(princarg)
{
    BOOST_CHECK_EQUAL(MathUtilities::princarg(M_PI), M_PI);
    BOOST_CHECK_EQUAL(MathUtilities::princarg(-M_PI), M_PI);
    BOOST_CHECK_EQUAL(MathUtilities::princarg(2 * M_PI), 0.0);
    BOOST_CHECK_EQUAL(MathUtilities::princarg(5 * M_PI), M_PI);
    BOOST_CHECK_EQUAL(MathUtilities::princarg(1.0), 1.0);
    BOOST_CHECK_EQUAL(MathUtilities::princarg(-1.0), -1.0);
    BOOST_CHECK_EQUAL(MathUtilities::princarg(-10.0), -10.0 + 4 * M_PI);
}

BOOST_AUTO_TEST_CASE(isPowerOfTwo)
{
    BOOST_CHECK_EQUAL(MathUtilities::isPowerOfTwo(0), false);
    BOOST_CHECK_EQUAL(MathUtilities::isPowerOfTwo(1), true);
    BOOST_CHECK_EQUAL(MathUtilities::isPowerOfTwo(-2), false);
    BOOST_CHECK_EQUAL(MathUtilities::isPowerOfTwo(2), true);
    BOOST_CHECK_EQUAL(MathUtilities::isPowerOfTwo(3), false);
    BOOST_CHECK_EQUAL(MathUtilities::isPowerOfTwo(12), false);
    BOOST_CHECK_EQUAL(MathUtilities::isPowerOfTwo(16), true);
}

BOOST_AUTO_TEST_CASE(nextPowerOfTwo)
{
    BOOST_CHECK_EQUAL(MathUtilities::nextPowerOfTwo(0), 1);
    BOOST_CHECK_EQUAL(MathUtilities::nextPowerOfTwo(1), 1);
    BOOST_CHECK_EQUAL(MathUtilities::nextPowerOfTwo(-2), 1);
    BOOST_CHECK_EQUAL(MathUtilities::nextPowerOfTwo(2), 2);
    BOOST_CHECK_EQUAL(MathUtilities::nextPowerOfTwo(3), 4);
    BOOST_CHECK_EQUAL(MathUtilities::nextPowerOfTwo(12), 16);
    BOOST_CHECK_EQUAL(MathUtilities::nextPowerOfTwo(16), 16);
}

BOOST_AUTO_TEST_CASE(previousPowerOfTwo)
{
    BOOST_CHECK_EQUAL(MathUtilities::previousPowerOfTwo(0), 1);
    BOOST_CHECK_EQUAL(MathUtilities::previousPowerOfTwo(1), 1);
    BOOST_CHECK_EQUAL(MathUtilities::previousPowerOfTwo(-2), 1);
    BOOST_CHECK_EQUAL(MathUtilities::previousPowerOfTwo(2), 2);
    BOOST_CHECK_EQUAL(MathUtilities::previousPowerOfTwo(3), 2);
    BOOST_CHECK_EQUAL(MathUtilities::previousPowerOfTwo(12), 8);
    BOOST_CHECK_EQUAL(MathUtilities::previousPowerOfTwo(16), 16);
}

BOOST_AUTO_TEST_CASE(nearestPowerOfTwo)
{
    BOOST_CHECK_EQUAL(MathUtilities::nearestPowerOfTwo(0), 1);
    BOOST_CHECK_EQUAL(MathUtilities::nearestPowerOfTwo(1), 1);
    BOOST_CHECK_EQUAL(MathUtilities::nearestPowerOfTwo(-2), 1);
    BOOST_CHECK_EQUAL(MathUtilities::nearestPowerOfTwo(2), 2);
    BOOST_CHECK_EQUAL(MathUtilities::nearestPowerOfTwo(3), 4);
    BOOST_CHECK_EQUAL(MathUtilities::nearestPowerOfTwo(11), 8);
    BOOST_CHECK_EQUAL(MathUtilities::nearestPowerOfTwo(12), 16);
    BOOST_CHECK_EQUAL(MathUtilities::nearestPowerOfTwo(16), 16);
}

BOOST_AUTO_TEST_CASE(factorial)
{
    BOOST_CHECK_EQUAL(MathUtilities::factorial(-10), 0.0);
    BOOST_CHECK_EQUAL(MathUtilities::factorial(0), 1.0);
    BOOST_CHECK_EQUAL(MathUtilities::factorial(1), 1.0);
    BOOST_CHECK_EQUAL(MathUtilities::factorial(2), 2.0);
    BOOST_CHECK_EQUAL(MathUtilities::factorial(3), 6.0);
    BOOST_CHECK_EQUAL(MathUtilities::factorial(4), 24.0);

    // Too big for an int, hence double return value from factorial
    BOOST_CHECK_EQUAL(MathUtilities::factorial(20), 2432902008176640000.0);
}

BOOST_AUTO_TEST_CASE(gcd)
{
    BOOST_CHECK_EQUAL(MathUtilities::gcd(1, 1), 1);
    BOOST_CHECK_EQUAL(MathUtilities::gcd(2, 1), 1);
    BOOST_CHECK_EQUAL(MathUtilities::gcd(2, 3), 1);
    BOOST_CHECK_EQUAL(MathUtilities::gcd(4, 2), 2);
    BOOST_CHECK_EQUAL(MathUtilities::gcd(18, 24), 6);
    BOOST_CHECK_EQUAL(MathUtilities::gcd(27, 18), 9);
    BOOST_CHECK_EQUAL(MathUtilities::gcd(18, 36), 18);
    BOOST_CHECK_EQUAL(MathUtilities::gcd(37, 18), 1);
}

BOOST_AUTO_TEST_CASE(getAlphaNorm1)
{
    vector<double> in { -1, 1.5, 3, 5 };
    double out = MathUtilities::getAlphaNorm(in, 1);
    double expected = 2.6250;
    double thresh = 1e-4;
    BOOST_CHECK_SMALL(out - expected, thresh);
}

BOOST_AUTO_TEST_CASE(getAlphaNorm2)
{
    vector<double> in { -1, 1.5, 3, 5 };
    double out = MathUtilities::getAlphaNorm(in, 2);
    double expected = 3.0516;
    double thresh = 1e-4;
    BOOST_CHECK_SMALL(out - expected, thresh);
}

BOOST_AUTO_TEST_CASE(getL1Norm)
{
    vector<double> in { -1, 1.5, 3, 5 };
    double out = MathUtilities::getLpNorm(in, 1);
    // L1 norm is the sum of magnitudes
    double expected = 10.5;
    double thresh = 1e-5;
    BOOST_CHECK_SMALL(out - expected, thresh);
}

BOOST_AUTO_TEST_CASE(getL2Norm)
{
    vector<double> in { -1, 1.5, 3, 5 };
    double out = MathUtilities::getLpNorm(in, 2);
    // L2 norm is the sqrt of sum of squared magnitudes
    double expected = sqrt(37.25);
    double thresh = 1e-5;
    BOOST_CHECK_SMALL(out - expected, thresh);
}

BOOST_AUTO_TEST_CASE(getL3Norm)
{
    vector<double> in { -1, 1.5, 3, 5 };
    double out = MathUtilities::getLpNorm(in, 3);
    double expected = 5.3875;
    double thresh = 1e-4;
    BOOST_CHECK_SMALL(out - expected, thresh);
}

BOOST_AUTO_TEST_CASE(normaliseL1)
{
    vector<double> in { -1, 1.5, 3, 5 };
    vector<double> expected { -0.095238, 0.142857, 0.285714, 0.476190 };
    vector<double> out = MathUtilities::normaliseLp(in, 1);
    double thresh = 1e-5;
    for (int i = 0; i < int(out.size()); ++i) {
        BOOST_CHECK_SMALL(out[i] - expected[i], thresh);
    }
    out = MathUtilities::normaliseLp({ 0, 0, 0, 0 }, 1);
    for (int i = 0; i < int(out.size()); ++i) {
        BOOST_CHECK_EQUAL(out[i], 0.25);
    }
}

BOOST_AUTO_TEST_CASE(normaliseL2)
{
    vector<double> in { -1, 1.5, 3, 5 };
    vector<double> expected { -0.16385, 0.24577, 0.49154, 0.81923 };
    vector<double> out = MathUtilities::normaliseLp(in, 2);
    double thresh = 1e-5;
    for (int i = 0; i < int(out.size()); ++i) {
        BOOST_CHECK_SMALL(out[i] - expected[i], thresh);
    }
    out = MathUtilities::normaliseLp({ 0, 0, 0, 0 }, 2);
    for (int i = 0; i < int(out.size()); ++i) {
        BOOST_CHECK_EQUAL(out[i], 0.5);
    }
}

BOOST_AUTO_TEST_CASE(normaliseL3)
{
    vector<double> in { -1, 1.5, 3, 5 };
    vector<double> expected { -0.18561, 0.27842, 0.55684, 0.92807 };
    vector<double> out = MathUtilities::normaliseLp(in, 3);
    double thresh = 1e-5;
    for (int i = 0; i < int(out.size()); ++i) {
        BOOST_CHECK_SMALL(out[i] - expected[i], thresh);
    }
    out = MathUtilities::normaliseLp({ 0, 0, 0, 0 }, 3);
    for (int i = 0; i < int(out.size()); ++i) {
        BOOST_CHECK_SMALL(out[i] - 0.62996, 1e-5);
    }
}

BOOST_AUTO_TEST_SUITE_END()


