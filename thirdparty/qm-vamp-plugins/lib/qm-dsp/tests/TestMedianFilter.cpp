/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

#include "maths/MedianFilter.h"

#include <cmath>
#include <cstdlib>

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(TestMedianFilter)

BOOST_AUTO_TEST_CASE(odd)
{
    // A median filter of size N always retains a pool of N elements,
    // which start out all-zero. So the output median will remain zero
    // until N/2 elements have been pushed.
    MedianFilter<double> f(3);
    f.push(1);                  // 0 0 1
    BOOST_CHECK_EQUAL(f.get(), 0);
    BOOST_CHECK_EQUAL(f.get(), 0);
    f.push(-3);                 // 0 1 -3
    BOOST_CHECK_EQUAL(f.get(), 0);
    f.push(5);                  // 1 -3 5
    BOOST_CHECK_EQUAL(f.get(), 1);
    f.push(7);                  // -3 5 7
    BOOST_CHECK_EQUAL(f.get(), 5);
    BOOST_CHECK_EQUAL(f.get(), 5);
    f.push(3);                  // 5 7 3
    BOOST_CHECK_EQUAL(f.get(), 5);
    f.push(3);                  // 7 3 3
    BOOST_CHECK_EQUAL(f.get(), 3);
}

BOOST_AUTO_TEST_CASE(even)
{
    // Our median does not halve the difference (should it??), it just
    // returns the next element from the input set
    MedianFilter<double> f(4);
    f.push(1);                  // 0 0 0 1
    BOOST_CHECK_EQUAL(f.get(), 0);
    BOOST_CHECK_EQUAL(f.get(), 0);
    f.push(-3);                 // 0 0 1 -3
    BOOST_CHECK_EQUAL(f.get(), 0);
    f.push(5);                  // 0 1 -3 5
    BOOST_CHECK_EQUAL(f.get(), 1);
    f.push(7);                  // 1 -3 5 7
    BOOST_CHECK_EQUAL(f.get(), 5);
    BOOST_CHECK_EQUAL(f.get(), 5);
    f.push(3);                  // -3 5 7 3
    BOOST_CHECK_EQUAL(f.get(), 5);
    f.push(3);                  // 5 7 3 3
    BOOST_CHECK_EQUAL(f.get(), 5);
}

BOOST_AUTO_TEST_CASE(odd75)
{
    MedianFilter<double> f(5, 75.f);
    f.push(1);                  // 0 0 0 0 1
    BOOST_CHECK_EQUAL(f.get(), 0);
    BOOST_CHECK_EQUAL(f.get(), 0);
    f.push(-3);                 // 0 0 0 1 -3
    BOOST_CHECK_EQUAL(f.get(), 0);
    f.push(5);                  // 0 0 1 -3 5
    BOOST_CHECK_EQUAL(f.get(), 1);
    f.push(7);                  // 0 1 -3 5 7
    BOOST_CHECK_EQUAL(f.get(), 5);
    BOOST_CHECK_EQUAL(f.get(), 5);
    f.push(3);                  // 0 -3 5 7 3
    BOOST_CHECK_EQUAL(f.get(), 5);
    f.push(3);                  // 0 5 7 3 3
    BOOST_CHECK_EQUAL(f.get(), 5);
}

BOOST_AUTO_TEST_CASE(even75)
{
    MedianFilter<double> f(4, 75.f);
    f.push(1);                  // 0 0 0 1
    BOOST_CHECK_EQUAL(f.get(), 1);
    BOOST_CHECK_EQUAL(f.get(), 1);
    f.push(-3);                 // 0 0 1 -3
    BOOST_CHECK_EQUAL(f.get(), 1);
    f.push(5);                  // 0 1 -3 5
    BOOST_CHECK_EQUAL(f.get(), 5);
    f.push(7);                  // 1 -3 5 7
    BOOST_CHECK_EQUAL(f.get(), 7);
    BOOST_CHECK_EQUAL(f.get(), 7);
    f.push(3);                  // -3 5 7 3
    BOOST_CHECK_EQUAL(f.get(), 7);
    f.push(3);                  // 5 7 3 3
    BOOST_CHECK_EQUAL(f.get(), 7);
}

BOOST_AUTO_TEST_CASE(oddStandalone)
{
    // The standalone (static filter() method) function is intended to
    // match e.g. MATLAB median filter, filtering a row at once
    // assuming the values beyond its edges are zeroes. This basically
    // just means it needs to compensate for the latency in the raw
    // filter:
    //
    // Raw filter
    //    1 3 5 7   ->   0 1 3 5    (N=3)
    // (because at each step only the values up to that step are
    // available)
    //
    // Standalone function:
    //    1 3 5 7   ->   1 3 5 5    (N=3)
    
    std::vector<double> in;
    in.push_back(1);
    in.push_back(3);
    in.push_back(5);
    in.push_back(7);

    std::vector<double> out = MedianFilter<double>::filter(3, in);
    BOOST_CHECK_EQUAL(out[0], 1);
    BOOST_CHECK_EQUAL(out[1], 3);
    BOOST_CHECK_EQUAL(out[2], 5);
    BOOST_CHECK_EQUAL(out[3], 5);
}

BOOST_AUTO_TEST_CASE(evenStandalone)
{
    // See above. But note that the even length does not match the
    // MATLAB because it doesn't halve the middle, as MATLAB does

    std::vector<double> in;
    in.push_back(1);
    in.push_back(3);
    in.push_back(5);
    in.push_back(7);

    std::vector<double> out = MedianFilter<double>::filter(4, in);
    BOOST_CHECK_EQUAL(out[0], 3);
    BOOST_CHECK_EQUAL(out[1], 5);
    BOOST_CHECK_EQUAL(out[2], 5);
    BOOST_CHECK_EQUAL(out[3], 5);
}

BOOST_AUTO_TEST_CASE(standaloneEmpty)
{
    std::vector<double> in;
    std::vector<double> out1 = MedianFilter<double>::filter(3, in);
    BOOST_CHECK_EQUAL(out1.size(), 0);
    std::vector<double> out2 = MedianFilter<double>::filter(4, in);
    BOOST_CHECK_EQUAL(out2.size(), 0);
}

BOOST_AUTO_TEST_CASE(types)
{
    MedianFilter<double> dfilt(3);
    dfilt.push(1.2);
    BOOST_CHECK_EQUAL(dfilt.get(), 0.0);
    dfilt.push(2.4);
    BOOST_CHECK_EQUAL(dfilt.get(), 1.2);
    dfilt.push(1e-6);
    BOOST_CHECK_EQUAL(dfilt.get(), 1.2);
    dfilt.push(1e6);
    BOOST_CHECK_EQUAL(dfilt.get(), 2.4);

    MedianFilter<int> ifilt(3);
    ifilt.push(1);
    BOOST_CHECK_EQUAL(ifilt.get(), 0);
    ifilt.push(2);
    BOOST_CHECK_EQUAL(ifilt.get(), 1);
    ifilt.push(0);
    BOOST_CHECK_EQUAL(ifilt.get(), 1);
    ifilt.push(1000);
    BOOST_CHECK_EQUAL(ifilt.get(), 2);
}

BOOST_AUTO_TEST_CASE(inf)
{
    // Inf and -Inf should behave normally
    double pinf = strtod("Inf", 0);
    double ninf = strtod("-Inf", 0);
    MedianFilter<double> dfilt(3);
    dfilt.push(1.0);
    dfilt.push(2.0);
    dfilt.push(pinf);
    BOOST_CHECK_EQUAL(dfilt.get(), 2.0);
    dfilt.push(pinf);
    BOOST_CHECK_EQUAL(dfilt.get(), pinf);
    dfilt.push(pinf);
    BOOST_CHECK_EQUAL(dfilt.get(), pinf);
    dfilt.push(2.0);
    BOOST_CHECK_EQUAL(dfilt.get(), pinf);
    dfilt.push(1.0);
    BOOST_CHECK_EQUAL(dfilt.get(), 2.0);
    dfilt.push(ninf);
    BOOST_CHECK_EQUAL(dfilt.get(), 1.0);
    dfilt.push(pinf);
    BOOST_CHECK_EQUAL(dfilt.get(), 1.0);
    dfilt.push(ninf);
    BOOST_CHECK_EQUAL(dfilt.get(), ninf);
}

BOOST_AUTO_TEST_CASE(nan)
{
    // The filter should never accept a NaN, because it breaks sorting
    // and comparison and that will break the filter. Instead it
    // should insert 0.
    double nan = strtod("NaN", 0);
    MedianFilter<double> dfilt(3);
    dfilt.push(1.0);
    dfilt.push(2.0);
    dfilt.push(nan);
    BOOST_CHECK_EQUAL(dfilt.get(), 1.0);
    dfilt.push(nan);
    BOOST_CHECK_EQUAL(dfilt.get(), 0.0);
    dfilt.push(-11.0);
    BOOST_CHECK_EQUAL(dfilt.get(), 0.0);
    dfilt.push(-1.0);
    BOOST_CHECK_EQUAL(dfilt.get(), -1.0);
}

BOOST_AUTO_TEST_SUITE_END()

    
