/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

#include "dsp/transforms/DCT.h"

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN

#include <boost/test/unit_test.hpp>

#include <stdexcept>

using namespace std;

BOOST_AUTO_TEST_SUITE(TestDCT)

BOOST_AUTO_TEST_CASE(forwardArrayBounds)
{
    // initialise bins to something recognisable, so we can tell if
    // they haven't been written; and allocate the inputs on the heap
    // so that, if running under valgrind, we get warnings about
    // overruns
    double *in = new double[4];
    in[0] = 1;
    in[1] = 1;
    in[2] = -1;
    in[3] = -1;
    double out[] = { 999, 999, 999, 999, 999, 999 };
    DCT(4).forward(in, out+1);
    // And check we haven't overrun the arrays
    BOOST_CHECK_EQUAL(out[0], 999.0);
    BOOST_CHECK_EQUAL(out[5], 999.0);
    delete[] in;
}

BOOST_AUTO_TEST_CASE(u_forwardArrayBounds)
{
    // initialise bins to something recognisable, so we can tell if
    // they haven't been written; and allocate the inputs on the heap
    // so that, if running under valgrind, we get warnings about
    // overruns
    double *in = new double[4];
    in[0] = 1;
    in[1] = 1;
    in[2] = -1;
    in[3] = -1;
    double out[] = { 999, 999, 999, 999, 999, 999 };
    DCT(4).forwardUnitary(in, out+1);
    // And check we haven't overrun the arrays
    BOOST_CHECK_EQUAL(out[0], 999.0);
    BOOST_CHECK_EQUAL(out[5], 999.0);
    delete[] in;
}

BOOST_AUTO_TEST_CASE(inverseArrayBounds)
{
    // initialise bins to something recognisable, so we can tell if
    // they haven't been written; and allocate the inputs on the heap
    // so that, if running under valgrind, we get warnings about
    // overruns
    double *in = new double[4];
    in[0] = 1;
    in[1] = 1;
    in[2] = -1;
    in[3] = -1;
    double out[] = { 999, 999, 999, 999, 999, 999 };
    DCT(4).inverse(in, out+1);
    // And check we haven't overrun the arrays
    BOOST_CHECK_EQUAL(out[0], 999.0);
    BOOST_CHECK_EQUAL(out[5], 999.0);
    delete[] in;

}

BOOST_AUTO_TEST_CASE(u_inverseArrayBounds)
{
    // initialise bins to something recognisable, so we can tell if
    // they haven't been written; and allocate the inputs on the heap
    // so that, if running under valgrind, we get warnings about
    // overruns
    double *in = new double[4];
    in[0] = 1;
    in[1] = 1;
    in[2] = -1;
    in[3] = -1;
    double out[] = { 999, 999, 999, 999, 999, 999 };
    DCT(4).inverseUnitary(in, out+1);
    // And check we haven't overrun the arrays
    BOOST_CHECK_EQUAL(out[0], 999.0);
    BOOST_CHECK_EQUAL(out[5], 999.0);
    delete[] in;
}

BOOST_AUTO_TEST_CASE(nonU4)
{
    int n = 4;
    vector<double> in { 1, 2, 3, 5 };
    vector<double> out(n);
    vector<double> inv(n);
    vector<double> expected { 22.0, -8.1564, 1.4142, -1.2137 };
    
    DCT d(n);

    // our expected values are very approximate
    double thresh = 1e-4;

    d.forward(in.data(), out.data());
    
    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(expected[i] - out[i], thresh);
    }

    d.inverse(out.data(), inv.data());

    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(in[i] - inv[i], thresh);
    }
    
    // do it again, just in case some internal state was modified in inverse
    
    d.forward(in.data(), out.data());
    
    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(expected[i] - out[i], thresh);
    }
}

BOOST_AUTO_TEST_CASE(u4)
{
    int n = 4;
    vector<double> in { 1, 2, 3, 5 };
    vector<double> out(n);
    vector<double> inv(n);
    vector<double> expected { 5.5, -2.8837, 0.5, -0.4291 };
    
    DCT d(n);

    // our expected values are very approximate
    double thresh = 1e-4;

    d.forwardUnitary(in.data(), out.data());
    
    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(expected[i] - out[i], thresh);
    }

    d.inverseUnitary(out.data(), inv.data());

    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(in[i] - inv[i], thresh);
    }
    
    // do it again, just in case some internal state was modified in inverse
    
    d.forwardUnitary(in.data(), out.data());
    
    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(expected[i] - out[i], thresh);
    }
}

BOOST_AUTO_TEST_CASE(u5)
{
    int n = 5;
    vector<double> in { 1, 2, 3, 5, 6 };
    vector<double> out(n);
    vector<double> inv(n);
    vector<double> expected { 7.6026, -4.1227, 0.3162, -0.0542, -0.3162 };
    
    DCT d(n);

    // our expected values are very approximate
    double thresh = 1e-4;

    d.forwardUnitary(in.data(), out.data());
    
    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(expected[i] - out[i], thresh);
    }

    d.inverseUnitary(out.data(), inv.data());

    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(in[i] - inv[i], thresh);
    }
    
    // do it again, just in case some internal state was modified in inverse
    
    d.forwardUnitary(in.data(), out.data());
    
    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(expected[i] - out[i], thresh);
    }
}

BOOST_AUTO_TEST_SUITE_END()

