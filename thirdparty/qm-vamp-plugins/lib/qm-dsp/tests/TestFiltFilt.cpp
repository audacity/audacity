/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

#include "dsp/signalconditioning/FiltFilt.h"

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN

#include <boost/test/unit_test.hpp>

#include <stdexcept>

using namespace std;

BOOST_AUTO_TEST_SUITE(TestFiltFilt)

// Our FiltFilt is, apparently intentionally (?) a bit different from
// the equivalents in MATLAB and Octave (which themselves also differ)
// in how it handles transients at either end. While the three should
// all be "roughly" the same, all produce slightly different values.
// So the "expected" values here are, unfortunately, simply produced
// by the function itself, in a process involving tracing the values
// passed in to each of the filter stages within it and compared their
// outputs against MATLAB.

static vector<double> iir_a { 1,5.75501989315662,16.326056867468,28.779190797823,34.2874379215653,28.137815126537,15.6064643257793,5.37874515231553,0.913800050254382,0.0,0.0 };
static vector<double> iir_b { 0.0031954608137085,0.0180937089815597,0.0508407778575426,0.0895040074158415,0.107385387168148,0.0895040074158415,0.0508407778575426,0.0180937089815597,0.0031954608137085,0.0,0.0 };

BOOST_AUTO_TEST_CASE(long_zeroes)
{
    vector<double> a(iir_a);
    vector<double> b(iir_b);

    vector<double> zeroes(1000, 0.0);
    vector<double> expected(zeroes);

    FiltFilt f({ a, b });
    
    int n = expected.size();
    vector<double> out(n, 0.0);

    f.process(zeroes.data(), out.data(), n);
    
    double thresh = 1e-12;
    
    for (int i = 0; i < n; ++i) {
	BOOST_CHECK_SMALL(out[i] - expected[i], thresh);
    }
}

BOOST_AUTO_TEST_CASE(empty)
{
    vector<double> a(iir_a);
    vector<double> b(iir_b);
    vector<double> in;
    vector<double> out;
    vector<double> expected;

    FiltFilt f({ a, b });

    f.process(in.data(), out.data(), 0);
}

BOOST_AUTO_TEST_CASE(single_value)
{
    vector<double> a(iir_a);
    vector<double> b(iir_b);
    vector<double> in({ 1.0 });
    vector<double> out({ 0.0 });
    vector<double> expected({ 0.000607783187363 });

    FiltFilt f({ a, b });

    f.process(in.data(), out.data(), 1);

    double thresh = 1e-12;

    BOOST_CHECK_SMALL(out[0] - expected[0], thresh);
}

BOOST_AUTO_TEST_CASE(shortish)
{
    vector<double> a(iir_a);
    vector<double> b(iir_b);

    vector<double> in { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
    vector<double> expected { 0.0183210564167, -0.0251812631055, 0.0182718199887, -0.00232894428926, -0.0131783133711, 0.0204387725824, -0.016092298975, 0.00402175146108, 0.00913732971217, -0.0158214509991, 0.0138601876508, -0.00455783828038, -0.00563875593951, 0.0121184171941, -0.011073916445, 0.00491681731861 };

    FiltFilt f({ a, b });
    
    int n = expected.size();
    vector<double> out(n, 0.0);

    f.process(in.data(), out.data(), n);
    
    double thresh = 1e-12;
    
    for (int i = 0; i < n; ++i) {
	BOOST_CHECK_SMALL(out[i] - expected[i], thresh);
    }
}

BOOST_AUTO_TEST_SUITE_END()
