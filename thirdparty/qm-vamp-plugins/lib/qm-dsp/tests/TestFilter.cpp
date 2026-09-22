/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

#include "dsp/signalconditioning/Filter.h"

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN

#include <boost/test/unit_test.hpp>

#include <stdexcept>

using namespace std;

BOOST_AUTO_TEST_SUITE(TestFilter)

static vector<double> in { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

static vector<double> iir_a { 1,5.75501989315662,16.326056867468,28.779190797823,34.2874379215653,28.137815126537,15.6064643257793,5.37874515231553,0.913800050254382,0.0,0.0 };
static vector<double> iir_b { 0.0031954608137085,0.0180937089815597,0.0508407778575426,0.0895040074158415,0.107385387168148,0.0895040074158415,0.0508407778575426,0.0180937089815597,0.0031954608137085,0.0,0.0 };
static vector<double> iir_expected { 0.003195460813709, 0.006094690058282, 0.009370240771381, 0.012857578361690, 0.015328760300750, 0.019107809614909, 0.022257958968869, 0.024598034053011, 0.029106103380941, 0.031152166476509, 0.034424013713795, 0.038775350541015, 0.039924063374886, 0.044846280036012, 0.047614917256999, 0.049338485830505 };

static vector<double> fir_b { -1.5511e-18,-0.022664,1.047e-17,0.27398,0.49737,0.27398,1.047e-17,-0.022664,-1.5511e-18 };
static vector<double> fir_expected { -1.5511e-18,-0.022664,-0.045328,0.20599,0.95467,1.9773,3,4,5,6,7,8,9,10,11,12 };

BOOST_AUTO_TEST_CASE(iir)
{
    vector<double> a(iir_a);
    vector<double> b(iir_b);
    vector<double> expected(iir_expected);

    Filter f({ a, b });
    
    int n = expected.size();
    vector<double> out(n, 0.0);

    f.process(in.data(), out.data(), n);
    
    double thresh = 1e-12;
    
    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(out[i] - expected[i], thresh);
    }
}

BOOST_AUTO_TEST_CASE(iir_chunked)
{
    vector<double> a(iir_a);
    vector<double> b(iir_b);
    vector<double> expected(iir_expected);

    Filter f({ a, b });
    
    int n = expected.size();
    vector<double> out(n, 0.0);

    int j = 0;
    int i = 0;
    while (j < n) {
        if (++i == 4) {
            i = 1;
        }
        if (j + i >= n) {
            i = n - j;
        }
        f.process(in.data() + j, out.data() + j, i);
        j += i;
    }
    
    double thresh = 1e-12;
    
    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(out[i] - expected[i], thresh);
    }
}

BOOST_AUTO_TEST_CASE(fir)
{
    vector<double> b(fir_b);
    vector<double> expected(fir_expected);

    Filter f({ {}, b });
    
    int n = expected.size();
    vector<double> out(n, 0.0);

    f.process(in.data(), out.data(), n);
    
    double thresh = 1e-4;
    
    for (int i = 0; i < n; ++i) {
        BOOST_CHECK_SMALL(out[i] - expected[i], thresh);
    }
}

BOOST_AUTO_TEST_SUITE_END()

