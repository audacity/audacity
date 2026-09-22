/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

#include "dsp/chromagram/Chromagram.h"

#include <iostream>

#include <cmath>

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(TestChromagram)

using std::cout;
using std::endl;
using std::string;
using std::vector;

string midiPitchName(int midiPitch)
{
    static string names[] = {
        "C",  "C#", "D",  "D#",
        "E",  "F",  "F#", "G",
        "G#", "A",  "A#", "B"
    };

    return names[midiPitch % 12];
}

vector<double> generateSinusoid(double frequency,
                                double sampleRate,
                                int length)
{
    vector<double> buffer;
    for (int i = 0; i < length; ++i) {
        buffer.push_back(sin((i * M_PI * 2.0 * frequency) / sampleRate));
    }
    return buffer;
}

double frequencyForPitch(int midiPitch, double concertA)
{
    return concertA * pow(2.0, (midiPitch - 69.0) / 12.0);
}

void test_sinusoid_12tET(double concertA, double sampleRate, int bpo)
{
    int chromaMinPitch = 36;
    int chromaMaxPitch = 108;

    int probeMinPitch = 36;
    int probeMaxPitch = 108;
    
    ChromaConfig config {
        sampleRate,
        frequencyForPitch(chromaMinPitch, concertA),
        frequencyForPitch(chromaMaxPitch, concertA),
        bpo,
        0.0054,
        MathUtilities::NormaliseNone
    };
    
    Chromagram chroma(config);

    int binsPerSemi = bpo / 12;
    
    for (int midiPitch = probeMinPitch;
         midiPitch < probeMaxPitch;
         ++midiPitch) {

        int blockSize = chroma.getFrameSize();

        double frequency = frequencyForPitch(midiPitch, concertA);
        int expectedPeakBin =
            ((midiPitch - chromaMinPitch) * binsPerSemi) % bpo;
/*
        cout << "midiPitch = " << midiPitch
             << ", name = " << midiPitchName(midiPitch)
             << ", frequency = " << frequency
             << ", expected peak bin = "
             << expectedPeakBin << endl;
*/        
        vector<double> signal = generateSinusoid(frequency,
                                                 sampleRate,
                                                 blockSize);

        double *output = chroma.process(signal.data());

        int peakBin = -1;
        double peakValue = 0.0;

        for (int i = 0; i < bpo; ++i) {
            if (i == 0 || output[i] > peakValue) {
                peakValue = output[i];
                peakBin = i;
            }
        }
/*
        cout << "peak value = " << peakValue << " at bin " << peakBin << endl;
        cout << "(neighbouring values are "
             << (peakBin > 0 ? output[peakBin-1] : output[bpo-1])
             << " and "
             << (peakBin+1 < bpo ? output[peakBin+1] : output[0])
             << ")" << endl;
        if (peakBin != expectedPeakBin) {
            cout << "NOTE: peak bin " << peakBin << " does not match expected " << expectedPeakBin << endl;
            cout << "bin values are: ";
            for (int i = 0; i < bpo; ++i) {
                cout << i << ": " << output[i] << "   ";
            }
            cout << endl;
        }
*/
        
        BOOST_CHECK_EQUAL(peakBin, expectedPeakBin);
    }
}

BOOST_AUTO_TEST_CASE(sinusoid_12tET_440_44100_36)
{
    test_sinusoid_12tET(440.0, 44100.0, 36);
}
    
BOOST_AUTO_TEST_CASE(sinusoid_12tET_440_44100_60)
{
    test_sinusoid_12tET(440.0, 44100.0, 60);
}

BOOST_AUTO_TEST_CASE(sinusoid_12tET_397_44100_60)
{
    test_sinusoid_12tET(397.0, 44100.0, 60);
}

BOOST_AUTO_TEST_CASE(sinusoid_12tET_440_48000_60)
{
    test_sinusoid_12tET(440.0, 48000.0, 60);
}

BOOST_AUTO_TEST_SUITE_END()
