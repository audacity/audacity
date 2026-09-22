/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

#include "dsp/keydetection/GetKeyMode.h"

#include <iostream>

#include <cmath>

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(TestGetKeyMode)

using std::cout;
using std::endl;
using std::string;
using std::vector;

string keyName(int index, bool minor)
{
    static string namesMajor[] = {
        "C", "Db", "D", "Eb",
        "E", "F", "F# / Gb", "G",
        "Ab", "A", "Bb", "B"
    };

    static string namesMinor[] = {
        "C", "C#", "D", "Eb / D#",
        "E", "F", "F#", "G",
        "G#", "A", "Bb", "B"
    };

    if (index < 1 || index > 12) return "";

    std::string name;
    if (minor) name = namesMinor[index - 1] + " minor";
    else name = namesMajor[index - 1] + " major";
    return name;
}

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
                                int sampleRate,
                                int length)
{
    vector<double> buffer;
    buffer.reserve(length);
    for (int i = 0; i < length; ++i) {
        buffer.push_back(sin(i * M_PI * 2.0 * frequency / sampleRate));
    }
    return buffer;
}
    
BOOST_AUTO_TEST_CASE(sinusoid_12tET)
{
    double concertA = 440.0;
    int sampleRate = 44100;
    
    for (int midiPitch = 48; midiPitch < 96; ++midiPitch) {

        GetKeyMode::Config config(sampleRate, concertA);
        GetKeyMode gkm(config);
        int blockSize = gkm.getBlockSize();
        int hopSize = gkm.getHopSize();

        double frequency = concertA * pow(2.0, (midiPitch - 69.0) / 12.0);

        int blocks = 4;
        int totalLength = blockSize * blocks;
        vector<double> signal = generateSinusoid(frequency, sampleRate,
                                                 totalLength);

        int key;
        
        for (int offset = 0; offset + blockSize < totalLength;
             offset += hopSize) {
            int k = gkm.process(signal.data() + offset);
            if (offset == 0) {
                key = k;
            } else {
                BOOST_CHECK_EQUAL(key, k);
            }
        }

        bool minor = (key > 12);

        int tonic = key;
        if (minor) tonic -= 12;

        BOOST_CHECK_EQUAL(tonic, 1 + (midiPitch % 12));
    }
}

BOOST_AUTO_TEST_SUITE_END()
