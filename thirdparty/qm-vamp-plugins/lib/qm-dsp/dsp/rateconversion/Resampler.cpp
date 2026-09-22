/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file by Chris Cannam.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#include "Resampler.h"

#include "maths/MathUtilities.h"
#include "base/KaiserWindow.h"
#include "base/SincWindow.h"
#include "base/Restrict.h"

#include <iostream>
#include <vector>
#include <map>
#include <cassert>
#include <algorithm>

using std::vector;
using std::map;
using std::cerr;
using std::endl;

//#define DEBUG_RESAMPLER 1
//#define DEBUG_RESAMPLER_VERBOSE 1

Resampler::Resampler(int sourceRate, int targetRate) :
    m_sourceRate(sourceRate),
    m_targetRate(targetRate)
{
#ifdef DEBUG_RESAMPLER
    cerr << "Resampler::Resampler(" <<  sourceRate << "," << targetRate << ")" << endl;
#endif
    initialise(100, 0.02);
}

Resampler::Resampler(int sourceRate, int targetRate, 
                     double snr, double bandwidth) :
    m_sourceRate(sourceRate),
    m_targetRate(targetRate)
{
    initialise(snr, bandwidth);
}

Resampler::~Resampler()
{
    delete[] m_phaseData;
}

void
Resampler::initialise(double snr, double bandwidth)
{
    int higher = std::max(m_sourceRate, m_targetRate);
    int lower = std::min(m_sourceRate, m_targetRate);

    m_gcd = MathUtilities::gcd(lower, higher);
    m_peakToPole = higher / m_gcd;

    if (m_targetRate < m_sourceRate) {
        // antialiasing filter, should be slightly below nyquist
        m_peakToPole = m_peakToPole / (1.0 - bandwidth/2.0);
    }

    KaiserWindow::Parameters params =
        KaiserWindow::parametersForBandwidth(snr, bandwidth, higher / m_gcd);

    params.length =
        (params.length % 2 == 0 ? params.length + 1 : params.length);
    
    params.length =
        (params.length > 200001 ? 200001 : params.length);

    m_filterLength = params.length;

    vector<double> filter;

    KaiserWindow kw(params);
    SincWindow sw(m_filterLength, m_peakToPole * 2);

    filter = vector<double>(m_filterLength, 0.0);
    for (int i = 0; i < m_filterLength; ++i) filter[i] = 1.0;
    sw.cut(filter.data());
    kw.cut(filter.data());
    
    int inputSpacing = m_targetRate / m_gcd;
    int outputSpacing = m_sourceRate / m_gcd;

#ifdef DEBUG_RESAMPLER
    cerr << "resample " << m_sourceRate << " -> " << m_targetRate
         << ": inputSpacing " << inputSpacing << ", outputSpacing "
         << outputSpacing << ": filter length " << m_filterLength
         << endl;
#endif

    // Now we have a filter of (odd) length flen in which the lower
    // sample rate corresponds to every n'th point and the higher rate
    // to every m'th where n and m are higher and lower rates divided
    // by their gcd respectively. So if x coordinates are on the same
    // scale as our filter resolution, then source sample i is at i *
    // (targetRate / gcd) and target sample j is at j * (sourceRate /
    // gcd).

    // To reconstruct a single target sample, we want a buffer (real
    // or virtual) of flen values formed of source samples spaced at
    // intervals of (targetRate / gcd), in our example case 3.  This
    // is initially formed with the first sample at the filter peak.
    //
    // 0  0  0  0  a  0  0  b  0
    //
    // and of course we have our filter
    //
    // f1 f2 f3 f4 f5 f6 f7 f8 f9
    //
    // We take the sum of products of non-zero values from this buffer
    // with corresponding values in the filter
    //
    // a * f5 + b * f8
    //
    // Then we drop (sourceRate / gcd) values, in our example case 4,
    // from the start of the buffer and fill until it has flen values
    // again
    //
    // a  0  0  b  0  0  c  0  0
    //
    // repeat to reconstruct the next target sample
    //
    // a * f1 + b * f4 + c * f7
    //
    // and so on.
    //
    // Above I said the buffer could be "real or virtual" -- ours is
    // virtual. We don't actually store all the zero spacing values,
    // except for padding at the start; normally we store only the
    // values that actually came from the source stream, along with a
    // phase value that tells us how many virtual zeroes there are at
    // the start of the virtual buffer.  So the two examples above are
    //
    // 0 a b  [ with phase 1 ]
    // a b c  [ with phase 0 ]
    //
    // Having thus broken down the buffer so that only the elements we
    // need to multiply are present, we can also unzip the filter into
    // every-nth-element subsets at each phase, allowing us to do the
    // filter multiplication as a simply vector multiply. That is, rather
    // than store
    //
    // f1 f2 f3 f4 f5 f6 f7 f8 f9
    // 
    // we store separately
    //
    // f1 f4 f7
    // f2 f5 f8
    // f3 f6 f9
    //
    // Each time we complete a multiply-and-sum, we need to work out
    // how many (real) samples to drop from the start of our buffer,
    // and how many to add at the end of it for the next multiply.  We
    // know we want to drop enough real samples to move along by one
    // computed output sample, which is our outputSpacing number of
    // virtual buffer samples. Depending on the relationship between
    // input and output spacings, this may mean dropping several real
    // samples, one real sample, or none at all (and simply moving to
    // a different "phase").

    m_phaseData = new Phase[inputSpacing];

    for (int phase = 0; phase < inputSpacing; ++phase) {

        Phase p;

        p.nextPhase = phase - outputSpacing;
        while (p.nextPhase < 0) p.nextPhase += inputSpacing;
        p.nextPhase %= inputSpacing;
        
        p.drop = int(ceil(std::max(0.0, double(outputSpacing - phase))
                          / inputSpacing));

        int filtZipLength = int(ceil(double(m_filterLength - phase)
                                     / inputSpacing));

        for (int i = 0; i < filtZipLength; ++i) {
            p.filter.push_back(filter[i * inputSpacing + phase]);
        }

        m_phaseData[phase] = p;
    }

#ifdef DEBUG_RESAMPLER
    int cp = 0;
    int totDrop = 0;
    for (int i = 0; i < inputSpacing; ++i) {
        cerr << "phase = " << cp << ", drop = " << m_phaseData[cp].drop
             << ", filter length = " << m_phaseData[cp].filter.size()
             << ", next phase = " << m_phaseData[cp].nextPhase << endl;
        totDrop += m_phaseData[cp].drop;
        cp = m_phaseData[cp].nextPhase;
    }
    cerr << "total drop = " << totDrop << endl;
#endif

    // The May implementation of this uses a pull model -- we ask the
    // resampler for a certain number of output samples, and it asks
    // its source stream for as many as it needs to calculate
    // those. This means (among other things) that the source stream
    // can be asked for enough samples up-front to fill the buffer
    // before the first output sample is generated.
    // 
    // In this implementation we're using a push model in which a
    // certain number of source samples is provided and we're asked
    // for as many output samples as that makes available. But we
    // can't return any samples from the beginning until half the
    // filter length has been provided as input. This means we must
    // either return a very variable number of samples (none at all
    // until the filter fills, then half the filter length at once) or
    // else have a lengthy declared latency on the output. We do the
    // latter. (What do other implementations do?)
    //
    // We want to make sure the first "real" sample will eventually be
    // aligned with the centre sample in the filter (it's tidier, and
    // easier to do diagnostic calculations that way). So we need to
    // pick the initial phase and buffer fill accordingly.
    // 
    // Example: if the inputSpacing is 2, outputSpacing is 3, and
    // filter length is 7,
    // 
    //    x     x     x     x     a     b     c ... input samples
    // 0  1  2  3  4  5  6  7  8  9 10 11 12 13 ... 
    //          i        j        k        l    ... output samples
    // [--------|--------] <- filter with centre mark
    //
    // Let h be the index of the centre mark, here 3 (generally
    // int(filterLength/2) for odd-length filters).
    //
    // The smallest n such that h + n * outputSpacing > filterLength
    // is 2 (that is, ceil((filterLength - h) / outputSpacing)), and
    // (h + 2 * outputSpacing) % inputSpacing == 1, so the initial
    // phase is 1.
    //
    // To achieve our n, we need to pre-fill the "virtual" buffer with
    // 4 zero samples: the x's above. This is int((h + n *
    // outputSpacing) / inputSpacing). It's the phase that makes this
    // buffer get dealt with in such a way as to give us an effective
    // index for sample a of 9 rather than 8 or 10 or whatever.
    //
    // This gives us output latency of 2 (== n), i.e. output samples i
    // and j will appear before the one in which input sample a is at
    // the centre of the filter.

    int h = int(m_filterLength / 2);
    int n = ceil(double(m_filterLength - h) / outputSpacing);
    
    m_phase = (h + n * outputSpacing) % inputSpacing;

    int fill = (h + n * outputSpacing) / inputSpacing;
    
    m_latency = n;

    m_buffer = vector<double>(fill, 0);
    m_bufferOrigin = 0;

#ifdef DEBUG_RESAMPLER
    cerr << "initial phase " << m_phase << " (as " << (m_filterLength/2) << " % " << inputSpacing << ")"
              << ", latency " << m_latency << endl;
#endif
}

double
Resampler::reconstructOne()
{
    Phase &pd = m_phaseData[m_phase];
    double v = 0.0;
    int n = pd.filter.size();

    if (n + m_bufferOrigin > (int)m_buffer.size()) {
        cerr << "ERROR: n + m_bufferOrigin > m_buffer.size() [" << n << " + "
             << m_bufferOrigin << " > " << m_buffer.size() << "]" << endl;
        throw std::logic_error("n + m_bufferOrigin > m_buffer.size()");
    }

    const double *const QM_R__ buf(m_buffer.data() + m_bufferOrigin);
    const double *const QM_R__ filt(pd.filter.data());

    for (int i = 0; i < n; ++i) {
        // NB gcc can only vectorize this with -ffast-math
        v += buf[i] * filt[i];
    }

    m_bufferOrigin += pd.drop;
    m_phase = pd.nextPhase;
    return v;
}

int
Resampler::process(const double *src, double *dst, int n)
{
    m_buffer.insert(m_buffer.end(), src, src + n);

    int maxout = int(ceil(double(n) * m_targetRate / m_sourceRate));
    int outidx = 0;

#ifdef DEBUG_RESAMPLER
    cerr << "process: buf siz " << m_buffer.size() << " filt siz for phase " << m_phase << " " << m_phaseData[m_phase].filter.size() << endl;
#endif

    double scaleFactor = (double(m_targetRate) / m_gcd) / m_peakToPole;

    while (outidx < maxout &&
           m_buffer.size() >= m_phaseData[m_phase].filter.size() + m_bufferOrigin) {
        dst[outidx] = scaleFactor * reconstructOne();
        outidx++;
    }

    if (m_bufferOrigin > (int)m_buffer.size()) {
        cerr << "ERROR: m_bufferOrigin > m_buffer.size() [" 
             << m_bufferOrigin << " > " << m_buffer.size() << "]" << endl;
        throw std::logic_error("m_bufferOrigin > m_buffer.size()");
    }

    m_buffer = vector<double>(m_buffer.begin() + m_bufferOrigin, m_buffer.end());
    m_bufferOrigin = 0;
    
    return outidx;
}
    
vector<double>
Resampler::process(const double *src, int n)
{
    int maxout = int(ceil(double(n) * m_targetRate / m_sourceRate));
    vector<double> out(maxout, 0.0);
    int got = process(src, out.data(), n);
    assert(got <= maxout);
    if (got < maxout) out.resize(got);
    return out;
}

vector<double>
Resampler::resample(int sourceRate, int targetRate, const double *data, int n)
{
    Resampler r(sourceRate, targetRate);

    int latency = r.getLatency();

    // latency is the output latency. We need to provide enough
    // padding input samples at the end of input to guarantee at
    // *least* the latency's worth of output samples. that is,

    int inputPad = int(ceil((double(latency) * sourceRate) / targetRate));

    // that means we are providing this much input in total:

    int n1 = n + inputPad;

    // and obtaining this much output in total:

    int m1 = int(ceil((double(n1) * targetRate) / sourceRate));

    // in order to return this much output to the user:

    int m = int(ceil((double(n) * targetRate) / sourceRate));
    
#ifdef DEBUG_RESAMPLER
    cerr << "n = " << n << ", sourceRate = " << sourceRate << ", targetRate = " << targetRate << ", m = " << m << ", latency = " << latency << ", inputPad = " << inputPad << ", m1 = " << m1 << ", n1 = " << n1 << ", n1 - n = " << n1 - n << endl;
#endif

    vector<double> pad(n1 - n, 0.0);
    vector<double> out(m1 + 1, 0.0);

    int gotData = r.process(data, out.data(), n);
    int gotPad = r.process(pad.data(), out.data() + gotData, pad.size());
    int got = gotData + gotPad;
    
#ifdef DEBUG_RESAMPLER
    cerr << "resample: " << n << " in, " << pad.size() << " padding, " << got << " out (" << gotData << " data, " << gotPad << " padding, latency = " << latency << ")" << endl;
#endif
#ifdef DEBUG_RESAMPLER_VERBOSE
    int printN = 50;
    cerr << "first " << printN << " in:" << endl;
    for (int i = 0; i < printN && i < n; ++i) {
        if (i % 5 == 0) cerr << endl << i << "... ";
        cerr << data[i] << " ";
    }
    cerr << endl;
#endif

    int toReturn = got - latency;
    if (toReturn > m) toReturn = m;

    vector<double> sliced(out.begin() + latency, 
                          out.begin() + latency + toReturn);

#ifdef DEBUG_RESAMPLER_VERBOSE
    cerr << "first " << printN << " out (after latency compensation), length " << sliced.size() << ":";
    for (int i = 0; i < printN && i < sliced.size(); ++i) {
        if (i % 5 == 0) cerr << endl << i << "... ";
        cerr << sliced[i] << " ";
    }
    cerr << endl;
#endif

    return sliced;
}

