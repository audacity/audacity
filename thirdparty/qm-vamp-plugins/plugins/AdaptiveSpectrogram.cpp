/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM Vamp Plugin Set

    Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#include "AdaptiveSpectrogram.h"

#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cfloat>

#include <iostream>

#include <dsp/transforms/FFT.h>
#include <dsp/rateconversion/Decimator.h>

using std::string;
using std::vector;
using std::cerr;
using std::endl;

using Vamp::RealTime;

//#define DEBUG_VERBOSE 1

AdaptiveSpectrogram::AdaptiveSpectrogram(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_w(8),
    m_n(2),
    m_coarse(false),
    m_threaded(true),
    m_decFactor(1),
    m_buffer(0),
    m_buflen(0),
    m_decimator(0),
    m_threadsInUse(false)
{
}

AdaptiveSpectrogram::~AdaptiveSpectrogram()
{
    for (int i = 0; i < int(m_cutThreads.size()); ++i) {
        delete m_cutThreads[i];
    }
    m_cutThreads.clear();

    for (FFTMap::iterator i = m_fftThreads.begin();
         i != m_fftThreads.end(); ++i) {
        delete i->second;
    }
    m_fftThreads.clear();

    delete[] m_buffer;
    delete m_decimator;
}

string
AdaptiveSpectrogram::getIdentifier() const
{
    return "qm-adaptivespectrogram";
}

string
AdaptiveSpectrogram::getName() const
{
    return "Adaptive Spectrogram";
}

string
AdaptiveSpectrogram::getDescription() const
{
    return "Produce an adaptive spectrogram by adaptive selection from spectrograms at multiple resolutions";
}

string
AdaptiveSpectrogram::getMaker() const
{
    return "Queen Mary, University of London";
}

int
AdaptiveSpectrogram::getPluginVersion() const
{
    return 2;
}

string
AdaptiveSpectrogram::getCopyright() const
{
    return "Plugin by Wen Xue and Chris Cannam.  Copyright (c) 2009 Wen Xue and QMUL - All Rights Reserved";
}

size_t
AdaptiveSpectrogram::getPreferredStepSize() const
{
    return getPreferredBlockSize();
}

size_t
AdaptiveSpectrogram::getPreferredBlockSize() const
{
    // the /2 at the end is for 50% overlap (we handle framing ourselves)
    return ((2 << m_w) << m_n) * m_decFactor / 2;
}

bool
AdaptiveSpectrogram::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    if (blockSize != getPreferredBlockSize()) {
        std::cerr << "AdaptiveSpectrogram::initialise: Block size " << blockSize << " does not match required block size of " << getPreferredBlockSize() << std::endl;
        return false;
    }
    if (stepSize != getPreferredStepSize()) {
        std::cerr << "AdaptiveSpectrogram::initialise: Step size " << stepSize << " does not match required step size of " << getPreferredStepSize() << std::endl;
        return false;
    }

    if (m_decFactor > 1) {
        m_decimator = new Decimator(blockSize, m_decFactor);
    }

    m_buflen = (blockSize * 2) / m_decFactor; // *2 for 50% overlap
    m_buffer = new float[m_buflen];

    reset();
    
    return true;
}

void
AdaptiveSpectrogram::reset()
{
    if (m_decimator) m_decimator->resetFilter();
    for (int i = 0; i < m_buflen; ++i) m_buffer[i] = 0.f;
}

AdaptiveSpectrogram::ParameterList
AdaptiveSpectrogram::getParameterDescriptors() const
{
    ParameterList list;

    ParameterDescriptor desc;
    desc.identifier = "n";
    desc.name = "Number of resolutions";
    desc.description = "Number of consecutive powers of two in the range to be used as spectrogram resolutions, starting with the minimum resolution specified";
    desc.unit = "";
    desc.minValue = 2;
    desc.maxValue = 10;
    desc.defaultValue = 3;
    desc.isQuantized = true;
    desc.quantizeStep = 1;
    list.push_back(desc);

    ParameterDescriptor desc2;
    desc2.identifier = "w";
    desc2.name = "Smallest resolution";
    desc2.description = "Smallest of the consecutive powers of two to use as spectrogram resolutions";
    desc2.unit = "";
    desc2.minValue = 1;
    desc2.maxValue = 14;
    desc2.defaultValue = 9;
    desc2.isQuantized = true;
    desc2.quantizeStep = 1;
    // I am so lazy
    desc2.valueNames.push_back("2");
    desc2.valueNames.push_back("4");
    desc2.valueNames.push_back("8");
    desc2.valueNames.push_back("16");
    desc2.valueNames.push_back("32");
    desc2.valueNames.push_back("64");
    desc2.valueNames.push_back("128");
    desc2.valueNames.push_back("256");
    desc2.valueNames.push_back("512");
    desc2.valueNames.push_back("1024");
    desc2.valueNames.push_back("2048");
    desc2.valueNames.push_back("4096");
    desc2.valueNames.push_back("8192");
    desc2.valueNames.push_back("16384");
    list.push_back(desc2);

    desc2.identifier = "dec";
    desc2.name = "Decimation factor";
    desc2.description = "Factor to down-sample by, increasing speed but lowering maximum frequency";
    desc2.unit = "";
    desc2.minValue = 0;
    desc2.maxValue = 3;
    desc2.defaultValue = 0;
    desc2.isQuantized = true;
    desc2.quantizeStep = 1;
    desc2.valueNames.clear();
    desc2.valueNames.push_back("No decimation");
    desc2.valueNames.push_back("2");
    desc2.valueNames.push_back("4");
    desc2.valueNames.push_back("8");
    list.push_back(desc2);

    ParameterDescriptor desc3;
    desc3.identifier = "coarse";
    desc3.name = "Omit alternate resolutions";
    desc3.description = "Generate a coarser spectrogram faster by excluding every alternate resolution (first and last resolution are always retained)";
    desc3.unit = "";
    desc3.minValue = 0;
    desc3.maxValue = 1;
    desc3.defaultValue = 0;
    desc3.isQuantized = true;
    desc3.quantizeStep = 1;
    list.push_back(desc3);

    desc3.identifier = "threaded";
    desc3.name = "Multi-threaded processing";
    desc3.description = "Perform calculations using several threads in parallel";
    desc3.unit = "";
    desc3.minValue = 0;
    desc3.maxValue = 1;
    desc3.defaultValue = 1;
    desc3.isQuantized = true;
    desc3.quantizeStep = 1;
    list.push_back(desc3);

    return list;
}

float
AdaptiveSpectrogram::getParameter(std::string id) const
{
    if (id == "n") return m_n+1;
    else if (id == "w") return m_w+1;
    else if (id == "threaded") return (m_threaded ? 1 : 0);
    else if (id == "coarse") return (m_coarse ? 1 : 0);
    else if (id == "dec") {
        int f = m_decFactor;
        int p = 0;
        while (f > 1) {
            f >>= 1;
            p += 1;
        }
        return p;
    }
    return 0.f;
}

void
AdaptiveSpectrogram::setParameter(std::string id, float value)
{
    if (id == "n") {
        int n = lrintf(value);
        if (n >= 1 && n <= 10) m_n = n-1;
    } else if (id == "w") {
        int w = lrintf(value);
        if (w >= 1 && w <= 14) m_w = w-1;
    } else if (id == "threaded") {
        m_threaded = (value > 0.5);
    } else if (id == "coarse") {
        m_coarse = (value > 0.5);
    } else if (id == "dec") {
        int p = lrintf(value);
        if (p >= 0 && p <= 3) m_decFactor = 1 << p;
    }
}

AdaptiveSpectrogram::OutputList
AdaptiveSpectrogram::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor d;
    d.identifier = "output";
    d.name = "Output";
    d.description = "The output of the plugin";
    d.unit = "";
    d.hasFixedBinCount = true;
    d.binCount = getPreferredBlockSize() / (m_decFactor * 2);
    d.hasKnownExtents = false;
    d.isQuantized = false;
    d.sampleType = OutputDescriptor::FixedSampleRate;
    d.sampleRate = m_inputSampleRate / (m_decFactor * ((2 << m_w) / 2));
    d.hasDuration = false;
    char name[20];
    for (int i = 0; i < int(d.binCount); ++i) {
        float freq = (m_inputSampleRate / (m_decFactor * (d.binCount * 2)) * (i + 1)); // no DC bin
        sprintf(name, "%.1f Hz", freq);
        d.binNames.push_back(name);
    }
    list.push_back(d);

    return list;
}

AdaptiveSpectrogram::FeatureSet
AdaptiveSpectrogram::getRemainingFeatures()
{
    FeatureSet fs;
    return fs;
}

AdaptiveSpectrogram::FeatureSet
AdaptiveSpectrogram::process(const float *const *inputBuffers, RealTime)
{
    // framing: shift and write the new data to right half
    for (int i = 0; i < m_buflen/2; ++i) {
        m_buffer[i] = m_buffer[m_buflen/2 + i];
    }

    if (m_decFactor == 1) {
        for (int i = 0; i < m_buflen/2; ++i) {
            m_buffer[m_buflen/2 + i] = inputBuffers[0][i];
        }
    } else {
        m_decimator->process(inputBuffers[0], m_buffer + m_buflen/2);
    }

    FeatureSet fs;

    int minwid = (2 << m_w), maxwid = ((2 << m_w) << m_n);

#ifdef DEBUG_VERBOSE
    cerr << "widths from " << minwid << " to " << maxwid << " ("
         << minwid/2 << " to " << maxwid/2 << " in real parts)" << endl;
#endif

    Spectrograms s(minwid/2, maxwid/2, 1);

    int w = minwid;
    int index = 0;

    while (w <= maxwid) {

        if (!isResolutionWanted(s, w/2)) {
            w *= 2;
            ++index;
            continue;
        }

        if (m_fftThreads.find(w) == m_fftThreads.end()) {
            m_fftThreads[w] = new FFTThread(w);
        }
        if (m_threaded) {
            m_fftThreads[w]->startCalculation(m_buffer, s, index, maxwid);
        } else {
            m_fftThreads[w]->setParameters(m_buffer, s, index, maxwid);
            m_fftThreads[w]->performTask();
        }
        w *= 2;
        ++index;
    }

    if (m_threaded) {
        w = minwid;
        index = 0;
        while (w <= maxwid) {
            if (!isResolutionWanted(s, w/2)) {
                w *= 2;
                ++index;
                continue;
            }
            m_fftThreads[w]->await();
            w *= 2;
            ++index;
        }
    }

    m_threadsInUse = false;

//    std::cerr << "maxwid/2 = " << maxwid/2 << ", minwid/2 = " << minwid/2 << ", n+1 = " << m_n+1 << ", 2^(n+1) = " << (2<<m_n) << std::endl;

    int cutwid = maxwid/2;
    Cutting *cutting = cut(s, cutwid, 0, 0, cutwid, 0);

#ifdef DEBUG_VERBOSE
    printCutting(cutting, "  ");
#endif

    vector<vector<float> > rmat(maxwid/minwid);
    for (int i = 0; i < maxwid/minwid; ++i) {
        rmat[i] = vector<float>(maxwid/2);
    }
    
    assemble(s, cutting, rmat, 0, 0, maxwid/minwid, cutwid);

    cutting->erase();

    for (int i = 0; i < int(rmat.size()); ++i) {
        Feature f;
        f.hasTimestamp = false;
        f.values = rmat[i];
        fs[0].push_back(f);
    }

//    std::cerr << "process returning!\n" << std::endl;

    return fs;
}

void
AdaptiveSpectrogram::printCutting(Cutting *c, string pfx) const
{
    if (c->first) {
        if (c->cut == Cutting::Horizontal) {
            cerr << pfx << "H" << endl;
        } else if (c->cut == Cutting::Vertical) {
            cerr << pfx << "V" << endl;
        }
        printCutting(c->first, pfx + "  ");
        printCutting(c->second, pfx + "  ");
    } else {
        cerr << pfx << "* " << c->value << endl;
    }
}

void
AdaptiveSpectrogram::getSubCuts(const Spectrograms &s,
                                int res,
                                int x, int y, int h,
                                Cutting **top, Cutting **bottom,
                                Cutting **left, Cutting **right,
                                BlockAllocator *allocator) const
{
    if (m_threaded && !m_threadsInUse) {

        m_threadsInUse = true;

        if (m_cutThreads.empty()) {
            for (int i = 0; i < 4; ++i) {
                CutThread *t = new CutThread(this);
                m_cutThreads.push_back(t);
            }
        }

        // Cut threads 0 and 1 calculate the top and bottom halves;
        // threads 2 and 3 calculate left and right.  See notes in
        // unthreaded code below for more information.

        if (top) m_cutThreads[0]->cut(s, res, x, y + h/2, h/2);
        if (bottom) m_cutThreads[1]->cut(s, res, x, y, h/2);

        if (left) m_cutThreads[2]->cut(s, res/2, 2 * x, y/2, h/2);
        if (right) m_cutThreads[3]->cut(s, res/2, 2 * x + 1, y/2, h/2);

        if (top) *top = m_cutThreads[0]->get();
        if (bottom) *bottom = m_cutThreads[1]->get();
        if (left) *left = m_cutThreads[2]->get();
        if (right) *right = m_cutThreads[3]->get();

    } else {

        // Unthreaded version

        // The "vertical" division is a top/bottom split.
        // Splitting this way keeps us in the same resolution,
        // but with two vertical subregions of height h/2.

        if (top) *top = cut(s, res, x, y + h/2, h/2, allocator);
        if (bottom) *bottom = cut(s, res, x, y, h/2, allocator);

        // The "horizontal" division is a left/right split.  Splitting
        // this way places us in resolution res/2, which has lower
        // vertical resolution but higher horizontal resolution.  We
        // need to double x accordingly.
        
        if (left) *left = cut(s, res/2, 2 * x, y/2, h/2, allocator);
        if (right) *right = cut(s, res/2, 2 * x + 1, y/2, h/2, allocator);
    }
}

AdaptiveSpectrogram::Cutting *
AdaptiveSpectrogram::cut(const Spectrograms &s,
                         int res,
                         int x, int y, int h,
                         BlockAllocator *allocator) const
{
//    cerr << "res = " << res << ", x = " << x << ", y = " << y << ", h = " << h << endl;

    Cutting *cutting;
    if (allocator) {
        cutting = (Cutting *)(allocator->allocate());
        cutting->allocator = allocator;
    } else {
        cutting = new Cutting;
        cutting->allocator = 0;
    }

    if (h > 1 && res > s.minres) {

        if (!isResolutionWanted(s, res)) {

            Cutting *left = 0, *right = 0;
            getSubCuts(s, res, x, y, h, 0, 0, &left, &right, allocator);
                
            double hcost = left->cost + right->cost;
            double henergy = left->value + right->value;
            hcost = normalize(hcost, henergy);
                
            cutting->cut = Cutting::Horizontal;
            cutting->first = left;
            cutting->second = right;
            cutting->cost = hcost;
            cutting->value = left->value + right->value;

        } else if (h == 2 && !isResolutionWanted(s, res/2)) {

            Cutting *top = 0, *bottom = 0;
            getSubCuts(s, res, x, y, h, &top, &bottom, 0, 0, allocator);
                
            double vcost = top->cost + bottom->cost;
            double venergy = top->value + bottom->value;
            vcost = normalize(vcost, venergy);
                
            cutting->cut = Cutting::Vertical;
            cutting->first = top;
            cutting->second = bottom;
            cutting->cost = vcost;
            cutting->value = top->value + bottom->value;

        } else {

            Cutting *top = 0, *bottom = 0, *left = 0, *right = 0;
            getSubCuts(s, res, x, y, h, &top, &bottom, &left, &right, allocator);

            double vcost = top->cost + bottom->cost;
            double venergy = top->value + bottom->value;
            vcost = normalize(vcost, venergy);
            
            double hcost = left->cost + right->cost;
            double henergy = left->value + right->value;
            hcost = normalize(hcost, henergy);
            
            if (vcost > hcost) {
                cutting->cut = Cutting::Horizontal;
                cutting->first = left;
                cutting->second = right;
                cutting->cost = hcost;
                cutting->value = left->value + right->value;
                top->erase();
                bottom->erase();
                return cutting;
            } else {
                cutting->cut = Cutting::Vertical;
                cutting->first = top;
                cutting->second = bottom;
                cutting->cost = vcost;
                cutting->value = top->value + bottom->value;
                left->erase();
                right->erase();
                return cutting;
            }
        }

    } else {

        // no cuts possible from this level

        cutting->cut = Cutting::Finished;
        cutting->first = 0;
        cutting->second = 0;

        int n = 0;
        for (int r = res; r > s.minres; r >>= 1) ++n;
        const Spectrogram *spectrogram = s.spectrograms[n];
        cutting->cost = cost(*spectrogram, x, y);
        cutting->value = value(*spectrogram, x, y);
    }

    return cutting;
}

void
AdaptiveSpectrogram::assemble(const Spectrograms &s,
                              const Cutting *cutting,
                              vector<vector<float> > &rmat,
                              int x, int y, int w, int h) const
{
    switch (cutting->cut) {

    case Cutting::Finished:
        for (int i = 0; i < w; ++i) {
            for (int j = 0; j < h; ++j) {
                rmat[x+i][y+j] = cutting->value;
            }
        }
        return;

    case Cutting::Horizontal:
        assemble(s, cutting->first, rmat, x, y, w/2, h);
        assemble(s, cutting->second, rmat, x+w/2, y, w/2, h);
        break;
        
    case Cutting::Vertical:
        assemble(s, cutting->first, rmat, x, y+h/2, w, h/2);
        assemble(s, cutting->second, rmat, x, y, w, h/2);
        break;
    }        
}
            
