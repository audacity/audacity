/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
 *  Segmenter.h
 *  soundbite
 *
 *  Created by Mark Levy on 23/03/2006.
 *  Copyright 2006 Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
 *
 */

#ifndef QM_DSP_SEGMENTER_H
#define QM_DSP_SEGMENTER_H

#include <vector>
#include <iostream>

class Segment
{
public:
    int start;              // in samples
    int end;
    int type;
};

class Segmentation
{
public:
    int nsegtypes;          // number of segment types, so possible types are {0,1,...,nsegtypes-1}
    int samplerate;
    std::vector<Segment> segments;       
};

std::ostream& operator<<(std::ostream& os, const Segmentation& s);

class Segmenter
{
public:
    Segmenter() {}
    virtual ~Segmenter() {}
    virtual void initialise(int samplerate) = 0;    // must be called before any other methods
    virtual int getWindowsize() = 0;                                // required window size for calls to extractFeatures()
    virtual int getHopsize() = 0;                                   // required hop size for calls to extractFeatures()
    virtual void extractFeatures(const double* samples, int nsamples) = 0;
    virtual void segment() = 0;                                             // call once all the features have been extracted
    virtual void segment(int m) = 0;                                // specify desired number of segment-types
    virtual void clear() { features.clear(); }
    const Segmentation& getSegmentation() const { return segmentation; } 
protected:
    std::vector<std::vector<double> > features;
    Segmentation segmentation;
    int samplerate;
};

#endif
