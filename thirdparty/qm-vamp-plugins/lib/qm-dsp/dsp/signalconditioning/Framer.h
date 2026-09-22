/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file 2005-2006 Christian Landone.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_FRAMER_H
#define QM_DSP_FRAMER_H

#include <stdint.h>

class Framer  
{
public:
    Framer();
    virtual ~Framer();

    void setSource(double* src, int64_t length);
    void configure(int frameLength, int hop);
    
    int getMaxNoFrames();
    void getFrame(double* dst);

    void resetCounters();

private:
    int64_t m_sampleLen;          // DataLength (samples)
    int m_framesRead;             // Read Frames Index
 
    double* m_srcBuffer;
    double* m_dataFrame;          // Analysis Frame Buffer
    double* m_strideFrame;        // Stride Frame Buffer
    int m_frameLength;            // Analysis Frame Length
    int m_stepSize;               // Analysis Frame Stride

    int m_maxFrames;

    int64_t m_srcIndex;
};

#endif
