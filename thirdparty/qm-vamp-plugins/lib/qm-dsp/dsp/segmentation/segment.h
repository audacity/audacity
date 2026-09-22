/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
 *  segment.h
 *
 *  Created by Mark Levy on 06/04/2006.
 *  Copyright 2006 Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
 *
 */

#ifndef QM_DSP_SEGMENT_H
#define QM_DSP_SEGMENT_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct segment_t
{
    long start;                     /* in samples */
    long end;
    int type;
} segment_t;

typedef struct segmentation_t
{
    int nsegs; /* number of segments */
    int nsegtypes; /* number of segment types, so possible types are {0,1,...,nsegtypes-1} */
    int samplerate;
    segment_t* segments;
} segmentation_t;

typedef enum 
{ 
    FEATURE_TYPE_UNKNOWN = 0, 
    FEATURE_TYPE_CONSTQ = 1, 
    FEATURE_TYPE_CHROMA = 2,
    FEATURE_TYPE_MFCC = 3
} feature_types;

#ifdef __cplusplus
}
#endif

#endif

