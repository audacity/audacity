/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
    Copyright (c) 2005 Centre for Digital Music ( C4DM )
                       Queen Mary Univesrity of London

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
 */

#ifndef QM_DSP_GETKEYMODE_H
#define QM_DSP_GETKEYMODE_H

class Decimator;
class Chromagram;

class GetKeyMode  
{
public:
    struct Config {
        double sampleRate;
        float tuningFrequency;
        double hpcpAverage;
        double medianAverage;
        int frameOverlapFactor; // 1 = none (default, fast, but means
                                // we skip a fair bit of input data);
                                // 8 = normal chroma overlap
        int decimationFactor;

        Config(double _sampleRate, float _tuningFrequency) :
            sampleRate(_sampleRate),
            tuningFrequency(_tuningFrequency),
            hpcpAverage(10),
            medianAverage(10),
            frameOverlapFactor(1),
            decimationFactor(8) {
        }
    };
    
    GetKeyMode(Config config);

    virtual ~GetKeyMode();

    /**
     * Process a single time-domain input sample frame of length
     * getBlockSize(). Successive calls should provide overlapped data
     * with an advance of getHopSize() between frames.
     *
     * Return a key index in the range 0-24, where 0 indicates no key
     * detected, 1 is C major, and 13 is C minor.
     */
    int process(double *pcmData);

    /**
     * Return a pointer to an internal 24-element array containing the
     * correlation of the chroma vector generated in the last
     * process() call against the stored key profiles for the 12 major
     * and 12 minor keys, where index 0 is C major and 12 is C minor.
     */
    double *getKeyStrengths();

    int getBlockSize() {
        return m_chromaFrameSize * m_decimationFactor;
    }
    int getHopSize() {
        return m_chromaHopSize * m_decimationFactor;
    }

protected:
    double krumCorr(const double *pDataNorm, const double *pProfileNorm, 
                    int shiftProfile, int length);

    double m_hpcpAverage;
    double m_medianAverage;
    int m_decimationFactor;

    // Decimator (fixed)
    Decimator* m_decimator;

    // Chromagram object
    Chromagram* m_chroma;

    // Chromagram output pointer
    double* m_chrPointer;

    // Framesize
    int m_chromaFrameSize;

    // Hop
    int m_chromaHopSize;

    int m_chromaBufferSize;
    int m_medianWinSize;
        
    int m_bufferIndex;
    int m_chromaBufferFilling;
    int m_medianBufferFilling;

    double* m_decimatedBuffer;
    double* m_chromaBuffer;
    double* m_meanHPCP;

    double* m_majProfileNorm;
    double* m_minProfileNorm;
    double* m_majCorr;
    double* m_minCorr;
    int* m_medianFilterBuffer;
    int* m_sortedBuffer;

    double *m_keyStrengths;
};

#endif // !defined QM_DSP_GETKEYMODE_H
