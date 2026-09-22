/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright 2008-2009 Matthew Davies and QMUL.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_DOWNBEAT_H
#define QM_DSP_DOWNBEAT_H

#include <vector>
#include <cstddef>

#include "dsp/rateconversion/Decimator.h"

class FFTReal;

/**
 * This class takes an input audio signal and a sequence of beat
 * locations (calculated e.g. by TempoTrackV2) and estimates which of
 * the beat locations are downbeats (first beat of the bar).
 * 
 * The input audio signal is expected to have been downsampled to a
 * very low sampling rate (e.g. 2700Hz).  A utility function for
 * downsampling and buffering incoming block-by-block audio is
 * provided.
 */
class DownBeat
{
public:
    /**
     * Construct a downbeat locator that will operate on audio at the
     * downsampled by the given decimation factor from the given
     * original sample rate, plus beats extracted from the same audio
     * at the given original sample rate with the given frame
     * increment.
     *
     * decimationFactor must be a power of two no greater than 64, and
     * dfIncrement must be a multiple of decimationFactor.
     */
    DownBeat(float originalSampleRate,
             size_t decimationFactor,
             size_t dfIncrement);
    ~DownBeat();

    void setBeatsPerBar(int bpb);

    /**
     * Estimate which beats are down-beats.
     * 
     * audio contains the input audio stream after downsampling, and
     * audioLength contains the number of samples in this downsampled
     * stream.
     *
     * beats contains a series of beat positions expressed in
     * multiples of the df increment at the audio's original sample
     * rate, as described to the constructor.
     *
     * The returned downbeat array contains a series of indices to the
     * beats array.
     */
    void findDownBeats(const float *audio, // downsampled
                       size_t audioLength, // after downsampling
                       const std::vector<double> &beats,
                       std::vector<int> &downbeats);

    /**
     * Return the beat spectral difference function.  This is
     * calculated during findDownBeats, so this function can only be
     * meaningfully called after that has completed.  The returned
     * vector contains one value for each of the beat times passed in
     * to findDownBeats, less one.  Each value contains the spectral
     * difference between region prior to the beat's nominal position
     * and the region following it.
     */
    void getBeatSD(std::vector<double> &beatsd) const;
    
    /**
     * For your downsampling convenience: call this function
     * repeatedly with input audio blocks containing dfIncrement
     * samples at the original sample rate, to decimate them to the
     * downsampled rate and buffer them within the DownBeat class.
     *     
     * Call getBufferedAudio() to retrieve the results after all
     * blocks have been processed.
     */
    void pushAudioBlock(const float *audio);
    
    /**
     * Retrieve the accumulated audio produced by pushAudioBlock calls.
     */
    const float *getBufferedAudio(size_t &length) const;

    /**
     * Clear any buffered downsampled audio data.
     */
    void resetAudioBuffer();

private:
    typedef std::vector<int> i_vec_t;
    typedef std::vector<std::vector<int> > i_mat_t;
    typedef std::vector<double> d_vec_t;
    typedef std::vector<std::vector<double> > d_mat_t;

    void makeDecimators();
    double measureSpecDiff(d_vec_t oldspec, d_vec_t newspec);

    int m_bpb;
    float m_rate;
    size_t m_factor;
    size_t m_increment;
    Decimator *m_decimator1;
    Decimator *m_decimator2;
    float *m_buffer;
    float *m_decbuf;
    size_t m_bufsiz;
    size_t m_buffill;
    size_t m_beatframesize;
    double *m_beatframe;
    FFTReal *m_fft;
    double *m_fftRealOut;
    double *m_fftImagOut;
    d_vec_t m_beatsd;
};

#endif
