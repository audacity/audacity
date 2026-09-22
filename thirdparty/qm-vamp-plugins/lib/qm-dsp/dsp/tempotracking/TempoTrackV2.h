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

#ifndef QM_DSP_TEMPOTRACKV2_H
#define QM_DSP_TEMPOTRACKV2_H

#include <vector>

//!!! Question: how far is this actually sample rate dependent?  I
// think it does produce plausible results for e.g. 48000 as well as
// 44100, but surely the fixed window sizes and comb filtering will
// make it prefer double or half time when run at e.g. 96000?

class TempoTrackV2
{
public:
    /**
     * Construct a tempo tracker that will operate on beat detection
     * function data calculated from audio at the given sample rate
     * with the given frame increment.
     *
     * Currently the sample rate and increment are used only for the
     * conversion from beat frame location to bpm in the tempo array.
     */
    TempoTrackV2(float sampleRate, int dfIncrement);
    ~TempoTrackV2();

    // Returned beat periods are given in df increment units; inputtempo and tempi in bpm
    void calculateBeatPeriod(const std::vector<double> &df,
                             std::vector<double> &beatPeriod,
                             std::vector<double> &tempi) {
        calculateBeatPeriod(df, beatPeriod, tempi, 120.0, false);
    }

    // Returned beat periods are given in df increment units; inputtempo and tempi in bpm
    // MEPD 28/11/12 Expose inputtempo and constraintempo parameters
    // Note, if inputtempo = 120 and constraintempo = false, then functionality is as it was before
    void calculateBeatPeriod(const std::vector<double> &df,
                             std::vector<double> &beatPeriod,
                             std::vector<double> &tempi,
                             double inputtempo, bool constraintempo);

    // Returned beat positions are given in df increment units
    void calculateBeats(const std::vector<double> &df,
                        const std::vector<double> &beatPeriod,
                        std::vector<double> &beats) {
        calculateBeats(df, beatPeriod, beats, 0.9, 4.0);
    }

    // Returned beat positions are given in df increment units
    // MEPD 28/11/12 Expose alpha and tightness parameters
    // Note, if alpha = 0.9 and tightness = 4, then functionality is as it was before
    void calculateBeats(const std::vector<double> &df,
                        const std::vector<double> &beatPeriod,
                        std::vector<double> &beats,
                        double alpha, double tightness);

private:
    typedef std::vector<int> i_vec_t;
    typedef std::vector<std::vector<int> > i_mat_t;
    typedef std::vector<double> d_vec_t;
    typedef std::vector<std::vector<double> > d_mat_t;

    float m_rate;
    int m_increment;

    void adapt_thresh(d_vec_t &df);
    double mean_array(const d_vec_t &dfin, int start, int end);
    void filter_df(d_vec_t &df);
    void get_rcf(const d_vec_t &dfframe, const d_vec_t &wv, d_vec_t &rcf);
    void viterbi_decode(const d_mat_t &rcfmat, const d_vec_t &wv,
                        d_vec_t &bp, d_vec_t &tempi);
    double get_max_val(const d_vec_t &df);
    int get_max_ind(const d_vec_t &df);
    void normalise_vec(d_vec_t &df);
};

#endif
