/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP library
    Centre for Digital Music, Queen Mary, University of London.
 
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_KAISER_WINDOW_H
#define QM_DSP_KAISER_WINDOW_H

#include <vector>
#include <cmath>

/**
 * Kaiser window: A windower whose bandwidth and sidelobe height
 * (signal-noise ratio) can be specified. These parameters are traded
 * off against the window length.
 */
class KaiserWindow
{
public:
    struct Parameters {
        int length;
        double beta;
    };

    /**
     * Construct a Kaiser windower with the given length and beta
     * parameter.
     */
    KaiserWindow(Parameters p) : m_length(p.length), m_beta(p.beta) { init(); }

    /**
     * Construct a Kaiser windower with the given attenuation in dB
     * and transition width in samples.
     */
    static KaiserWindow byTransitionWidth(double attenuation,
                                          double transition) {
        return KaiserWindow
            (parametersForTransitionWidth(attenuation, transition));
    }

    /**
     * Construct a Kaiser windower with the given attenuation in dB
     * and transition bandwidth in Hz for the given samplerate.
     */
    static KaiserWindow byBandwidth(double attenuation,
                                    double bandwidth,
                                    double samplerate) {
        return KaiserWindow
            (parametersForBandwidth(attenuation, bandwidth, samplerate));
    }

    /**
     * Obtain the parameters necessary for a Kaiser window of the
     * given attenuation in dB and transition width in samples.
     */
    static Parameters parametersForTransitionWidth(double attenuation,
                                                   double transition);

    /**
     * Obtain the parameters necessary for a Kaiser window of the
     * given attenuation in dB and transition bandwidth in Hz for the
     * given samplerate.
     */
    static Parameters parametersForBandwidth(double attenuation,
                                             double bandwidth,
                                             double samplerate) {
        return parametersForTransitionWidth
            (attenuation, (bandwidth * 2 * M_PI) / samplerate);
    } 

    int getLength() const {
        return m_length;
    }

    const double *getWindow() const { 
        return m_window.data();
    }

    void cut(double *src) const { 
        cut(src, src); 
    }

    void cut(const double *src, double *dst) const {
        for (int i = 0; i < m_length; ++i) {
            dst[i] = src[i] * m_window[i];
        }
    }

private:
    int m_length;
    double m_beta;
    std::vector<double> m_window;

    void init();
};

#endif
