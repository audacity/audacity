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

#ifndef _ADAPTIVE_SPECTROGRAM_H_
#define _ADAPTIVE_SPECTROGRAM_H_

#include <vamp-sdk/Plugin.h>
#include <cmath>
#include <vector>

#include <dsp/transforms/FFT.h>
#include <base/Window.h>

#include <thread/Thread.h>
#include <thread/AsynchronousTask.h>
#include <thread/BlockAllocator.h>

class Decimator;

class AdaptiveSpectrogram : public Vamp::Plugin
{
public:
    AdaptiveSpectrogram(float inputSampleRate);
    virtual ~AdaptiveSpectrogram();

    bool initialise(size_t channels, size_t stepSize, size_t blockSize);
    void reset();

    InputDomain getInputDomain() const { return TimeDomain; }

    std::string getIdentifier() const;
    std::string getName() const;
    std::string getDescription() const;
    std::string getMaker() const;
    int getPluginVersion() const;
    std::string getCopyright() const;

    size_t getPreferredStepSize() const;
    size_t getPreferredBlockSize() const;

    ParameterList getParameterDescriptors() const;
    float getParameter(std::string id) const;
    void setParameter(std::string id, float value);

    OutputList getOutputDescriptors() const;

    FeatureSet process(const float *const *inputBuffers,
                       Vamp::RealTime timestamp);

    FeatureSet getRemainingFeatures();

protected:
    int m_w;
    int m_n;
    bool m_coarse;
    bool m_threaded;
    int m_decFactor;
    float *m_buffer;
    int m_buflen;
    Decimator *m_decimator;

    struct Spectrogram
    {
        int resolution;
        int width;
        double **data;

        Spectrogram(int r, int w) :
            resolution(r), width(w) {
            data = new double *[width];
            for (int i = 0; i < width; ++i) data[i] = new double[resolution];
        }

        ~Spectrogram() {
            for (int i = 0; i < width; ++i) delete[] data[i];
            delete[] data;
        }            
    };

    struct Spectrograms
    {
        int minres;
        int maxres;
        int n;
        Spectrogram **spectrograms;

        Spectrograms(int mn, int mx, int widthofmax) :
            minres(mn), maxres(mx) {
            n = log2(maxres/minres) + 1;
            spectrograms = new Spectrogram *[n];
            int r = mn;
            for (int i = 0; i < n; ++i) {
                spectrograms[i] = new Spectrogram(r, widthofmax * (mx / r));
                r = r * 2;
            }
        }
        ~Spectrograms() {
            for (int i = 0; i < n; ++i) {
                delete spectrograms[i];
            }
            delete[] spectrograms;
        }
    };

    struct Cutting
    {
        enum Cut { Horizontal, Vertical, Finished };
        Cut cut;
        Cutting *first;
        Cutting *second;
        double cost;
        double value;
        BlockAllocator *allocator;

        ~Cutting() {
            if (first) first->erase();
            if (second) second->erase();
        }

        void erase() {
            if (allocator) {
                if (first) first->erase();
                if (second) second->erase();
                allocator->deallocate(this);
            } else {
                delete this;
            }
        }
    };

    class FFTThread : public AsynchronousTask
    {
    public:
        FFTThread(int w) :
            m_window(HanningWindow, w) {
            m_w = w;
            m_fft = new FFTReal(m_w);
            m_rin = new double[m_w];
            m_rout = new double[m_w];
            m_iout = new double[m_w];
        }
        ~FFTThread() {
            delete[] m_rin;
            delete[] m_rout;
            delete[] m_iout;
            delete m_fft;
        }

        int getW() const { return m_w; }

        void startCalculation(const float *timeDomain, Spectrograms &s,
                              int res, int maxwidth) {
            setParameters(timeDomain, s, res, maxwidth);
            startTask();
        }

        void await() {
            awaitTask();
        }

        void setParameters(const float *timeDomain, Spectrograms &s,
                           int res, int maxwidth) {
            m_in = timeDomain;
            m_s = &s;
            m_res = res;
            m_maxwid = maxwidth;
        }

        void performTask() {
            for (int i = 0; i < m_maxwid / m_w; ++i) {
                int origin = m_maxwid/4 - m_w/4; // for 50% overlap
                for (int j = 0; j < m_w; ++j) {
                    m_rin[j] = m_in[origin + i * m_w/2 + j];
                }
                m_window.cut(m_rin);
                m_fft->forward(m_rin, m_rout, m_iout);
                for (int j = 0; j < m_w/2; ++j) {
                    int k = j+1; // include Nyquist but not DC
                    double mag = sqrt(m_rout[k] * m_rout[k] +
                                      m_iout[k] * m_iout[k]);
                    double scaled = mag / (m_w/2);
                    m_s->spectrograms[m_res]->data[i][j] = scaled;
                }
            }
        }

    private:
        Window<double> m_window;
        FFTReal *m_fft;
        const float *m_in;
        double *m_rin;
        double *m_rout;
        double *m_iout;
        Spectrograms *m_s;
        int m_res;
        int m_w;
        int m_maxwid;
    };

    typedef std::map<int, FFTThread *> FFTMap;
    FFTMap m_fftThreads;

    class CutThread : public AsynchronousTask
    {
    public:
        CutThread(const AdaptiveSpectrogram *as) : m_as(as), m_result(0) {
            m_allocator = new BlockAllocator(sizeof(Cutting));
        }
        ~CutThread() {
            delete m_allocator;
        }
        
        void cut(const Spectrograms &s, int res, int x, int y, int h) {
            m_s = &s;
            m_res = res;
            m_x = x;
            m_y = y;
            m_h = h;
            startTask();
        }

        Cutting *get() {
            awaitTask();
            return m_result;
        }

    protected:
        void performTask() {
            m_result = m_as->cut(*m_s, m_res, m_x, m_y, m_h, m_allocator);
        }

    private:
        const AdaptiveSpectrogram *m_as;
        BlockAllocator *m_allocator;
        const Spectrograms *m_s;
        int m_res;
        int m_x;
        int m_y;
        int m_h;
        Cutting *m_result;
    };

    mutable std::vector<CutThread *> m_cutThreads;
    mutable bool m_threadsInUse;

    inline double xlogx(double x) const {
        if (x == 0.0) return 0.0;
        else return x * log(x);
    }

    inline double cost(const Spectrogram &s, int x, int y) const {
        return xlogx(s.data[x][y]);
    }

    inline double value(const Spectrogram &s, int x, int y) const {
        return s.data[x][y];
    }

    inline double normalize(double vcost, double venergy) const {
        return (vcost + (venergy * log(venergy))) / venergy;
    }

    inline bool isResolutionWanted(const Spectrograms &s, int res) const {
        if (!m_coarse) return true;
        if (res == s.minres || res == s.maxres) return true;
        int n = 0;
        for (int r = res; r > s.minres; r >>= 1) ++n;
        return ((n & 0x1) == 0);
    }

    Cutting *cut(const Spectrograms &, int res, int x, int y, int h,
                 BlockAllocator *allocator) const;

    void getSubCuts(const Spectrograms &, int res, int x, int y, int h,
                    Cutting **top, Cutting **bottom,
                    Cutting **left, Cutting **right,
                    BlockAllocator *allocator) const;

    void printCutting(Cutting *, std::string) const;

    void assemble(const Spectrograms &, const Cutting *,
                  std::vector<std::vector<float> > &,
                  int x, int y, int w, int h) const;
};


#endif
