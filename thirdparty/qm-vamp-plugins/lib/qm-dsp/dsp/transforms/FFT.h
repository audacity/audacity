/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#ifndef QM_DSP_FFT_H
#define QM_DSP_FFT_H

class FFT  
{
public:
    /**
     * Construct an FFT object to carry out complex-to-complex
     * transforms of size nsamples. nsamples does not have to be a
     * power of two.
     */
    FFT(int nsamples);
    ~FFT();

    /**
     * Carry out a forward or inverse transform (depending on the
     * value of inverse) of size nsamples, where nsamples is the value
     * provided to the constructor above.
     *
     * realIn and (where present) imagIn should contain nsamples each,
     * and realOut and imagOut should point to enough space to receive
     * nsamples each.
     *
     * imagIn may be NULL if the signal is real, but the other
     * pointers must be valid.
     *
     * The inverse transform is scaled by 1/nsamples.
     */
    void process(bool inverse,
                 const double *realIn, const double *imagIn,
                 double *realOut, double *imagOut);
    
private:
    class D;
    D *m_d;
};

class FFTReal
{
public:
    /**
     * Construct an FFT object to carry out real-to-complex transforms
     * of size nsamples. nsamples does not have to be a power of two,
     * but it does have to be even. (Use the complex-complex FFT above
     * if you need an odd FFT size. This constructor will throw
     * std::invalid_argument if nsamples is odd.)
     */
    FFTReal(int nsamples);
    ~FFTReal();

    /**
     * Carry out a forward real-to-complex transform of size nsamples,
     * where nsamples is the value provided to the constructor above.
     *
     * realIn, realOut, and imagOut must point to (enough space for)
     * nsamples values. For consistency with the FFT class above, and
     * compatibility with existing code, the conjugate half of the
     * output is returned even though it is redundant.
     */
    void forward(const double *realIn,
                 double *realOut, double *imagOut);

    /**
     * Carry out a forward real-to-complex transform of size nsamples,
     * where nsamples is the value provided to the constructor
     * above. Return only the magnitudes of the complex output values.
     *
     * realIn and magOut must point to (enough space for) nsamples
     * values. For consistency with the FFT class above, and
     * compatibility with existing code, the conjugate half of the
     * output is returned even though it is redundant.
     */
    void forwardMagnitude(const double *realIn, double *magOut);

    /**
     * Carry out an inverse real transform (i.e. complex-to-real) of
     * size nsamples, where nsamples is the value provided to the
     * constructor above.
     *
     * realIn and imagIn should point to at least nsamples/2+1 values;
     * if more are provided, only the first nsamples/2+1 values of
     * each will be used (the conjugate half will always be deduced
     * from the first nsamples/2+1 rather than being read from the
     * input data).  realOut should point to enough space to receive
     * nsamples values.
     *
     * The inverse transform is scaled by 1/nsamples.
     */
    void inverse(const double *realIn, const double *imagIn,
                 double *realOut);

private:
    class D;
    D *m_d;
};    

#endif
