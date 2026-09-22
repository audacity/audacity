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

#ifndef MATHUTILITIES_H
#define MATHUTILITIES_H

#include <vector>

#include "nan-inf.h"

/**
 * Static helper functions for simple mathematical calculations.
 */
class MathUtilities  
{
public: 
    /**
     * Round x to the nearest integer.
     */
    static double round( double x );

    /**
     * Return through min and max pointers the highest and lowest
     * values in the given array of the given length.
     */
    static void getFrameMinMax( const double* data, int len,
                                double* min, double* max );

    /**
     * Return the mean of the given array of the given length.
     */
    static double mean( const double* src, int len );

    /**
     * Return the mean of the subset of the given vector identified by
     * start and count.
     */
    static double mean( const std::vector<double> &data,
                        int start, int count );
    
    /**
     * Return the sum of the values in the given array of the given
     * length.
     */
    static double sum( const double* src, int len );

    /**
     * Return the median of the values in the given array of the given
     * length. If the array is even in length, the returned value will
     * be half-way between the two values adjacent to median.
     */
    static double median( const double* src, int len );

    /**
     * The principle argument function. Map the phase angle ang into
     * the range [-pi,pi).
     */
    static double princarg( double ang );

    /**
     * Floating-point division modulus: return x % y.
     */
    static double mod( double x, double y);

    /**
     * The alpha norm is the alpha'th root of the mean alpha'th power
     * magnitude. For example if alpha = 2 this corresponds to the RMS
     * of the input data, and when alpha = 1 this is the mean
     * magnitude.
     */
    static void getAlphaNorm(const double *data, int len, int alpha, double* ANorm);

    /**
     * The alpha norm is the alpha'th root of the mean alpha'th power
     * magnitude. For example if alpha = 2 this corresponds to the RMS
     * of the input data, and when alpha = 1 this is the mean
     * magnitude.
     */
    static double getAlphaNorm(const std::vector <double> &data, int alpha );

    enum NormaliseType {
        NormaliseNone,
        NormaliseUnitSum,
        NormaliseUnitMax
    };

    static void normalise(double *data, int length,
                          NormaliseType n = NormaliseUnitMax);

    static void normalise(std::vector<double> &data,
                          NormaliseType n = NormaliseUnitMax);

    /**
     * Calculate the L^p norm of a vector. Equivalent to MATLAB's
     * norm(data, p).
     */
    static double getLpNorm(const std::vector<double> &data,
                            int p);

    /**
     * Normalise a vector by dividing through by its L^p norm. If the
     * norm is below the given threshold, the unit vector for that
     * norm is returned. p may be 0, in which case no normalisation
     * happens and the data is returned unchanged.
     */
    static std::vector<double> normaliseLp(const std::vector<double> &data,
                                           int p,
                                           double threshold = 1e-6);
    
    /**
     * Threshold the input/output vector data against a moving-mean
     * average filter.
     */
    static void adaptiveThreshold(std::vector<double> &data);

    static void circShift( double* data, int length, int shift);

    static int getMax( double* data, int length, double* max = 0 );
    static int getMax( const std::vector<double> &data, double* max = 0 );
    static int compareInt(const void * a, const void * b);

    /** 
     * Return true if x is 2^n for some integer n >= 0.
     */
    static bool isPowerOfTwo(int x);

    /**
     * Return the next higher integer power of two from x, e.g. 1300
     * -> 2048, 2048 -> 2048.
     */
    static int nextPowerOfTwo(int x);

    /**
     * Return the next lower integer power of two from x, e.g. 1300 ->
     * 1024, 2048 -> 2048.
     */
    static int previousPowerOfTwo(int x);

    /**
     * Return the nearest integer power of two to x, e.g. 1300 -> 1024,
     * 12 -> 16 (not 8; if two are equidistant, the higher is returned).
     */
    static int nearestPowerOfTwo(int x);

    /**
     * Return x!
     */
    static double factorial(int x); // returns double in case it is large

    /**
     * Return the greatest common divisor of natural numbers a and b.
     */
    static int gcd(int a, int b);
};

#endif
