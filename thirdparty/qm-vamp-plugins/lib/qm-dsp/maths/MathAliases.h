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

#ifndef QM_DSP_MATHALIASES_H
#define QM_DSP_MATHALIASES_H

#include <cmath>
#include <complex>

#define TWO_PI          (2. * M_PI)

#define EPS             2.2204e-016

/* aliases to math.h functions */
#define EXP                             exp
#define COS                             cos
#define SIN                             sin
#define ABS                             fabs
#define POW                             powf
#define SQRT                    sqrtf
#define LOG10                   log10f
#define LOG                             logf
#define FLOOR                   floorf
#define TRUNC                   truncf

typedef std::complex<double> ComplexData;

/* aliases to complex.h functions */
/** sample = EXPC(complex) */
#define EXPC                    cexpf
/** complex = CEXPC(complex) */
#define CEXPC                   cexp
/** sample = ARGC(complex) */
#define ARGC                    cargf
/** sample = ABSC(complex) norm */
#define ABSC                    cabsf
/** sample = REAL(complex) */
#define REAL                    crealf
/** sample = IMAG(complex) */
#define IMAG                    cimagf

#endif
