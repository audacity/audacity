/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
 *  hmm.h
 *
 *  Created by Mark Levy on 12/02/2006.
 *  Copyright 2006 Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
 *
 */

#ifndef QM_DSP_HMM_H
#define QM_DSP_HMM_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _model_t {
    int N;          /* number of states */
    double* p0;     /* initial probs */
    double** a;     /* transition probs */
    int L;          /* dimensionality of data */
    double** mu;    /* state means */
    double** cov;   /* covariance, tied between all states */
} model_t;

void hmm_train(double** x, int T, model_t* model); /* with scaling */

void forward_backwards(double*** xi, double** gamma,
                       double* loglik, double* loglik1, double* loglik2,
                       int iter, int N, int T,
                       double* p0, double** a, double** b);
    
void baum_welch(double* p0, double** a, double** mu, double** cov,
                int N, int T, int L, double** x, double*** xi, double** gamma);

void viterbi_decode(double** x, int T, model_t* model, int* q); /* using logs */

model_t* hmm_init(double** x, int T, int L, int N);
void hmm_close(model_t* model);
    
void invert(double** cov, int L, double** icov, double* detcov); /* uses LAPACK */
    
double gauss(double* x, int L, double* mu, double** icov,
             double detcov, double* y, double* z);
    
double loggauss(double* x, int L, double* mu, double** icov,
                double detcov, double* y, double* z);
    
void hmm_print(model_t* model);

#ifdef __cplusplus
}
#endif

#endif

