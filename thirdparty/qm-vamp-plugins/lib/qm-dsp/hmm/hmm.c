/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
 *  hmm.c
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

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>
#include <time.h>               /* to seed random number generator */

#include <clapack.h>            /* LAPACK for matrix inversion */

#include "maths/nan-inf.h"

#ifdef ATLAS_ORDER
#define HAVE_ATLAS 1
#endif

#ifdef HAVE_ATLAS
// Using ATLAS C interface to LAPACK
#define dgetrf_(m, n, a, lda, ipiv, info) \
        clapack_dgetrf(CblasColMajor, *m, *n, a, *lda, ipiv)
#define dgetri_(n, a, lda, ipiv, work, lwork, info) \
        clapack_dgetri(CblasColMajor, *n, a, *lda, ipiv)
#endif

#ifdef _MAC_OS_X
#include <vecLib/cblas.h>
#else
#include <cblas.h>              /* BLAS for matrix multiplication */
#endif

#include "hmm.h"

model_t* hmm_init(double** x, int T, int L, int N)
{
    int i, j, d, e, t;
    double s, ss;
        
    model_t* model;
    model = (model_t*) malloc(sizeof(model_t));
    model->N = N;
    model->L = L;   
    model->p0 = (double*) malloc(N*sizeof(double));
    model->a = (double**) malloc(N*sizeof(double*));
    model->mu = (double**) malloc(N*sizeof(double*));
    for (i = 0; i < N; i++) {
        model->a[i] = (double*) malloc(N*sizeof(double));
        model->mu[i] = (double*) malloc(L*sizeof(double));
    }
    model->cov = (double**) malloc(L*sizeof(double*));
    for (i = 0; i < L; i++) {
        model->cov[i] = (double*) malloc(L*sizeof(double));
    }
        
    srand(time(0));
    double* global_mean = (double*) malloc(L*sizeof(double));
        
    /* find global mean */
    for (d = 0; d < L; d++) {
        global_mean[d] = 0;
        for (t = 0; t < T; t++) {
            global_mean[d] += x[t][d];
        }
        global_mean[d] /= T;
    }
        
    /* calculate global diagonal covariance */
    for (d = 0; d < L; d++) {
        for (e = 0; e < L; e++) {
            model->cov[d][e] = 0;
        }
        for (t = 0; t < T; t++) {
            model->cov[d][d] +=
                (x[t][d] - global_mean[d]) * (x[t][d] - global_mean[d]);
        }
        model->cov[d][d] /= T-1;
    }
        
    /* set all means close to global mean */
    for (i = 0; i < N; i++) {
        for (d = 0; d < L; d++) {
            /* add some random noise related to covariance */
            /* ideally the random number would be Gaussian(0,1),
               as a hack we make it uniform on [-0.25,0.25] */
            model->mu[i][d] = global_mean[d] +
                (0.5 * rand() / (double) RAND_MAX - 0.25)
                * sqrt(model->cov[d][d]);
        }
    }       
        
    /* random initial and transition probs */
    s = 0;
    for (i = 0; i < N; i++) {
        model->p0[i] = 1 + rand() / (double) RAND_MAX;
        s += model->p0[i];
        ss = 0;
        for (j = 0; j < N; j++) {
            model->a[i][j] = 1 + rand() / (double) RAND_MAX;
            ss += model->a[i][j];
        }
        for (j = 0; j < N; j++) {
            model->a[i][j] /= ss;
        }
    }
    for (i = 0; i < N; i++) {
        model->p0[i] /= s;
    }
        
    free(global_mean);
        
    return model;
}

void hmm_close(model_t* model)
{
    int i;
        
    for (i = 0; i < model->N; i++) {
        free(model->a[i]);
        free(model->mu[i]);
    }
    free(model->a);
    free(model->mu);
    for (i = 0; i < model->L; i++) {
        free(model->cov[i]);
    }
    free(model->cov);         
    free(model);    
}

void hmm_train(double** x, int T, model_t* model)
{
    int i, t;
    double loglik;  /* overall log-likelihood at each iteration */
        
    int N = model->N;
    int L = model->L;
    double* p0 = model->p0;
    double** a = model->a;
    double** mu = model->mu;
    double** cov = model->cov;
        
    /* allocate memory */   
    double** gamma = (double**) malloc(T*sizeof(double*));
    double*** xi = (double***) malloc(T*sizeof(double**));
    for (t = 0; t < T; t++) {
        gamma[t] = (double*) malloc(N*sizeof(double));
        xi[t] = (double**) malloc(N*sizeof(double*));
        for (i = 0; i < N; i++) {
            xi[t][i] = (double*) malloc(N*sizeof(double));
        }
    }
        
    /* temporary memory */
    double* gauss_y = (double*) malloc(L*sizeof(double));
    double* gauss_z = (double*) malloc(L*sizeof(double));   
                        
    /* obs probs P(j|{x}) */
    double** b = (double**) malloc(T*sizeof(double*));
    for (t = 0; t < T; t++) {
        b[t] = (double*) malloc(N*sizeof(double));
    }
        
    /* inverse covariance and its determinant */
    double** icov = (double**) malloc(L*sizeof(double*));
    for (i = 0; i < L; i++) {
        icov[i] = (double*) malloc(L*sizeof(double));
    }
    double detcov;
        
    double thresh = 0.0001;
    int niter = 50; 
    int iter = 0;
    double loglik1, loglik2;
    int foundnan = 0;

    while (iter < niter && !foundnan &&
           !(iter > 1 && (loglik - loglik1) < thresh * (loglik1 - loglik2))) {
        
        ++iter;

        /* precalculate obs probs */
        invert(cov, L, icov, &detcov);
                
        for (t = 0; t < T; t++) {
            for (i = 0; i < N; i++) {
                b[t][i] = exp(loggauss(x[t], L, mu[i], icov, detcov, gauss_y, gauss_z));
            }
        }
        forward_backwards(xi, gamma, &loglik, &loglik1, &loglik2,
                          iter, N, T, p0, a, b);
        if (ISNAN(loglik)) {
            foundnan = 1;
            continue;
        }
                
        baum_welch(p0, a, mu, cov, N, T, L, x, xi, gamma);
    }
        
    /* deallocate memory */
    for (t = 0; t < T; t++) {
        free(gamma[t]);
        free(b[t]);
        for (i = 0; i < N; i++) {
            free(xi[t][i]);
        }
        free(xi[t]);
    }
    free(gamma);
    free(xi);
    free(b);        
        
    for (i = 0; i < L; i++) {
        free(icov[i]);
    }
    free(icov);
        
    free(gauss_y);
    free(gauss_z);
}

void baum_welch(double* p0, double** a, double** mu, double** cov,
                int N, int T, int L, double** x, double*** xi, double** gamma)
{
    int i, j, t;
        
    double* sum_gamma = (double*) malloc(N*sizeof(double));
        
    /* temporary memory */
    double* u = (double*) malloc(L*L*sizeof(double));
    double* yy = (double*) malloc(T*L*sizeof(double));
    double* yy2 = (double*) malloc(T*L*sizeof(double));     
        
    /* re-estimate transition probs */
    for (i = 0; i < N; i++) {
        sum_gamma[i] = 0;
        for (t = 0; t < T-1; t++) {
            sum_gamma[i] += gamma[t][i];
        }
    }
        
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            a[i][j] = 0;
            if (sum_gamma[i] == 0.) {
                continue;
            }
            for (t = 0; t < T-1; t++) {
                a[i][j] += xi[t][i][j];
            }
            a[i][j] /= sum_gamma[i];        
        }
    }
        
    /* NB: now we need to sum gamma over all t */
    for (i = 0; i < N; i++) {
        sum_gamma[i] += gamma[T-1][i];
    }
        
    /* re-estimate initial probs */
    for (i = 0; i < N; i++) {
        p0[i] = gamma[0][i];
    }
        
    /* re-estimate covariance */
    int d, e;
    double sum_sum_gamma = 0;
    for (i = 0; i < N; i++) {
        sum_sum_gamma += sum_gamma[i];
    }
        
    /* using BLAS */
    for (d = 0; d < L; d++) {
        for (e = 0; e < L; e++) {
            cov[d][e] = 0;
        }
    }
        
    for (j = 0; j < N; j++) {
        
        for (d = 0; d < L; d++) {
            for (t = 0; t < T; t++) {
                yy[d*T+t] = x[t][d] - mu[j][d];
                yy2[d*T+t] = gamma[t][j] * (x[t][d] - mu[j][d]);
            }
        }
                                
        cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans,
                    L, L, T, 1.0, yy, T, yy2, T, 0, u, L);
                
        for (e = 0; e < L; e++) {
            for (d = 0; d < L; d++) {
                cov[d][e] += u[e*L+d];
            }
        }
    }
        
    for (d = 0; d < L; d++) {
        for (e = 0; e < L; e++) {
            cov[d][e] /= T; /* sum_sum_gamma; */
        }
    }
    
    //printf("sum_sum_gamma = %f\n", sum_sum_gamma); /* fine, = T IS THIS ALWAYS TRUE with pooled cov?? */
        
    /* re-estimate means */
    for (j = 0; j < N; j++) {
        for (d = 0; d < L; d++) {
            mu[j][d] = 0;
            for (t = 0; t < T; t++)
                mu[j][d] += gamma[t][j] * x[t][d];
            mu[j][d] /= sum_gamma[j];
        }
    }
        
    /* deallocate memory */
    free(sum_gamma);
    free(yy);
    free(yy2);
    free(u);
}

void forward_backwards(double*** xi, double** gamma,
                       double* loglik, double* loglik1, double* loglik2,
                       int iter, int N, int T,
                       double* p0, double** a, double** b)
{
    /* forwards-backwards with scaling */
    int i, j, t;
        
    double** alpha = (double**) malloc(T*sizeof(double*));
    double** beta = (double**) malloc(T*sizeof(double*));
    for (t = 0; t < T; t++) {
        alpha[t] = (double*) malloc(N*sizeof(double));
        beta[t] = (double*) malloc(N*sizeof(double));
    }
        
    /* scaling coefficients */
    double* c = (double*) malloc(T*sizeof(double));
        
    /* calculate forward probs and scale coefficients */
    c[0] = 0;
    for (i = 0; i < N; i++) {
        alpha[0][i] = p0[i] * b[0][i];
        c[0] += alpha[0][i];
    }
    c[0] = 1 / c[0];
    for (i = 0; i < N; i++) {
        alpha[0][i] *= c[0];            
    }
        
    *loglik1 = *loglik;
    *loglik = -log(c[0]);
    if (iter == 2) {
        *loglik2 = *loglik;
    }
        
    for (t = 1; t < T; t++) {
        
        c[t] = 0;

        for (j = 0; j < N; j++) {
            alpha[t][j] = 0;
            for (i = 0; i < N; i++) {
                alpha[t][j] += alpha[t-1][i] * a[i][j];
            }
            alpha[t][j] *= b[t][j];
                        
            c[t] += alpha[t][j];
        }
                
        c[t] = 1 / c[t];
        for (j = 0; j < N; j++) {
            alpha[t][j] *= c[t];
        }
                
        *loglik -= log(c[t]);
    }
        
    /* calculate backwards probs using same coefficients */
    for (i = 0; i < N; i++) {
        beta[T-1][i] = 1;
    }
    t = T - 1;

    while (1) {
        for (i = 0; i < N; i++) {
            beta[t][i] *= c[t];
        }
                
        if (t == 0) {
            break;
        }
                
        for (i = 0; i < N; i++) {
            beta[t-1][i] = 0;
            for (j = 0; j < N; j++) {
                beta[t-1][i] += a[i][j] * b[t][j] * beta[t][j];
            }
        }
                
        t--;
    }
	
    /* calculate posterior probs */
    double tot;
    for (t = 0; t < T; t++) {
        tot = 0;
        for (i = 0; i < N; i++) {
            gamma[t][i] = alpha[t][i] * beta[t][i];
            tot += gamma[t][i];
        }
        for (i = 0; i < N; i++) {
            gamma[t][i] /= tot;				
        }
    }		
	
    for (t = 0; t < T-1; t++) {
        tot = 0;
        for (i = 0; i < N; i++) {
            for (j = 0; j < N; j++) {
                xi[t][i][j] = alpha[t][i] * a[i][j] * b[t+1][j] * beta[t+1][j];
                tot += xi[t][i][j];
            }
        }
        for (i = 0; i < N; i++) {
            for (j = 0; j < N; j++) {
                xi[t][i][j] /= tot;
            }
        }
    }
	
    for (t = 0; t < T; t++) {
        free(alpha[t]);
        free(beta[t]);
    }
    free(alpha);
    free(beta);
    free(c);
}

void viterbi_decode(double** x, int T, model_t* model, int* q)
{
    int i, j, t;
    double max;
    int havemax;
	
    int N = model->N;
    int L = model->L;
    double* p0 = model->p0;
    double** a = model->a;
    double** mu = model->mu;
    double** cov = model->cov;
	
    /* inverse covariance and its determinant */
    double** icov = (double**) malloc(L*sizeof(double*));
    for (i = 0; i < L; i++) {
        icov[i] = (double*) malloc(L*sizeof(double));
    }
    double detcov;
	
    double** logb = (double**) malloc(T*sizeof(double*));
    double** phi = (double**) malloc(T*sizeof(double*));
    int** psi = (int**) malloc(T*sizeof(int*));

    for (t = 0; t < T; t++) {
        logb[t] = (double*) malloc(N*sizeof(double));
        phi[t] = (double*) malloc(N*sizeof(double));
        psi[t] = (int*) malloc(N*sizeof(int));
    }
	
    /* temporary memory */
    double* gauss_y = (double*) malloc(L*sizeof(double));
    double* gauss_z = (double*) malloc(L*sizeof(double));	
	
    /* calculate observation logprobs */
    invert(cov, L, icov, &detcov);
    for (t = 0; t < T; t++) {
        for (i = 0; i < N; i++) {
            logb[t][i] = loggauss
                (x[t], L, mu[i], icov, detcov, gauss_y, gauss_z);
        }
    }
	
    /* initialise */
    for (i = 0; i < N; i++) {
        phi[0][i] = log(p0[i]) + logb[0][i];
        psi[0][i] = 0;
    }
	
    for (t = 1; t < T; t++) {
        for (j = 0; j < N; j++) {
            max = -1000000;
            havemax = 0;

            psi[t][j] = 0;
            for (i = 0; i < N; i++) {
                if (phi[t-1][i] + log(a[i][j]) > max || !havemax) {
                    max = phi[t-1][i] + log(a[i][j]);
                    phi[t][j] = max + logb[t][j];
                    psi[t][j] = i;
                    havemax = 1;
                }
            }
        }
    }
	
    /* find maximising state at time T-1 */
    max = phi[T-1][0];
    q[T-1] = 0;
    for (i = 1; i < N; i++) {
        if (phi[T-1][i] > max) {
            max = phi[T-1][i];
            q[T-1] = i;
        }
    }
	
    /* track back */
    t = T - 2;
    while (t >= 0) {
        q[t] = psi[t+1][q[t+1]];
        t--;
    }
	
    /* de-allocate memory */
    for (i = 0; i < L; i++) {
        free(icov[i]);
    }
    free(icov);
    for (t = 0; t < T; t++) {
        free(logb[t]);
        free(phi[t]);
        free(psi[t]);
    }
    free(logb);
    free(phi);
    free(psi);
	
    free(gauss_y);
    free(gauss_z);
}

/* invert matrix and calculate determinant using LAPACK */
void invert(double** cov, int L, double** icov, double* detcov)
{
    /* copy square matrix into a vector in column-major order */
    double* a = (double*) malloc(L*L*sizeof(double));
    int i, j;
    for (j=0; j < L; j++) {
        for (i=0; i < L; i++) {
            a[j*L+i] = cov[i][j];
        }
    }
	
    int M = (int) L;	
    int* ipiv = (int *) malloc(L*L*sizeof(int));
    int ret;
	
    /* LU decomposition */
    ret = dgetrf_(&M, &M, a, &M, ipiv, &ret);	/* ret should be zero, negative if cov is singular */
    if (ret < 0) {
        fprintf(stderr, "Covariance matrix was singular, couldn't invert\n");
        exit(-1);
    }
	
    /* find determinant */
    double det = 1;
    for (i = 0; i < L; i++) {
        det *= a[i*L+i];
    }

    // TODO: get this to work!!! If detcov < 0 then cov is bad anyway...
    if (det < 0) {
        det = -det;
    }
    *detcov = det;
	
    /* allocate required working storage */
#ifndef HAVE_ATLAS
    int lwork = -1;
    double lwbest = 0.0;
    dgetri_(&M, a, &M, ipiv, &lwbest, &lwork, &ret);
    lwork = (int) lwbest;	
    double* work  = (double*) malloc(lwork*sizeof(double));
#endif
	
    /* find inverse */
    dgetri_(&M, a, &M, ipiv, work, &lwork, &ret);

    for (j=0; j < L; j++) {
        for (i=0; i < L; i++) {
            icov[i][j] = a[j*L+i];
        }
    }
	
#ifndef HAVE_ATLAS	
    free(work);
#endif
    free(a);	
}

/* probability of multivariate Gaussian given mean, inverse and determinant of covariance */
double gauss(double* x, int L, double* mu, double** icov, double detcov, double* y, double* z)
{
    int i;
    double s = 0;

    for (i = 0; i < L; i++) {
        y[i] = x[i] - mu[i];
    }

    for (i = 0; i < L; i++) {
        z[i] = cblas_ddot(L, &icov[i][0], 1, y, 1);
    }

    s = cblas_ddot(L, z, 1, y, 1);
	
    return exp(-s/2.0) / (pow(2 * M_PI, L/2.0) * sqrt(detcov));
}

/* log probability of multivariate Gaussian given mean, inverse and determinant of covariance */
double loggauss(double* x, int L, double* mu, double** icov, double detcov, double* y, double* z)
{
    int i;
    double s = 0;
    double ret;

    for (i = 0; i < L; i++) {
        y[i] = x[i] - mu[i];
    }

    for (i = 0; i < L; i++) {
        z[i] = cblas_ddot(L, &icov[i][0], 1, y, 1);
    }

    s = cblas_ddot(L, z, 1, y, 1);
	
    ret = -0.5 * (s + L * log(2 * M_PI) + log(detcov));
	
    return ret;
}

void hmm_print(model_t* model)
{
    int i, j;
    printf("p0:\n");
    for (i = 0; i < model->N; i++) {
        printf("%f ", model->p0[i]);
    }
    printf("\n\n");
    printf("a:\n");
    for (i = 0; i < model->N; i++) {
        for (j = 0; j < model->N; j++) {
            printf("%f ", model->a[i][j]);
        }
        printf("\n");
    }
    printf("\n\n");
    printf("mu:\n");
    for (i = 0; i < model->N; i++) {
        for (j = 0; j < model->L; j++) {
            printf("%f ", model->mu[i][j]);
        }
        printf("\n");
    }
    printf("\n\n");
    printf("cov:\n");
    for (i = 0; i < model->L; i++) {
        for (j = 0; j < model->L; j++) {
            printf("%f ", model->cov[i][j]);
        }
        printf("\n");
    }
    printf("\n\n");
}


