/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
 *  cluster.c
 *  cluster_melt
 *
 *  Created by Mark Levy on 21/02/2006.
 *  Copyright 2006 Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
 *
 */

#include <stdlib.h>

#include "cluster_melt.h"

#define DEFAULT_LAMBDA 0.02;
#define DEFAULT_LIMIT 20;

double kldist(double* a, double* b, int n) {
    /* NB assume that all a[i], b[i] are non-negative
       because a, b represent probability distributions */
    double q, d;
    int i;
        
    d = 0;
    for (i = 0; i < n; i++) {
        q = (a[i] + b[i]) / 2.0;
        if (q > 0) {
            if (a[i] > 0) {
                d += a[i] * log(a[i] / q);
            }
            if (b[i] > 0) {
                d += b[i] * log(b[i] / q);
            }
        }
    }
    return d;               
}       

void cluster_melt(double *h, int m, int n, double *Bsched, int t, int k, int l, int *c) {
    double lambda, sum, beta, logsumexp, maxlp;
    int i, j, a, b, b0, b1, limit, /* B, */ it, maxiter, maxiter0, maxiter1;
    double** cl;    /* reference histograms for each cluster */
    int** nc;       /* neighbour counts for each histogram */
    double** lp;    /* soft assignment probs for each histogram */
    int* oldc;      /* previous hard assignments (to check convergence) */
        
    /* NB h is passed as a 1d row major array */
        
    /* parameter values */
    lambda = DEFAULT_LAMBDA;
    if (l > 0) {
        limit = l;
    } else {
        limit = DEFAULT_LIMIT;          /* use default if no valid neighbourhood limit supplied */
    }

    maxiter0 = 20;  /* number of iterations at initial temperature */
    maxiter1 = 5;   /* number of iterations at subsequent temperatures */
        
    /* allocate memory */   
    cl = (double**) malloc(k*sizeof(double*));
    for (i= 0; i < k; i++) {
        cl[i] = (double*) malloc(m*sizeof(double));
    }
        
    nc = (int**) malloc(n*sizeof(int*));
    for (i= 0; i < n; i++) {
        nc[i] = (int*) malloc(k*sizeof(int));
    }
        
    lp = (double**) malloc(n*sizeof(double*));
    for (i= 0; i < n; i++) {
        lp[i] = (double*) malloc(k*sizeof(double));
    }
        
    oldc = (int*) malloc(n * sizeof(int));
        
    /* initialise */
    for (i = 0; i < k; i++) {
        sum = 0;
        for (j = 0; j < m; j++) {
            cl[i][j] = rand();      /* random initial reference histograms */
            sum += cl[i][j] * cl[i][j];
        }
        sum = sqrt(sum);
        for (j = 0; j < m; j++) {
            cl[i][j] /= sum;        /* normalise */
        }
    }       
        
    for (i = 0; i < n; i++) {
        c[i] = 1;       /* initially assign all histograms to cluster 1 */
    }
        
    for (a = 0; a < t; a++) {
        
        beta = Bsched[a];
                
        if (a == 0) {
            maxiter = maxiter0;
        } else {
            maxiter = maxiter1;
        }
                
        for (it = 0; it < maxiter; it++) {

            //if (it == maxiter - 1)
            //      mexPrintf("hasn't converged after %d iterations\n", maxiter);
                        
            for (i = 0; i < n; i++) {

                /* save current hard assignments */
                oldc[i] = c[i];
                                
                /* calculate soft assignment logprobs for each cluster */
                sum = 0;

                for (j = 0; j < k; j++) {

                    lp[i][ j] = -beta * kldist(cl[j], &h[i*m], m);
                                        
                    /* update matching neighbour counts for this histogram, based on current hard assignments */

                    b0 = i - limit;
                    if (b0 < 0) {
                        b0 = 0;
                    }
                    b1 = i + limit;
                    if (b1 >= n) {
                        b1 = n - 1;
                    }
                    nc[i][j] = b1 - b0 + 1;         /* = B except at edges */
                    for (b = b0; b <= b1; b++) {
                        if (c[b] == j+1) {
                            nc[i][j]--;
                        }
                    }
                                        
                    sum += exp(lp[i][j]);
                }
                                
                /* normalise responsibilities and add duration logprior */
                logsumexp = log(sum);
                for (j = 0; j < k; j++) {
                    lp[i][j] -= logsumexp + lambda * nc[i][j];
                }
            }
                        
            /* update the assignments now that we know the duration priors
               based on the current assignments */
            for (i = 0; i < n; i++) {
                maxlp = lp[i][0];
                c[i] = 1;
                for (j = 1; j < k; j++) {
                    if (lp[i][j] > maxlp) {
                        maxlp = lp[i][j];
                        c[i] = j+1;
                    }
                }
            }
                                
            /* break if assignments haven't changed */
            i = 0;
            while (i < n && oldc[i] == c[i]) {
                i++;
            }
            if (i == n) {
                break;
            }
                        
            /* update reference histograms now we know new responsibilities */
            for (j = 0; j < k; j++) {
                for (b = 0; b < m; b++) {
                    cl[j][b] = 0;
                    for (i = 0; i < n; i++) {
                        cl[j][b] += exp(lp[i][j]) * h[i*m+b];
                    }       
                }
                                
                sum = 0;                                
                for (i = 0; i < n; i++) {
                    sum += exp(lp[i][j]);
                }
                for (b = 0; b < m; b++) {
                    cl[j][b] /= sum;        /* normalise */
                }
            }       
        }
    }
                
    /* free memory */
    for (i = 0; i < k; i++) {
        free(cl[i]);
    }
    free(cl);
    for (i = 0; i < n; i++) {
        free(nc[i]);
    }
    free(nc);
    for (i = 0; i < n; i++) {
        free(lp[i]);
    }
    free(lp);
    free(oldc);     
}


