/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
 *  cluster_segmenter.c
 *  soundbite
 *
 *  Created by Mark Levy on 06/04/2006.
 *  Copyright 2006 Centre for Digital Music, Queen Mary, University of London.

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of the
 License, or (at your option) any later version.  See the file
 COPYING included with this distribution for more information.
 *
 */

#include "cluster_segmenter.h"

extern int readmatarray_size(const char *filepath, int n_array, int* t, int* d);
extern int readmatarray(const char *filepath, int n_array, int t, int d, double** arr);

/* converts constant-Q features to normalised chroma */
void cq2chroma(double** cq, int nframes, int ncoeff, int bins, double** chroma)
{
    int noct = ncoeff / bins;       /* number of complete octaves in constant-Q */
    int t, b, oct, ix;
        
    for (t = 0; t < nframes; t++) {
        for (b = 0; b < bins; b++) {
            chroma[t][b] = 0;
        }
        for (oct = 0; oct < noct; oct++) {
            ix = oct * bins;
            for (b = 0; b < bins; b++) {
                chroma[t][b] += fabs(cq[t][ix+b]);
            }
        }
    }
}

/* applies MPEG-7 normalisation to constant-Q features, storing normalised envelope (norm) in last feature dimension */
void mpeg7_constq(double** features, int nframes, int ncoeff)
{
    int i, j;
    double ss;
    double env;
    double maxenv = 0;
        
    /* convert const-Q features to dB scale */
    for (i = 0; i < nframes; i++) {
        for (j = 0; j < ncoeff; j++) {
            features[i][j] = 10.0 * log10(features[i][j]+DBL_EPSILON);
        }
    }
        
    /* normalise each feature vector and add the norm as an extra feature dimension */      
    for (i = 0; i < nframes; i++) {
        ss = 0;
        for (j = 0; j < ncoeff; j++) {
            ss += features[i][j] * features[i][j];
        }
        env = sqrt(ss);
        for (j = 0; j < ncoeff; j++) {
            features[i][j] /= env;
        }
        features[i][ncoeff] = env;
        if (env > maxenv) {
            maxenv = env;
        }
    } 
    /* normalise the envelopes */
    for (i = 0; i < nframes; i++) {
        features[i][ncoeff] /= maxenv;
    }
}

/* return histograms h[nx*m] of data x[nx] into m bins using a sliding window of length h_len (MUST BE ODD) */
/* NB h is a vector in row major order, as required by cluster_melt() */
/* for historical reasons we normalise the histograms by their norm (not to sum to one) */
void create_histograms(int* x, int nx, int m, int hlen, double* h)
{
    int i, j, t;
    double norm;

    for (i = 0; i < nx*m; i++) {
        h[i] = 0;
    }

    for (i = hlen/2; i < nx-hlen/2; i++) {
        for (j = 0; j < m; j++) {
            h[i*m+j] = 0;
        }
        for (t = i-hlen/2; t <= i+hlen/2; t++) {
            ++h[i*m+x[t]];
        }
        norm = 0;
        for (j = 0; j < m; j++) {
            norm += h[i*m+j] * h[i*m+j];
        }
        for (j = 0; j < m; j++) {
            h[i*m+j] /= norm;
        }
    }
        
    /* duplicate histograms at beginning and end to create one histogram for each data value supplied */
    for (i = 0; i < hlen/2; i++) {
        for (j = 0; j < m; j++) {
            h[i*m+j] = h[hlen/2*m+j];
        }
    }
    for (i = nx-hlen/2; i < nx; i++) {
        for (j = 0; j < m; j++) {
            h[i*m+j] = h[(nx-hlen/2-1)*m+j];
        }
    }
}

/* segment using HMM and then histogram clustering */
void cluster_segment(int* q, double** features, int frames_read, int feature_length, int nHMM_states, 
                     int histogram_length, int nclusters, int neighbour_limit)
{
    int i, j;
        
    /*****************************/
    if (0) {
        /* try just using the predominant bin number as a 'decoded state' */
        nHMM_states = feature_length + 1;       /* allow a 'zero' state */
        double chroma_thresh = 0.05;
        double maxval;
        int maxbin;
        for (i = 0; i < frames_read; i++) {
            maxval = 0;
            for (j = 0; j < feature_length; j++) {
                if (features[i][j] > maxval) {
                    maxval = features[i][j];
                    maxbin = j;
                }                               
            }
            if (maxval > chroma_thresh) {
                q[i] = maxbin;
            } else {
                q[i] = feature_length;
            }
        }
        
    }
    if (1) {
        /*****************************/
        
        /* scale all the features to 'balance covariances' during HMM training */
        double scale = 10;
        for (i = 0; i < frames_read; i++)
            for (j = 0; j < feature_length; j++)
                features[i][j] *= scale;
        
        /* train an HMM on the features */
        
        /* create a model */
        model_t* model = hmm_init(features, frames_read, feature_length, nHMM_states);
        
        /* train the model */
        hmm_train(features, frames_read, model);
/*      
        printf("\n\nafter training:\n");
        hmm_print(model);
*/      
        /* decode the hidden state sequence */
        viterbi_decode(features, frames_read, model, q);  
        hmm_close(model);
        
        /*****************************/
    }
    /*****************************/
    
/*
  fprintf(stderr, "HMM state sequence:\n");
  for (i = 0; i < frames_read; i++)
  fprintf(stderr, "%d ", q[i]);
  fprintf(stderr, "\n\n");
*/
        
    /* create histograms of states */
    double* h = (double*) malloc(frames_read*nHMM_states*sizeof(double));   /* vector in row major order */
    create_histograms(q, frames_read, nHMM_states, histogram_length, h);
        
    /* cluster the histograms */
    int nbsched = 20;       /* length of inverse temperature schedule */
    double* bsched = (double*) malloc(nbsched*sizeof(double));      /* inverse temperature schedule */
    double b0 = 100;
    double alpha = 0.7;
    bsched[0] = b0;
    for (i = 1; i < nbsched; i++) {
        bsched[i] = alpha * bsched[i-1];
    }
    cluster_melt(h, nHMM_states, frames_read, bsched, nbsched, nclusters, neighbour_limit, q);
        
    /* now q holds a sequence of cluster assignments */
        
    free(h);  
    free(bsched);
}

/* segment constant-Q or chroma features */
void constq_segment(int* q, double** features, int frames_read, int bins, int ncoeff, int feature_type, 
                    int nHMM_states, int histogram_length, int nclusters, int neighbour_limit)
{
    int feature_length;
    double** chroma;
    int i;
        
    if (feature_type == FEATURE_TYPE_CONSTQ) {

        mpeg7_constq(features, frames_read, ncoeff);

        /* do PCA on the features (but not the envelope) */
        int ncomponents = 20;
        pca_project(features, frames_read, ncoeff, ncomponents);
                
        /* copy the envelope so that it immediatly follows the chosen components */
        for (i = 0; i < frames_read; i++) {
            features[i][ncomponents] = features[i][ncoeff];
        }
                
        feature_length = ncomponents + 1;
        
        cluster_segment(q, features, frames_read, feature_length,
                        nHMM_states, histogram_length, nclusters,
                        neighbour_limit);
    }
        
    if (feature_type == FEATURE_TYPE_CHROMA) {

        /* convert constant-Q to normalised chroma features */
        chroma = (double**) malloc(frames_read*sizeof(double*));
        for (i = 0; i < frames_read; i++) {
            chroma[i] = (double*) malloc(bins*sizeof(double));
        }
        
        cq2chroma(features, frames_read, ncoeff, bins, chroma);
        
        feature_length = bins;
                
        cluster_segment(q, chroma, frames_read, feature_length,
                        nHMM_states, histogram_length, nclusters,
                        neighbour_limit);
        
        for (i = 0; i < frames_read; i++)
            free(chroma[i]);
        free(chroma);
    }
}



