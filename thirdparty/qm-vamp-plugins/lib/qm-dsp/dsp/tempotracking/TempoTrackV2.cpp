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

#include "TempoTrackV2.h"

#include <cmath>
#include <cstdlib>
#include <iostream>

#include "maths/MathUtilities.h"

using std::vector;

#define   EPS 0.0000008 // just some arbitrary small number

TempoTrackV2::TempoTrackV2(float rate, int increment) :
    m_rate(rate), m_increment(increment) {
}

TempoTrackV2::~TempoTrackV2() { }

void
TempoTrackV2::filter_df(d_vec_t &df)
{
    int df_len = int(df.size());
    
    d_vec_t a(3);
    d_vec_t b(3);
    d_vec_t lp_df(df_len);

    //equivalent in matlab to [b,a] = butter(2,0.4);
    a[0] = 1.0000;
    a[1] = -0.3695;
    a[2] = 0.1958;
    b[0] = 0.2066;
    b[1] = 0.4131;
    b[2] = 0.2066;

    double inp1 = 0.;
    double inp2 = 0.;
    double out1 = 0.;
    double out2 = 0.;


    // forwards filtering
    for (int i = 0; i < df_len; i++) {
        lp_df[i] =  b[0]*df[i] + b[1]*inp1 + b[2]*inp2 - a[1]*out1 - a[2]*out2;
        inp2 = inp1;
        inp1 = df[i];
        out2 = out1;
        out1 = lp_df[i];
    }

    // copy forwards filtering to df...
    // but, time-reversed, ready for backwards filtering
    for (int i = 0; i < df_len; i++) {
        df[i] = lp_df[df_len - i - 1];
    }

    for (int i = 0; i < df_len; i++) {
        lp_df[i] = 0.;
    }

    inp1 = 0.; inp2 = 0.;
    out1 = 0.; out2 = 0.;

    // backwards filetering on time-reversed df
    for (int i = 0; i < df_len; i++) {
        lp_df[i] =  b[0]*df[i] + b[1]*inp1 + b[2]*inp2 - a[1]*out1 - a[2]*out2;
        inp2 = inp1;
        inp1 = df[i];
        out2 = out1;
        out1 = lp_df[i];
    }

    // write the re-reversed (i.e. forward) version back to df
    for (int i = 0; i < df_len; i++) {
        df[i] = lp_df[df_len - i - 1];
    }
}


// MEPD 28/11/12
// This function now allows for a user to specify an inputtempo (in BPM)
// and a flag "constraintempo" which replaces the general rayleigh weighting for periodicities
// with a gaussian which is centered around the input tempo
// Note, if inputtempo = 120 and constraintempo = false, then functionality is
// as it was before
void
TempoTrackV2::calculateBeatPeriod(const vector<double> &df,
                                  vector<double> &beat_period,
                                  vector<double> &tempi,
                                  double inputtempo, bool constraintempo)
{
    // to follow matlab.. split into 512 sample frames with a 128 hop size
    // calculate the acf,
    // then the rcf.. and then stick the rcfs as columns of a matrix
    // then call viterbi decoding with weight vector and transition matrix
    // and get best path

    int wv_len = 128;

    // MEPD 28/11/12
    // the default value of inputtempo in the beat tracking plugin is 120
    // so if the user specifies a different inputtempo, the rayparam will be updated
    // accordingly.
    // note: 60*44100/512 is a magic number
    // this might (will?) break if a user specifies a different frame rate for the onset detection function
    double rayparam = (60*44100/512)/inputtempo;

    // make rayleigh weighting curve
    d_vec_t wv(wv_len);

    // check whether or not to use rayleigh weighting (if constraintempo is false)
    // or use gaussian weighting it (constraintempo is true)
    if (constraintempo) {
        for (int i = 0; i < wv_len; i++) {
            // MEPD 28/11/12
            // do a gaussian weighting instead of rayleigh
            wv[i] = exp( (-1.*pow((double(i)-rayparam),2.)) / (2.*pow(rayparam/4.,2.)) );
        }
    } else {
        for (int i = 0; i < wv_len; i++) {
            // MEPD 28/11/12
            // standard rayleigh weighting over periodicities
            wv[i] = (double(i) / pow(rayparam,2.)) * exp((-1.*pow(-double(i),2.)) / (2.*pow(rayparam,2.)));
        }
    }

    // beat tracking frame size (roughly 6 seconds) and hop (1.5 seconds)
    int winlen = 512;
    int step = 128;

    // matrix to store output of comb filter bank, increment column of matrix at each frame
    d_mat_t rcfmat;
    int col_counter = -1;
    int df_len = int(df.size());

    // main loop for beat period calculation
    for (int i = 0; i+winlen < df_len; i+=step) {
        
        // get dfframe
        d_vec_t dfframe(winlen);
        for (int k=0; k < winlen; k++) {
            dfframe[k] = df[i+k];
        }
        // get rcf vector for current frame
        d_vec_t rcf(wv_len);
        get_rcf(dfframe,wv,rcf);

        rcfmat.push_back( d_vec_t() ); // adds a new column
        col_counter++;
        for (int j = 0; j < wv_len; j++) {
            rcfmat[col_counter].push_back( rcf[j] );
        }
    }

    // now call viterbi decoding function
    viterbi_decode(rcfmat,wv,beat_period,tempi);
}


void
TempoTrackV2::get_rcf(const d_vec_t &dfframe_in, const d_vec_t &wv, d_vec_t &rcf)
{
    // calculate autocorrelation function
    // then rcf
    // just hard code for now... don't really need separate functions to do this

    // make acf

    d_vec_t dfframe(dfframe_in);

    MathUtilities::adaptiveThreshold(dfframe);

    int dfframe_len = int(dfframe.size());
    int rcf_len = int(rcf.size());
    
    d_vec_t acf(dfframe_len);

    for (int lag = 0; lag < dfframe_len; lag++) {
        double sum = 0.;
        double tmp = 0.;

        for (int n = 0; n < (dfframe_len - lag); n++) {
            tmp = dfframe[n] * dfframe[n + lag];
            sum += tmp;
        }
        acf[lag] = double(sum/ (dfframe_len - lag));
    }

    // now apply comb filtering
    int numelem = 4;

    for (int i = 2; i < rcf_len; i++) { // max beat period
        for (int a = 1; a <= numelem; a++) { // number of comb elements
            for (int b = 1-a; b <= a-1; b++) { // general state using normalisation of comb elements
                rcf[i-1] += ( acf[(a*i+b)-1]*wv[i-1] ) / (2.*a-1.);     // calculate value for comb filter row
            }
        }
    }

    // apply adaptive threshold to rcf
    MathUtilities::adaptiveThreshold(rcf);

    double rcfsum =0.;
    for (int i = 0; i < rcf_len; i++) {
        rcf[i] += EPS ;
        rcfsum += rcf[i];
    }

    // normalise rcf to sum to unity
    for (int i = 0; i < rcf_len; i++) {
        rcf[i] /= (rcfsum + EPS);
    }
}

void
TempoTrackV2::viterbi_decode(const d_mat_t &rcfmat, const d_vec_t &wv, d_vec_t &beat_period, d_vec_t &tempi)
{
    // following Kevin Murphy's Viterbi decoding to get best path of
    // beat periods through rfcmat
    
    int wv_len = int(wv.size());
    
    // make transition matrix
    d_mat_t tmat;
    for (int i = 0; i < wv_len; i++) {
        tmat.push_back ( d_vec_t() ); // adds a new column
        for (int j = 0; j < wv_len; j++) {
            tmat[i].push_back(0.); // fill with zeros initially
        }
    }

    // variance of Gaussians in transition matrix
    // formed of Gaussians on diagonal - implies slow tempo change
    double sigma = 8.;
    // don't want really short beat periods, or really long ones
    for (int i = 20; i  < wv_len - 20; i++) {
        for (int j = 20; j < wv_len - 20; j++) {
            double mu = double(i);
            tmat[i][j] = exp( (-1.*pow((j-mu),2.)) / (2.*pow(sigma,2.)) );
        }
    }

    // parameters for Viterbi decoding... this part is taken from
    // Murphy's matlab

    d_mat_t delta;
    i_mat_t psi;
    for (int i = 0; i < int(rcfmat.size()); i++) {
        delta.push_back(d_vec_t());
        psi.push_back(i_vec_t());
        for (int j = 0; j < int(rcfmat[i].size()); j++) {
            delta[i].push_back(0.); // fill with zeros initially
            psi[i].push_back(0); // fill with zeros initially
        }
    }

    int T = int(delta.size());

    if (T < 2) return; // can't do anything at all meaningful

    int Q = int(delta[0].size());

    // initialize first column of delta
    for (int j = 0; j < Q; j++) {
        delta[0][j] = wv[j] * rcfmat[0][j];
        psi[0][j] = 0;
    }

    double deltasum = 0.;
    for (int i = 0; i < Q; i++) {
        deltasum += delta[0][i];
    }
    for (int i = 0; i < Q; i++) {
        delta[0][i] /= (deltasum + EPS);
    }

    for (int t=1; t < T; t++)
    {
        d_vec_t tmp_vec(Q);

        for (int j = 0; j < Q; j++) {
            for (int i = 0; i < Q; i++) {
                tmp_vec[i] = delta[t-1][i] * tmat[j][i];
            }

            delta[t][j] = get_max_val(tmp_vec);

            psi[t][j] = get_max_ind(tmp_vec);

            delta[t][j] *= rcfmat[t][j];
        }

        // normalise current delta column
        double deltasum = 0.;
        for (int i = 0; i < Q; i++) {
            deltasum += delta[t][i];
        }
        for (int i = 0; i < Q; i++) {
            delta[t][i] /= (deltasum + EPS);
        }
    }

    i_vec_t bestpath(T);
    d_vec_t tmp_vec(Q);
    for (int i = 0; i < Q; i++) {
        tmp_vec[i] = delta[T-1][i];
    }

    // find starting point - best beat period for "last" frame
    bestpath[T-1] = get_max_ind(tmp_vec);

    // backtrace through index of maximum values in psi
    for (int t=T-2; t>0 ;t--) {
        bestpath[t] = psi[t+1][bestpath[t+1]];
    }

    // weird but necessary hack -- couldn't get above loop to terminate at t >= 0
    bestpath[0] = psi[1][bestpath[1]];

    int lastind = 0;
    for (int i = 0; i < T; i++) {
        int step = 128;
        for (int j = 0; j < step; j++) {
            lastind = i*step+j;
            beat_period[lastind] = bestpath[i];
        }
//        std::cerr << "bestpath[" << i << "] = " << bestpath[i] << " (used for beat_periods " << i*step << " to " << i*step+step-1 << ")" << std::endl;
    }

    // fill in the last values...
    for (int i = lastind; i < int(beat_period.size()); i++) {
        beat_period[i] = beat_period[lastind];
    }

    for (int i = 0; i < int(beat_period.size()); i++) {
        tempi.push_back((60. * m_rate / m_increment)/beat_period[i]);
    }
}

double
TempoTrackV2::get_max_val(const d_vec_t &df)
{
    double maxval = 0.;
    int df_len = int(df.size());
    
    for (int i = 0; i < df_len; i++) {
        if (maxval < df[i]) {
            maxval = df[i];
        }
    }

    return maxval;
}

int
TempoTrackV2::get_max_ind(const d_vec_t &df)
{
    double maxval = 0.;
    int ind = 0;
    int df_len = int(df.size());
    
    for (int i = 0; i < df_len; i++) {
        if (maxval < df[i]) {
            maxval = df[i];
            ind = i;
        }
    }

    return ind;
}

void
TempoTrackV2::normalise_vec(d_vec_t &df)
{
    double sum = 0.;
    int df_len = int(df.size());
    
    for (int i = 0; i < df_len; i++) {
        sum += df[i];
    }

    for (int i = 0; i < df_len; i++) {
        df[i]/= (sum + EPS);
    }
}

// MEPD 28/11/12
// this function has been updated to allow the "alpha" and "tightness" parameters
// of the dynamic program to be set by the user
// the default value of alpha = 0.9 and tightness = 4
void
TempoTrackV2::calculateBeats(const vector<double> &df,
                             const vector<double> &beat_period,
                             vector<double> &beats, double alpha, double tightness)
{
    if (df.empty() || beat_period.empty()) return;

    int df_len = int(df.size());

    d_vec_t cumscore(df_len); // store cumulative score
    i_vec_t backlink(df_len); // backlink (stores best beat locations at each time instant)
    d_vec_t localscore(df_len); // localscore, for now this is the same as the detection function

    for (int i = 0; i < df_len; i++) {
        localscore[i] = df[i];
        backlink[i] = -1;
    }

    //double tightness = 4.;
    //double alpha = 0.9;
    // MEPD 28/11/12
    // debug statements that can be removed.
//    std::cerr << "alpha" << alpha << std::endl;
//    std::cerr << "tightness" << tightness << std::endl;

    // main loop
    for (int i = 0; i < df_len; i++) {
        
        int prange_min = -2*beat_period[i];
        int prange_max = round(-0.5*beat_period[i]);

        // transition range
        int txwt_len = prange_max - prange_min + 1;
        d_vec_t txwt (txwt_len);
        d_vec_t scorecands (txwt_len);

        for (int j = 0; j < txwt_len; j++) {
            
            double mu = double(beat_period[i]);
            txwt[j] = exp( -0.5*pow(tightness * log((round(2*mu)-j)/mu),2));

            // IF IN THE ALLOWED RANGE, THEN LOOK AT CUMSCORE[I+PRANGE_MIN+J
            // ELSE LEAVE AT DEFAULT VALUE FROM INITIALISATION:  D_VEC_T SCORECANDS (TXWT.SIZE());

            int cscore_ind = i + prange_min + j;
            if (cscore_ind >= 0) {
                scorecands[j] = txwt[j] * cumscore[cscore_ind];
            }
        }

        // find max value and index of maximum value
        double vv = get_max_val(scorecands);
        int xx = get_max_ind(scorecands);

        cumscore[i] = alpha*vv + (1.-alpha)*localscore[i];
        backlink[i] = i+prange_min+xx;

//        std::cerr << "backlink[" << i << "] <= " << backlink[i] << std::endl;
    }

    // STARTING POINT, I.E. LAST BEAT.. PICK A STRONG POINT IN cumscore VECTOR
    d_vec_t tmp_vec;
    for (int i = df_len - beat_period[beat_period.size()-1] ; i < df_len; i++) {
        tmp_vec.push_back(cumscore[i]);
    }

    int startpoint = get_max_ind(tmp_vec) +
        df_len - beat_period[beat_period.size()-1] ;

    // can happen if no results obtained earlier (e.g. input too short)
    if (startpoint >= int(backlink.size())) {
        startpoint = int(backlink.size()) - 1;
    }

    // USE BACKLINK TO GET EACH NEW BEAT (TOWARDS THE BEGINNING OF THE FILE)
    //  BACKTRACKING FROM THE END TO THE BEGINNING.. MAKING SURE NOT TO GO BEFORE SAMPLE 0
    i_vec_t ibeats;
    ibeats.push_back(startpoint);
//    std::cerr << "startpoint = " << startpoint << std::endl;
    while (backlink[ibeats.back()] > 0) {
//        std::cerr << "backlink[" << ibeats.back() << "] = " << backlink[ibeats.back()] << std::endl;
        int b = ibeats.back();
        if (backlink[b] == b) break; // shouldn't happen... haha
        ibeats.push_back(backlink[b]);
    }

    // REVERSE SEQUENCE OF IBEATS AND STORE AS BEATS
    for (int i = 0; i < int(ibeats.size()); i++) {
        beats.push_back(double(ibeats[ibeats.size() - i - 1]));
    }
}


