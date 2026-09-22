/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
//---------------------------------------------------------------------------

#ifndef QM_DSP_POLYFIT_H
#define QM_DSP_POLYFIT_H

//---------------------------------------------------------------------------
// Least-squares curve fitting class for arbitrary data types
/*

{ ******************************************
  ****   Scientific Subroutine Library  ****
  ****         for C++ Builder          ****
  ******************************************

  The following programs were written by Allen Miller and appear in the
  book entitled "Pascal Programs For Scientists And Engineers" which is
  published by Sybex, 1981.
  They were originally typed and submitted to MTPUG in Oct. 1982
    Juergen Loewner
    Hoher Heckenweg 3
    D-4400 Muenster
  They have had minor corrections and adaptations for Turbo Pascal by
    Jeff Weiss
    1572 Peacock Ave.
    Sunnyvale, CA 94087.


2000 Oct 28  Updated for Delphi 4, open array parameters.
             This allows the routine to be generalised so that it is no longer
             hard-coded to make a specific order of best fit or work with a
             specific number of points.
2001 Jan 07  Update Web site address

Copyright © David J Taylor, Edinburgh and others listed above
Web site:  www.satsignal.net
E-mail:    davidtaylor@writeme.com
}*/

///////////////////////////////////////////////////////////////////////////////
// Modified by CLandone for VC6 Aug 2004
///////////////////////////////////////////////////////////////////////////////

#include <iostream>

using std::vector;

class TPolyFit
{
    typedef vector<vector<double> > Matrix;
public:

    static double PolyFit2 (const vector<double> &x,  // does the work
                            const vector<double> &y,
                            vector<double> &coef);

                   
private:
    TPolyFit &operator = (const TPolyFit &);   // disable assignment
    TPolyFit();                                // and instantiation
    TPolyFit(const TPolyFit&);                 // and copying
  
    static void Square (const Matrix &x,              // Matrix multiplication routine
                        const vector<double> &y,
                        Matrix &a,                    // A = transpose X times X
                        vector<double> &g,         // G = Y times X
                        const int nrow, const int ncol);
    // Forms square coefficient matrix

    static bool GaussJordan (Matrix &b,                  // square matrix of coefficients
                             const vector<double> &y, // constant vector
                             vector<double> &coef);   // solution vector
    // returns false if matrix singular

    static bool GaussJordan2(Matrix &b,
                             const vector<double> &y,
                             Matrix &w,
                             vector<vector<int> > &index);
};

// some utility functions

struct NSUtility
{
    static void swap(double &a, double &b) {
        double t = a; a = b; b = t;
    }
    
    // fills a vector with zeros.
    static void zeroise(vector<double> &array, int n) { 
        array.clear();
        for(int j = 0; j < n; ++j) array.push_back(0);
    }
    
    // fills a vector with zeros.
    static void zeroise(vector<int> &array, int n) {
        array.clear();
        for(int j = 0; j < n; ++j) array.push_back(0);
    }
    
    // fills a (m by n) matrix with zeros.
    static void zeroise(vector<vector<double> > &matrix, int m, int n) {
        vector<double> zero;
        zeroise(zero, n);
        matrix.clear();
        for(int j = 0; j < m; ++j) matrix.push_back(zero);
    }
    
    // fills a (m by n) matrix with zeros.
    static void zeroise(vector<vector<int> > &matrix, int m, int n) {
        vector<int> zero;
        zeroise(zero, n);
        matrix.clear();
        for(int j = 0; j < m; ++j) matrix.push_back(zero);
    }
    
    static double sqr(const double &x) {return x * x;}
};


// main PolyFit routine

double TPolyFit::PolyFit2 (const vector<double> &x,
                           const vector<double> &y,
                           vector<double> &coefs)
// nterms = coefs.size()
// npoints = x.size()
{
    int i, j;
    double xi, yi, yc, srs, sum_y, sum_y2;
    Matrix xmatr;        // Data matrix
    Matrix a;
    vector<double> g;      // Constant vector
    const int npoints(x.size());
    const int nterms(coefs.size());
    double correl_coef;
    NSUtility::zeroise(g, nterms);
    NSUtility::zeroise(a, nterms, nterms);
    NSUtility::zeroise(xmatr, npoints, nterms);
    if (nterms < 1) {
        std::cerr << "ERROR: PolyFit called with less than one term" << std::endl;
        return 0;
    }
    if(npoints < 2) {
        std::cerr << "ERROR: PolyFit called with less than two points" << std::endl;
        return 0;
    }
    if(npoints != (int)y.size()) {
        std::cerr << "ERROR: PolyFit called with x and y of unequal size" << std::endl;
        return 0;
    }
    for(i = 0; i < npoints; ++i) {
        //      { setup x matrix }
        xi = x[i];
        xmatr[i][0] = 1.0;         //     { first column }
        for(j = 1; j < nterms; ++j)
            xmatr[i][j] = xmatr [i][j - 1] * xi;
    }
    Square (xmatr, y, a, g, npoints, nterms);
    if(!GaussJordan (a, g, coefs)) {
        return -1;
    }
    sum_y = 0.0;
    sum_y2 = 0.0;
    srs = 0.0;
    for(i = 0; i < npoints; ++i) {
        yi = y[i];
        yc = 0.0;
        for(j = 0; j < nterms; ++j) {
            yc += coefs [j] * xmatr [i][j];
        }
        srs += NSUtility::sqr (yc - yi);
        sum_y += yi;
        sum_y2 += yi * yi;
    }

    // If all Y values are the same, avoid dividing by zero
    correl_coef = sum_y2 - NSUtility::sqr (sum_y) / npoints;
    // Either return 0 or the correct value of correlation coefficient
    if (correl_coef != 0) {
        correl_coef = srs / correl_coef;
    }
    if (correl_coef >= 1) {
        correl_coef = 0.0;
    } else {
        correl_coef = sqrt (1.0 - correl_coef);
    }
    return correl_coef;
}


//------------------------------------------------------------------------

// Matrix multiplication routine
// A = transpose X times X
// G = Y times X

// Form square coefficient matrix

void TPolyFit::Square (const Matrix &x,
                       const vector<double> &y,
                       Matrix &a,
                       vector<double> &g,
                       const int nrow,
                       const int ncol)
{
    int i, k, l;
    for(k = 0; k < ncol; ++k) {
        for(l = 0; l < k + 1; ++l) {
            a [k][l] = 0.0;
            for(i = 0; i < nrow; ++i) {
                a[k][l] += x[i][l] * x [i][k];
                if(k != l) {
                    a[l][k] = a[k][l];
                }
            }
        }
        g[k] = 0.0;
        for(i = 0; i < nrow; ++i) {
            g[k] += y[i] * x[i][k];
        }
    }
}
//---------------------------------------------------------------------------------


bool TPolyFit::GaussJordan (Matrix &b,
                            const vector<double> &y,
                            vector<double> &coef)
//b square matrix of coefficients
//y constant vector
//coef solution vector
//ncol order of matrix got from b.size()


{
/*
  { Gauss Jordan matrix inversion and solution }

  { B (n, n) coefficient matrix becomes inverse }
  { Y (n) original constant vector }
  { W (n, m) constant vector(s) become solution vector }
  { DETERM is the determinant }
  { ERROR = 1 if singular }
  { INDEX (n, 3) }
  { NV is number of constant vectors }
*/

    int ncol(b.size());
    int irow, icol;
    vector<vector<int> >index;
    Matrix w;

    NSUtility::zeroise(w, ncol, ncol);
    NSUtility::zeroise(index, ncol, 3);

    if (!GaussJordan2(b, y, w, index)) {
        return false;
    }

    // Interchange columns
    int m;
    for (int i = 0; i <  ncol; ++i) {
        m = ncol - i - 1;
        if(index [m][0] != index [m][1]) {
            irow = index [m][0];
            icol = index [m][1];
            for(int k = 0; k < ncol; ++k) {
                NSUtility::swap (b[k][irow], b[k][icol]);
            }
        }
    }

    for(int k = 0; k < ncol; ++k) {
        if(index [k][2] != 0) {
            std::cerr << "ERROR: Error in PolyFit::GaussJordan: matrix is singular" << std::endl;
            return false;
        }
    }

    for( int i = 0; i < ncol; ++i) {
        coef[i] = w[i][0];
    }
 
    return true;
}   // end;     { procedure GaussJordan }
//----------------------------------------------------------------------------------------------


bool TPolyFit::GaussJordan2(Matrix &b,
                            const vector<double> &y,
                            Matrix &w,
                            vector<vector<int> > &index)
{
    //GaussJordan2;         // first half of GaussJordan
    // actual start of gaussj
 
    double big, t;
    double pivot;
    double determ;
    int irow = 0, icol = 0;
    int ncol(b.size());
    int nv = 1;                  // single constant vector

    for(int i = 0; i < ncol; ++i) {
        w[i][0] = y[i];      // copy constant vector
        index[i][2] = -1;
    }

    determ = 1.0;

    for (int i = 0; i < ncol; ++i) {
        // Search for largest element
        big = 0.0;

        for (int j = 0; j < ncol; ++j) {
            if (index[j][2] != 0) {
                for (int k = 0; k < ncol; ++k) {
                    if (index[k][2] > 0) {
                        std::cerr << "ERROR: Error in PolyFit::GaussJordan2: matrix is singular" << std::endl;
                        return false;
                    }

                    if (index[k][2] < 0 && fabs(b[j][k]) > big) {
                        irow = j;
                        icol = k;
                        big = fabs(b[j][k]);
                    }
                } //   { k-loop }
            }
        }  // { j-loop }
        
        index [icol][2] = index [icol][2] + 1;
        index [i][0] = irow;
        index [i][1] = icol;

        // Interchange rows to put pivot on diagonal
        // GJ3
        if (irow != icol) {
            determ = -determ;
            for (int m = 0; m < ncol; ++m) {
                NSUtility::swap (b [irow][m], b[icol][m]);
            }
            if (nv > 0) {
                for (int m = 0; m < nv; ++m) {
                    NSUtility::swap (w[irow][m], w[icol][m]);
                }
            }
        } // end GJ3

        // divide pivot row by pivot column
        pivot = b[icol][icol];
        determ *= pivot;
        b[icol][icol] = 1.0;

        for (int m = 0; m < ncol; ++m) {
            b[icol][m] /= pivot;
        }
        if (nv > 0) {
            for (int m = 0; m < nv; ++m) {
                w[icol][m] /= pivot;
            }
        }

        // Reduce nonpivot rows
        for (int n = 0; n < ncol; ++n) {
            if (n != icol) {
                t = b[n][icol];
                b[n][icol] = 0.0;
                for (int m = 0; m < ncol; ++m) {
                    b[n][m] -= b[icol][m] * t;
                }
                if (nv > 0) {
                    for (int m = 0; m < nv; ++m) {
                        w[n][m] -= w[icol][m] * t;
                    }
                }
            }
        }
    } // { i-loop }
    
    return true;
}

#endif
 
