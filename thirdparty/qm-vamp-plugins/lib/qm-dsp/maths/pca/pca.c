/*********************************/
/* Principal Components Analysis */
/*********************************/

/*********************************************************************/
/* Principal Components Analysis or the Karhunen-Loeve expansion is a
   classical method for dimensionality reduction or exploratory data
   analysis.  One reference among many is: F. Murtagh and A. Heck,
   Multivariate Data Analysis, Kluwer Academic, Dordrecht, 1987.

   Author:
   F. Murtagh
   Phone:        + 49 89 32006298 (work)
                 + 49 89 965307 (home)
   Earn/Bitnet:  fionn@dgaeso51,  fim@dgaipp1s,  murtagh@stsci
   Span:         esomc1::fionn
   Internet:     murtagh@scivax.stsci.edu
   
   F. Murtagh, Munich, 6 June 1989                                   */   
/*********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "pca.h"

#define SIGN(a, b) ( (b) < 0 ? -fabs(a) : fabs(a) )

/**  Variance-covariance matrix: creation  *****************************/

/* Create m * m covariance matrix from given n * m data matrix. */
void covcol(double** data, int n, int m, double** symmat)
{
    double *mean;
    int i, j, j1, j2;

/* Allocate storage for mean vector */

    mean = (double*) malloc(m*sizeof(double));

/* Determine mean of column vectors of input data matrix */

    for (j = 0; j < m; j++)
    {
        mean[j] = 0.0;
        for (i = 0; i < n; i++)
        {
            mean[j] += data[i][j];
        }
        mean[j] /= (double)n;
    }

/*
  printf("\nMeans of column vectors:\n");
  for (j = 0; j < m; j++)  {
  printf("%12.1f",mean[j]);  }   printf("\n");
*/

/* Center the column vectors. */

    for (i = 0; i < n; i++)
    {
        for (j = 0; j < m; j++)
        {
            data[i][j] -= mean[j];
        }
    }

/* Calculate the m * m covariance matrix. */
    for (j1 = 0; j1 < m; j1++)
    {
        for (j2 = j1; j2 < m; j2++)
        {
            symmat[j1][j2] = 0.0;
            for (i = 0; i < n; i++)
            {
                symmat[j1][j2] += data[i][j1] * data[i][j2];
            }
            symmat[j2][j1] = symmat[j1][j2];
        }
    }

    free(mean);

    return;

}

/**  Error handler  **************************************************/

void erhand(char* err_msg)
{
    fprintf(stderr,"Run-time error:\n");
    fprintf(stderr,"%s\n", err_msg);
    fprintf(stderr,"Exiting to system.\n");
    exit(1);
}


/**  Reduce a real, symmetric matrix to a symmetric, tridiag. matrix. */

/* Householder reduction of matrix a to tridiagonal form.
   Algorithm: Martin et al., Num. Math. 11, 181-195, 1968.
   Ref: Smith et al., Matrix Eigensystem Routines -- EISPACK Guide
   Springer-Verlag, 1976, pp. 489-494.
   W H Press et al., Numerical Recipes in C, Cambridge U P,
   1988, pp. 373-374.  */
void tred2(double** a, int n, double* d, double* e)
{
    int l, k, j, i;
    double scale, hh, h, g, f;
        
    for (i = n-1; i >= 1; i--)
    {
        l = i - 1;
        h = scale = 0.0;
        if (l > 0)
        {
            for (k = 0; k <= l; k++)
                scale += fabs(a[i][k]);
            if (scale == 0.0)
                e[i] = a[i][l];
            else
            {
                for (k = 0; k <= l; k++)
                {
                    a[i][k] /= scale;
                    h += a[i][k] * a[i][k];
                }
                f = a[i][l];
                g = f>0 ? -sqrt(h) : sqrt(h);
                e[i] = scale * g;
                h -= f * g;
                a[i][l] = f - g;
                f = 0.0;
                for (j = 0; j <= l; j++)
                {
                    a[j][i] = a[i][j]/h;
                    g = 0.0;
                    for (k = 0; k <= j; k++)
                        g += a[j][k] * a[i][k];
                    for (k = j+1; k <= l; k++)
                        g += a[k][j] * a[i][k];
                    e[j] = g / h;
                    f += e[j] * a[i][j];
                }
                hh = f / (h + h);
                for (j = 0; j <= l; j++)
                {
                    f = a[i][j];
                    e[j] = g = e[j] - hh * f;
                    for (k = 0; k <= j; k++)
                        a[j][k] -= (f * e[k] + g * a[i][k]);
                }
            }
        }
        else
            e[i] = a[i][l];
        d[i] = h;
    }
    d[0] = 0.0;
    e[0] = 0.0;
    for (i = 0; i < n; i++)
    {
        l = i - 1;
        if (d[i])
        {
            for (j = 0; j <= l; j++)
            {
                g = 0.0;
                for (k = 0; k <= l; k++)
                    g += a[i][k] * a[k][j];
                for (k = 0; k <= l; k++)
                    a[k][j] -= g * a[k][i];
            }
        }
        d[i] = a[i][i];
        a[i][i] = 1.0;
        for (j = 0; j <= l; j++)
            a[j][i] = a[i][j] = 0.0;
    }
}

/**  Tridiagonal QL algorithm -- Implicit  **********************/

void tqli(double* d, double* e, int n, double** z)
{
    int m, l, iter, i, k;
    double s, r, p, g, f, dd, c, b;
        
    for (i = 1; i < n; i++)
        e[i-1] = e[i];
    e[n-1] = 0.0;
    for (l = 0; l < n; l++)
    {
        iter = 0;
        do
        {
            for (m = l; m < n-1; m++)
            {
                dd = fabs(d[m]) + fabs(d[m+1]);
                if (fabs(e[m]) + dd == dd) break;
            }
            if (m != l)
            {
                if (iter++ == 30) erhand("No convergence in TLQI.");
                g = (d[l+1] - d[l]) / (2.0 * e[l]);
                r = sqrt((g * g) + 1.0);
                g = d[m] - d[l] + e[l] / (g + SIGN(r, g));
                s = c = 1.0;
                p = 0.0;
                for (i = m-1; i >= l; i--)
                {
                    f = s * e[i];
                    b = c * e[i];
                    if (fabs(f) >= fabs(g))
                    {
                        c = g / f;
                        r = sqrt((c * c) + 1.0);
                        e[i+1] = f * r;
                        c *= (s = 1.0/r);
                    }
                    else
                    {
                        s = f / g;
                        r = sqrt((s * s) + 1.0);
                        e[i+1] = g * r;
                        s *= (c = 1.0/r);
                    }
                    g = d[i+1] - p;
                    r = (d[i] - g) * s + 2.0 * c * b;
                    p = s * r;
                    d[i+1] = g + p;
                    g = c * r - b;
                    for (k = 0; k < n; k++)
                    {
                        f = z[k][i+1];
                        z[k][i+1] = s * z[k][i] + c * f;
                        z[k][i] = c * z[k][i] - s * f;
                    }
                }
                d[l] = d[l] - p;
                e[l] = g;
                e[m] = 0.0;
            }
        }  while (m != l);
    }
}

/* In place projection onto basis vectors */
void pca_project(double** data, int n, int m, int ncomponents)
{
    int  i, j, k, k2;
    double  **symmat, /* **symmat2, */ *evals, *interm;
        
    //TODO: assert ncomponents < m
        
    symmat = (double**) malloc(m*sizeof(double*));
    for (i = 0; i < m; i++)
        symmat[i] = (double*) malloc(m*sizeof(double));
                
    covcol(data, n, m, symmat);
        
    /*********************************************************************
                Eigen-reduction
    **********************************************************************/
        
    /* Allocate storage for dummy and new vectors. */
    evals = (double*) malloc(m*sizeof(double));     /* Storage alloc. for vector of eigenvalues */
    interm = (double*) malloc(m*sizeof(double));    /* Storage alloc. for 'intermediate' vector */
    //MALLOC_ARRAY(symmat2,m,m,double);    
    //for (i = 0; i < m; i++) {
    //      for (j = 0; j < m; j++) {
    //              symmat2[i][j] = symmat[i][j]; /* Needed below for col. projections */
    //      }
    //}
    tred2(symmat, m, evals, interm);  /* Triangular decomposition */
    tqli(evals, interm, m, symmat);   /* Reduction of sym. trid. matrix */
/* evals now contains the eigenvalues,
   columns of symmat now contain the associated eigenvectors. */   

/*
  printf("\nEigenvalues:\n");
  for (j = m-1; j >= 0; j--) {
  printf("%18.5f\n", evals[j]); }
  printf("\n(Eigenvalues should be strictly positive; limited\n");
  printf("precision machine arithmetic may affect this.\n");
  printf("Eigenvalues are often expressed as cumulative\n");
  printf("percentages, representing the 'percentage variance\n");
  printf("explained' by the associated axis or principal component.)\n");
        
  printf("\nEigenvectors:\n");
  printf("(First three; their definition in terms of original vbes.)\n");
  for (j = 0; j < m; j++) {
  for (i = 1; i <= 3; i++)  {
  printf("%12.4f", symmat[j][m-i]);  }
  printf("\n");  }
*/

/* Form projections of row-points on prin. components. */
/* Store in 'data', overwriting original data. */
    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++) {
            interm[j] = data[i][j]; }   /* data[i][j] will be overwritten */
        for (k = 0; k < ncomponents; k++) {
            data[i][k] = 0.0;
            for (k2 = 0; k2 < m; k2++) {
                data[i][k] += interm[k2] * symmat[k2][m-k-1]; }
        }
    }

/*      
        printf("\nProjections of row-points on first 3 prin. comps.:\n");
        for (i = 0; i < n; i++) {
        for (j = 0; j < 3; j++)  {
        printf("%12.4f", data[i][j]);  }
        printf("\n");  }
*/

/* Form projections of col.-points on first three prin. components. */
/* Store in 'symmat2', overwriting what was stored in this. */
//for (j = 0; j < m; j++) {
//       for (k = 0; k < m; k++) {
//               interm[k] = symmat2[j][k]; }  /*symmat2[j][k] will be overwritten*/
//  for (i = 0; i < 3; i++) {
//      symmat2[j][i] = 0.0;
//              for (k2 = 0; k2 < m; k2++) {
//                      symmat2[j][i] += interm[k2] * symmat[k2][m-i-1]; }
//              if (evals[m-i-1] > 0.0005)   /* Guard against zero eigenvalue */
//                      symmat2[j][i] /= sqrt(evals[m-i-1]);   /* Rescale */
//              else
//                      symmat2[j][i] = 0.0;    /* Standard kludge */
//    }
// }

/*
  printf("\nProjections of column-points on first 3 prin. comps.:\n");
  for (j = 0; j < m; j++) {
  for (k = 0; k < 3; k++)  {
  printf("%12.4f", symmat2[j][k]);  }
  printf("\n");  }
*/


    for (i = 0; i < m; i++)
        free(symmat[i]);
    free(symmat);
//FREE_ARRAY(symmat2,m);
    free(evals);
    free(interm);

}
