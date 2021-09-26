/**********************************************************************

  Audacity: A Digital Audio Editor

  InterpolateAudio.cpp

  Dominic Mazzoni

**********************************************************************/

#include "InterpolateAudio.h"

#include <math.h>
#include <stdlib.h>

#include <wx/defs.h>

#include "Matrix.h"

static inline int imin(int x, int y)
{
   return x<y? x: y;
}

static inline int imax(int x, int y)
{
   return x>y? x: y;
}

// This function is a really dumb, simple way to interpolate audio,
// if the more general InterpolateAudio function below doesn't have
// enough data to work with.  If the bad samples are in the middle,
// it's literally linear.  If it's on either edge, we add some decay
// back to zero.
static void LinearInterpolateAudio(float *buffer, int len,
                                   int firstBad, int numBad)
{
   int i;

   float decay = 0.9f;

   if (firstBad==0) {
      float delta = buffer[numBad] - buffer[numBad+1];
      float value = buffer[numBad];
      i = numBad - 1;
      while (i >= 0) {
         value += delta;
         buffer[i] = value;
         value *= decay;
         delta *= decay;
         i--;
      }
   }
   else if (firstBad + numBad == len) {
      float delta = buffer[firstBad-1] - buffer[firstBad-2];
      float value = buffer[firstBad-1];
      i = firstBad;
      while (i < firstBad + numBad) {
         value += delta;
         buffer[i] = value;
         value *= decay;
         delta *= decay;
         i++;
      }
   }
   else {
      float v1 = buffer[firstBad-1];
      float v2 = buffer[firstBad+numBad];
      float value = v1;
      float delta = (v2 - v1) / (numBad+1);
      i = firstBad;
      while (i < firstBad + numBad) {
         value += delta;
         buffer[i] = value;
         i++;
      }
   }
}

// Here's the main interpolate function, using
// Least Squares AutoRegression (LSAR):
void InterpolateAudio(float *buffer, const size_t len,
                      size_t firstBad, size_t numBad)
{
   const auto N = len;

   wxASSERT(len > 0 &&
            firstBad >= 0 &&
            numBad < len &&
            firstBad+numBad <= len);

   if(numBad >= len)
      return;  //should never have been called!

   if (firstBad == 0) {
      // The algorithm below has a weird asymmetry in that it
      // performs poorly when interpolating to the left.  If
      // we're asked to interpolate the left side of a buffer,
      // we just reverse the problem and try it that way.
      Floats buffer2{ len };
      for(size_t i=0; i<len; i++)
         buffer2[len-1-i] = buffer[i];
      InterpolateAudio(buffer2.get(), len, len-numBad, numBad);
      for(size_t i=0; i<len; i++)
         buffer[len-1-i] = buffer2[i];
      return;
   }

   Vector s(len, buffer);

   // Choose P, the order of the autoregression equation
   const int IP =
      imin(imin(numBad * 3, 50), imax(firstBad - 1, len - (firstBad + numBad) - 1));

   if (IP < 3 || IP >= (int)N) {
      LinearInterpolateAudio(buffer, len, firstBad, numBad);
      return;
   }

   size_t P(IP);

   // Add a tiny amount of random noise to the input signal -
   // this sounds like a bad idea, but the amount we're adding
   // is only about 1 bit in 16-bit audio, and it's an extremely
   // effective way to avoid nearly-singular matrices.  If users
   // run it more than once they get slightly different results;
   // this is sometimes even advantageous.
   for(size_t i=0; i<N; i++)
      s[i] += (rand()-(RAND_MAX/2))/(RAND_MAX*10000.0);

   // Solve for the best autoregression coefficients
   // using a least-squares fit to all of the non-bad
   // data we have in the buffer
   Matrix X(P, P);
   Vector b(P);

   for(size_t i = 0; i + P < len; i++)
      if (i+P < firstBad || i >= (firstBad + numBad))
         for(size_t row=0; row<P; row++) {
            for(size_t col=0; col<P; col++)
               X[row][col] += (s[i+row] * s[i+col]);
            b[row] += s[i+P] * s[i+row];
         }

   Matrix Xinv(P, P);
   if (!InvertMatrix(X, Xinv)) {
      // The matrix is singular!  Fall back on linear...
      // In practice I have never seen this happen if
      // we add the tiny bit of random noise.
      LinearInterpolateAudio(buffer, len, firstBad, numBad);
      return;
   }

   // This vector now contains the autoregression coefficients
   const Vector &a = Xinv * b;

   // Create a matrix (a "Toeplitz" matrix, as it turns out)
   // which encodes the autoregressive relationship between
   // elements of the sequence.
   Matrix A(N-P, N);
   for(size_t row=0; row<N-P; row++) {
      for(size_t col=0; col<P; col++)
         A[row][row+col] = -a[col];
      A[row][row+P] = 1;
   }

   // Split both the Toeplitz matrix and the signal into
   // two pieces.  Note that this code could be made to
   // work even in the case where the "bad" samples are
   // not contiguous, but currently it assumes they are.
   //   "u" is for unknown (bad)
   //   "k" is for known (good)
   Matrix Au = MatrixSubset(A, 0, N-P, firstBad, numBad);
   Matrix A_left = MatrixSubset(A, 0, N-P, 0, firstBad);
   Matrix A_right = MatrixSubset(A, 0, N-P,
                                 firstBad+numBad, N-(firstBad+numBad));
   Matrix Ak = MatrixConcatenateCols(A_left, A_right);

   const Vector &s_left = VectorSubset(s, 0, firstBad);
   const Vector &s_right = VectorSubset(s, firstBad+numBad,
                                 N-(firstBad+numBad));
   const Vector &sk = VectorConcatenate(s_left, s_right);

   // Do some linear algebra to find the best possible
   // values that fill in the "bad" area
   Matrix AuT = TransposeMatrix(Au);
   Matrix X1 = MatrixMultiply(AuT, Au);
   Matrix X2(X1.Rows(), X1.Cols());
   if (!InvertMatrix(X1, X2)) {
      // The matrix is singular!  Fall back on linear...
      LinearInterpolateAudio(buffer, len, firstBad, numBad);
      return;
   }
   Matrix X2b = X2 * -1.0;
   Matrix X3 = MatrixMultiply(X2b, AuT);
   Matrix X4 = MatrixMultiply(X3, Ak);
   // This vector contains our best guess as to the
   // unknown values
   const Vector &su = X4 * sk;

   // Put the results into the return buffer
   for(size_t i=0; i<numBad; i++)
      buffer[firstBad+i] = (float)su[i];
}
