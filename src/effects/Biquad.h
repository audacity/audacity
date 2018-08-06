/**********************************************************************

Audacity: A Digital Audio Editor

EffectScienFilter.h

Norm C
Max Maisel

***********************************************************************/

#ifndef __BIQUAD_H__
#define __BIQUAD_H__

#include "MemoryX.h"

/// \brief Represents a biquad digital filter.
struct Biquad
{
   Biquad();
   void Reset();
   void Process(float* pfIn, float* pfOut, int iNumSamples);

   enum
   {
      /// Numerator coefficient indices
      B0=0, B1, B2,
      /// Denominator coefficient indices
      A1=0, A2,

      /// Possible filter orders for the Calc...Filter(...) functions
      MIN_Order = 1,
      MAX_Order = 10
   };

   inline float ProcessOne(float fIn)
   {
      float fOut = fIn * fNumerCoeffs[B0] +
            fPrevIn * fNumerCoeffs[B1] +
            fPrevPrevIn * fNumerCoeffs[B2] -
            fPrevOut * fDenomCoeffs[A1] -
            fPrevPrevOut * fDenomCoeffs[A2];
      fPrevPrevIn = fPrevIn;
      fPrevIn = fIn;
      fPrevPrevOut = fPrevOut;
      fPrevOut = fOut;
      return fOut;
   }

   float fNumerCoeffs[3]; // B0 B1 B2
   float fDenomCoeffs[2]; // A1 A2, A0 == 1.0
   float fPrevIn;
   float fPrevPrevIn;
   float fPrevOut;
   float fPrevPrevOut;

   enum kSubTypes
   {
      kLowPass,
      kHighPass,
      nSubTypes
   };

   static ArrayOf<Biquad> CalcButterworthFilter(int order, double fn, double fc, int type);
   static ArrayOf<Biquad> CalcChebyshevType1Filter(int order, double fn, double fc, double ripple, int type);
   static ArrayOf<Biquad> CalcChebyshevType2Filter(int order, double fn, double fc, double ripple, int type);

   static void ComplexDiv (float fNumerR, float fNumerI, float fDenomR, float fDenomI, float* pfQuotientR, float* pfQuotientI);
   static bool BilinTransform (float fSX, float fSY, float* pfZX, float* pfZY);
   static float Calc2D_DistSqr (float fX1, float fY1, float fX2, float fY2);

   static const double s_fChebyCoeffs[MAX_Order][MAX_Order + 1];
   static double ChebyPoly(int Order, double NormFreq);
};

#endif
