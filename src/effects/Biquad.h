/**********************************************************************

Audacity: A Digital Audio Editor

EffectScienFilter.h

Norm C
Max Maisel

***********************************************************************/

#ifndef __BIQUAD_H__
#define __BIQUAD_H__


/// \brief Represents a biquad digital filter.
struct Biquad
{
   Biquad();
   void Reset();
   void Process(int iNumSamples);

   enum
   {
      /// Numerator coefficient indices
      B0=0, B1, B2,
      /// Denominator coefficient indices
      A1=0, A2
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

   float* pfIn;
   float* pfOut;
   float fNumerCoeffs[3]; // B0 B1 B2
   float fDenomCoeffs[2]; // A1 A2, A0 == 1.0
   float fPrevIn;
   float fPrevPrevIn;
   float fPrevOut;
   float fPrevPrevOut;
};

void ComplexDiv (float fNumerR, float fNumerI, float fDenomR, float fDenomI, float* pfQuotientR, float* pfQuotientI);
bool BilinTransform (float fSX, float fSY, float* pfZX, float* pfZY);
float Calc2D_DistSqr (float fX1, float fY1, float fX2, float fY2);

#endif
