/**********************************************************************

Audacity: A Digital Audio Editor

EffectScienFilter.h

Norm C
Max Maisel

***********************************************************************/

#include "Biquad.h"

#define square(a) ((a)*(a))

Biquad::Biquad()
{
   pfIn = 0;
   pfOut = 0;
   fNumerCoeffs[B0] = 1;
   fNumerCoeffs[B1] = 0;
   fNumerCoeffs[B2] = 0;
   fDenomCoeffs[A1] = 0;
   fDenomCoeffs[A2] = 0;
   Reset();
}

void Biquad::Reset()
{
   fPrevIn = 0;
   fPrevPrevIn = 0;
   fPrevOut = 0;
   fPrevPrevOut = 0;
}

void Biquad::Process(int iNumSamples)
{
   for (int i = 0; i < iNumSamples; i++)
      *pfOut++ = ProcessOne(*pfIn++);
}

void ComplexDiv (float fNumerR, float fNumerI, float fDenomR, float fDenomI, float* pfQuotientR, float* pfQuotientI)
{
   float fDenom = square(fDenomR) + square(fDenomI);
   *pfQuotientR = (fNumerR * fDenomR + fNumerI * fDenomI) / fDenom;
   *pfQuotientI = (fNumerI * fDenomR - fNumerR * fDenomI) / fDenom;
}

bool BilinTransform (float fSX, float fSY, float* pfZX, float* pfZY)
{
   float fDenom = square (1 - fSX) + square (fSY);
   *pfZX = (1 - square (fSX) - square (fSY)) / fDenom;
   *pfZY = 2 * fSY / fDenom;
   return true;
}

float Calc2D_DistSqr (float fX1, float fY1, float fX2, float fY2)
{
   return square (fX1 - fX2) + square (fY1 - fY2);
}

