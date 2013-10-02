#include "Biquad.h"

#define square(a) ((a)*(a))

void Biquad_Process (BiquadStruct* pBQ, int iNumSamples)
{
   float* pfIn = pBQ->pfIn;
   float* pfOut = pBQ->pfOut;
   float fPrevIn = pBQ->fPrevIn;
   float fPrevPrevIn = pBQ->fPrevPrevIn;
   float fPrevOut = pBQ->fPrevOut;
   float fPrevPrevOut = pBQ->fPrevPrevOut;
   for (int i = 0; i < iNumSamples; i++)
   {
      float fIn = *pfIn++;
      *pfOut = fIn * pBQ->fNumerCoeffs [0] +
         fPrevIn * pBQ->fNumerCoeffs [1] +
         fPrevPrevIn * pBQ->fNumerCoeffs [2] -
         fPrevOut * pBQ->fDenomCoeffs [0] -
         fPrevPrevOut * pBQ->fDenomCoeffs [1];
      fPrevPrevIn = fPrevIn;
      fPrevIn = fIn;
      fPrevPrevOut = fPrevOut;
      fPrevOut = *pfOut++;
   }
   pBQ->fPrevIn = fPrevIn;
   pBQ->fPrevPrevIn = fPrevPrevIn;
   pBQ->fPrevOut = fPrevOut;
   pBQ->fPrevPrevOut = fPrevPrevOut;
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

