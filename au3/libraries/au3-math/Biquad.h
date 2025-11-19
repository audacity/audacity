/**********************************************************************

Audacity: A Digital Audio Editor

Biquad.h

Norm C
Max Maisel

***********************************************************************/

#ifndef __BIQUAD_H__
#define __BIQUAD_H__

#include "MemoryX.h"

/// \brief Represents a biquad digital filter.
struct MATH_API Biquad
{
    Biquad();
    void Reset();
    void Process(const float* pfIn, float* pfOut, int iNumSamples);

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
        // Biquad must use double for all calculations. Otherwise some
        // filters may have catastrophic rounding errors!
        double fOut = double(fIn) * fNumerCoeffs[B0]
                      + fPrevIn * fNumerCoeffs[B1]
                      + fPrevPrevIn * fNumerCoeffs[B2]
                      - fPrevOut * fDenomCoeffs[A1]
                      - fPrevPrevOut * fDenomCoeffs[A2];
        fPrevPrevIn = fPrevIn;
        fPrevIn = fIn;
        fPrevPrevOut = fPrevOut;
        fPrevOut = fOut;
        return fOut;
    }

    double fNumerCoeffs[3]; // B0 B1 B2
    double fDenomCoeffs[2]; // A1 A2, A0 == 1.0
    double fPrevIn;
    double fPrevPrevIn;
    double fPrevOut;
    double fPrevPrevOut;

    enum kSubTypes
    {
        kLowPass,
        kHighPass,
        nSubTypes
    };

    static ArrayOf<Biquad> CalcButterworthFilter(int order, double fn, double fc, int type);
    static ArrayOf<Biquad> CalcChebyshevType1Filter(int order, double fn, double fc, double ripple, int type);
    static ArrayOf<Biquad> CalcChebyshevType2Filter(int order, double fn, double fc, double ripple, int type);

    static void ComplexDiv(double fNumerR, double fNumerI, double fDenomR, double fDenomI, double* pfQuotientR, double* pfQuotientI);
    static bool BilinTransform(double fSX, double fSY, double* pfZX, double* pfZY);
    static float Calc2D_DistSqr(double fX1, double fY1, double fX2, double fY2);

    static const double s_fChebyCoeffs[MAX_Order][MAX_Order + 1];
    static double ChebyPoly(int Order, double NormFreq);
};

#endif
