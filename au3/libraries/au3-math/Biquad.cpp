/**********************************************************************

Audacity: A Digital Audio Editor

Biquad.cpp

Norm C
Max Maisel

***********************************************************************/

#include "Biquad.h"

#include <cmath>
#include <wx/utils.h>

#define square(a) ((a) * (a))
#define PI M_PI

Biquad::Biquad()
{
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

void Biquad::Process(const float* pfIn, float* pfOut, int iNumSamples)
{
    for (int i = 0; i < iNumSamples; i++) {
        *pfOut++ = ProcessOne(*pfIn++);
    }
}

const double Biquad::s_fChebyCoeffs[MAX_Order][MAX_Order + 1] =
{
    // For Chebyshev polynomials of the first kind (see http://en.wikipedia.org/wiki/Chebyshev_polynomial)
    // Coeffs are in the order 0, 1, 2...9
    { 0,  1 },      // order 1
    { -1,  0,   2 },// order 2 etc.
    { 0, -3,   0,    4 },
    { 1,  0,  -8,    0,    8 },
    { 0,  5,   0,  -20,    0,   16 },
    { -1,  0,  18,    0,  -48,    0,   32 },
    { 0, -7,   0,   56,    0, -112,    0,   64 },
    { 1,  0, -32,    0,  160,    0, -256,    0,  128 },
    { 0,  9,   0, -120,    0,  432,    0, -576,    0,   256 },
    { -1,  0,  50,    0, -400,    0, 1120,    0, -1280,    0, 512 }
};

// order: filter order
// fn: nyquist frequency, i.e. half sample rate
// fc: cutoff frequency
// subtype: highpass or lowpass
ArrayOf<Biquad> Biquad::CalcButterworthFilter(int order, double fn, double fc, int subtype)
{
    ArrayOf<Biquad> pBiquad(size_t((order + 1) / 2), true);
    // Set up the coefficients in all the biquads
    double fNorm = fc / fn;
    if (fNorm >= 0.9999) {
        fNorm = 0.9999F;
    }
    double fC = tan(PI * fNorm / 2);
    double fDCPoleDistSqr = 1.0F;
    double fZPoleX, fZPoleY;

    if ((order & 1) == 0) {
        // Even order
        for (int iPair = 0; iPair < order / 2; iPair++) {
            double fSPoleX = fC * cos(PI - (iPair + 0.5) * PI / order);
            double fSPoleY = fC * sin(PI - (iPair + 0.5) * PI / order);
            BilinTransform(fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
            pBiquad[iPair].fNumerCoeffs [B0] = 1;
            if (subtype == kLowPass) { // LOWPASS
                pBiquad[iPair].fNumerCoeffs [B1] = 2;
            } else {
                pBiquad[iPair].fNumerCoeffs [B1] = -2;
            }
            pBiquad[iPair].fNumerCoeffs [B2] = 1;
            pBiquad[iPair].fDenomCoeffs [A1] = -2 * fZPoleX;
            pBiquad[iPair].fDenomCoeffs [A2] = square(fZPoleX) + square(fZPoleY);
            if (subtype == kLowPass) { // LOWPASS
                fDCPoleDistSqr *= Calc2D_DistSqr(1, 0, fZPoleX, fZPoleY);
            } else {
                fDCPoleDistSqr *= Calc2D_DistSqr(-1, 0, fZPoleX, fZPoleY); // distance from Nyquist
            }
        }
    } else {
        // Odd order - first do the 1st-order section
        double fSPoleX = -fC;
        double fSPoleY = 0;
        BilinTransform(fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
        pBiquad[0].fNumerCoeffs [B0] = 1;
        if (subtype == kLowPass) { // LOWPASS
            pBiquad[0].fNumerCoeffs [B1] = 1;
        } else {
            pBiquad[0].fNumerCoeffs [B1] = -1;
        }
        pBiquad[0].fNumerCoeffs [B2] = 0;
        pBiquad[0].fDenomCoeffs [A1] = -fZPoleX;
        pBiquad[0].fDenomCoeffs [A2] = 0;
        if (subtype == kLowPass) { // LOWPASS
            fDCPoleDistSqr = 1 - fZPoleX;
        } else {
            fDCPoleDistSqr = fZPoleX + 1; // dist from Nyquist
        }
        for (int iPair = 1; iPair <= order / 2; iPair++) {
            double fSPoleX = fC * cos(PI - iPair * PI / order);
            double fSPoleY = fC * sin(PI - iPair * PI / order);
            BilinTransform(fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
            pBiquad[iPair].fNumerCoeffs [B0] = 1;
            if (subtype == kLowPass) { // LOWPASS
                pBiquad[iPair].fNumerCoeffs [B1] = 2;
            } else {
                pBiquad[iPair].fNumerCoeffs [B1] = -2;
            }
            pBiquad[iPair].fNumerCoeffs [B2] = 1;
            pBiquad[iPair].fDenomCoeffs [A1] = -2 * fZPoleX;
            pBiquad[iPair].fDenomCoeffs [A2] = square(fZPoleX) + square(fZPoleY);
            if (subtype == kLowPass) { // LOWPASS
                fDCPoleDistSqr *= Calc2D_DistSqr(1, 0, fZPoleX, fZPoleY);
            } else {
                fDCPoleDistSqr *= Calc2D_DistSqr(-1, 0, fZPoleX, fZPoleY); // distance from Nyquist
            }
        }
    }
    pBiquad[0].fNumerCoeffs [B0] *= fDCPoleDistSqr / (1 << order);  // mult by DC dist from poles, divide by dist from zeroes
    pBiquad[0].fNumerCoeffs [B1] *= fDCPoleDistSqr / (1 << order);
    pBiquad[0].fNumerCoeffs [B2] *= fDCPoleDistSqr / (1 << order);

    return pBiquad;
}

// order: filter order
// fn: nyquist frequency, i.e. half sample rate
// fc: cutoff frequency
// ripple: passband ripple in dB
// subtype: highpass or lowpass
ArrayOf<Biquad> Biquad::CalcChebyshevType1Filter(int order, double fn, double fc, double ripple, int subtype)
{
    ArrayOf<Biquad> pBiquad(size_t((order + 1) / 2), true);
    // Set up the coefficients in all the biquads
    double fNorm = fc / fn;
    if (fNorm >= 0.9999) {
        fNorm = 0.9999F;
    }
    double fC = tan(PI * fNorm / 2);
    double fDCPoleDistSqr = 1.0F;
    double fZPoleX, fZPoleY;
    double fZZeroX;
    double beta = cos(fNorm * PI);

    double eps;
    eps = sqrt(pow(10.0, wxMax(0.001, ripple) / 10.0) - 1);
    double a;
    a = log(1 / eps + sqrt(1 / square(eps) + 1)) / order;
    // Assume even order to start
    for (int iPair = 0; iPair < order / 2; iPair++) {
        double fSPoleX = -fC* sinh(a) * sin((2 * iPair + 1) * PI / (2 * order));
        double fSPoleY = fC * cosh(a) * cos((2 * iPair + 1) * PI / (2 * order));
        BilinTransform(fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
        if (subtype == kLowPass) { // LOWPASS
            fZZeroX = -1;
            fDCPoleDistSqr = Calc2D_DistSqr(1, 0, fZPoleX, fZPoleY);
            fDCPoleDistSqr /= 2 * 2; // dist from zero at Nyquist
        } else {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv(beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -beta * fZPoleY, &fZPoleX, &fZPoleY);
            fZZeroX = 1;
            fDCPoleDistSqr = Calc2D_DistSqr(-1, 0, fZPoleX, fZPoleY);   // distance from Nyquist
            fDCPoleDistSqr /= 2 * 2; // dist from zero at Nyquist
        }
        pBiquad[iPair].fNumerCoeffs [B0] = fDCPoleDistSqr;
        pBiquad[iPair].fNumerCoeffs [B1] = -2 * fZZeroX * fDCPoleDistSqr;
        pBiquad[iPair].fNumerCoeffs [B2] = fDCPoleDistSqr;
        pBiquad[iPair].fDenomCoeffs [A1] = -2 * fZPoleX;
        pBiquad[iPair].fDenomCoeffs [A2] = square(fZPoleX) + square(fZPoleY);
    }
    if ((order & 1) == 0) {
        double fTemp = DB_TO_LINEAR(-wxMax(0.001, ripple));    // at DC the response is down R dB (for even-order)
        pBiquad[0].fNumerCoeffs [B0] *= fTemp;
        pBiquad[0].fNumerCoeffs [B1] *= fTemp;
        pBiquad[0].fNumerCoeffs [B2] *= fTemp;
    } else {
        // Odd order - now do the 1st-order section
        double fSPoleX = -fC* sinh(a);
        double fSPoleY = 0;
        BilinTransform(fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
        if (subtype == kLowPass) { // LOWPASS
            fZZeroX = -1;
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr(1, 0, fZPoleX, fZPoleY));
            fDCPoleDistSqr /= 2; // dist from zero at Nyquist
        } else {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv(beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -beta * fZPoleY, &fZPoleX, &fZPoleY);
            fZZeroX = 1;
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr(-1, 0, fZPoleX, fZPoleY));   // distance from Nyquist
            fDCPoleDistSqr /= 2; // dist from zero at Nyquist
        }
        pBiquad[(order - 1) / 2].fNumerCoeffs [B0] = fDCPoleDistSqr;
        pBiquad[(order - 1) / 2].fNumerCoeffs [B1] = -fZZeroX * fDCPoleDistSqr;
        pBiquad[(order - 1) / 2].fNumerCoeffs [B2] = 0;
        pBiquad[(order - 1) / 2].fDenomCoeffs [A1] = -fZPoleX;
        pBiquad[(order - 1) / 2].fDenomCoeffs [A2] = 0;
    }
    return pBiquad;
}

// order: filter order
// fn: nyquist frequency, i.e. half sample rate
// fc: cutoff frequency
// ripple: stopband ripple in dB
// subtype: highpass or lowpass
ArrayOf<Biquad> Biquad::CalcChebyshevType2Filter(int order, double fn, double fc, double ripple, int subtype)
{
    ArrayOf<Biquad> pBiquad(size_t((order + 1) / 2), true);
    // Set up the coefficients in all the biquads
    double fNorm = fc / fn;
    if (fNorm >= 0.9999) {
        fNorm = 0.9999F;
    }
    double fC = tan(PI * fNorm / 2);
    double fDCPoleDistSqr = 1.0F;
    double fZPoleX, fZPoleY;
    double fZZeroX, fZZeroY;
    double beta = cos(fNorm * PI);

    double fSZeroX, fSZeroY;
    double fSPoleX, fSPoleY;
    double eps = DB_TO_LINEAR(-wxMax(0.001, ripple));
    double a = log(1 / eps + sqrt(1 / square(eps) + 1)) / order;

    // Assume even order
    for (int iPair = 0; iPair < order / 2; iPair++) {
        ComplexDiv(fC, 0, -sinh(a) * sin((2 * iPair + 1) * PI / (2 * order)),
                   cosh(a) * cos((2 * iPair + 1) * PI / (2 * order)),
                   &fSPoleX, &fSPoleY);
        BilinTransform(fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
        fSZeroX = 0;
        fSZeroY = fC / cos(((2 * iPair) + 1) * PI / (2 * order));
        BilinTransform(fSZeroX, fSZeroY, &fZZeroX, &fZZeroY);

        if (subtype == kLowPass) { // LOWPASS
            fDCPoleDistSqr = Calc2D_DistSqr(1, 0, fZPoleX, fZPoleY);
            fDCPoleDistSqr /= Calc2D_DistSqr(1, 0, fZZeroX, fZZeroY);
        } else {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv(beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -beta * fZPoleY, &fZPoleX, &fZPoleY);
            ComplexDiv(beta - fZZeroX, -fZZeroY, 1 - beta * fZZeroX, -beta * fZZeroY, &fZZeroX, &fZZeroY);
            fDCPoleDistSqr = Calc2D_DistSqr(-1, 0, fZPoleX, fZPoleY);   // distance from Nyquist
            fDCPoleDistSqr /= Calc2D_DistSqr(-1, 0, fZZeroX, fZZeroY);
        }
        pBiquad[iPair].fNumerCoeffs [B0] = fDCPoleDistSqr;
        pBiquad[iPair].fNumerCoeffs [B1] = -2 * fZZeroX * fDCPoleDistSqr;
        pBiquad[iPair].fNumerCoeffs [B2] = (square(fZZeroX) + square(fZZeroY)) * fDCPoleDistSqr;
        pBiquad[iPair].fDenomCoeffs [A1] = -2 * fZPoleX;
        pBiquad[iPair].fDenomCoeffs [A2] = square(fZPoleX) + square(fZPoleY);
    }
    // Now, if it's odd order, we have one more to do
    if (order & 1) {
        int iPair = (order - 1) / 2; // we'll do it as a biquad, but it's just first-order
        ComplexDiv(fC, 0, -sinh(a) * sin((2 * iPair + 1) * PI / (2 * order)),
                   cosh(a) * cos((2 * iPair + 1) * PI / (2 * order)),
                   &fSPoleX, &fSPoleY);
        BilinTransform(fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
        fZZeroX = -1;   // in the s-plane, the zero is at infinity
        fZZeroY = 0;
        if (subtype == kLowPass) { // LOWPASS
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr(1, 0, fZPoleX, fZPoleY));
            fDCPoleDistSqr /= 2;
        } else {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv(beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -fZPoleY, &fZPoleX, &fZPoleY);
            fZZeroX = 1;
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr(-1, 0, fZPoleX, fZPoleY));   // distance from Nyquist
            fDCPoleDistSqr /= 2;
        }
        pBiquad[iPair].fNumerCoeffs [B0] = fDCPoleDistSqr;
        pBiquad[iPair].fNumerCoeffs [B1] = -fZZeroX * fDCPoleDistSqr;
        pBiquad[iPair].fNumerCoeffs [B2] = 0;
        pBiquad[iPair].fDenomCoeffs [A1] = -fZPoleX;
        pBiquad[iPair].fDenomCoeffs [A2] = 0;
    }
    return pBiquad;
}

void Biquad::ComplexDiv(double fNumerR, double fNumerI, double fDenomR, double fDenomI,
                        double* pfQuotientR, double* pfQuotientI)
{
    double fDenom = square(fDenomR) + square(fDenomI);
    *pfQuotientR = (fNumerR * fDenomR + fNumerI * fDenomI) / fDenom;
    *pfQuotientI = (fNumerI * fDenomR - fNumerR * fDenomI) / fDenom;
}

bool Biquad::BilinTransform(double fSX, double fSY, double* pfZX, double* pfZY)
{
    double fDenom = square(1 - fSX) + square(fSY);
    *pfZX = (1 - square(fSX) - square(fSY)) / fDenom;
    *pfZY = 2 * fSY / fDenom;
    return true;
}

float Biquad::Calc2D_DistSqr(double fX1, double fY1, double fX2, double fY2)
{
    return square(fX1 - fX2) + square(fY1 - fY2);
}

double Biquad::ChebyPoly(int Order, double NormFreq)   // NormFreq = 1 at the f0 point (where response is R dB down)
{
    // Calc cosh (Order * acosh (NormFreq));
    double x = 1;
    double fSum = 0;
    wxASSERT(Order >= MIN_Order && Order <= MAX_Order);
    for (int i = 0; i <= Order; i++) {
        fSum += s_fChebyCoeffs [Order - 1][i] * x;
        x *= NormFreq;
    }
    return fSum;
}
