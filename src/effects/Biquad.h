#ifndef __BIQUAD_H__
#define __BIQUAD_H__

#if 0
//initialisations not supported in MSVC 2013.
//Gives error C2905
// Do not make conditional on compiler.
typedef struct {
   float* pfIn {};
   float* pfOut {};
   float fNumerCoeffs [3] { 1.0f, 0.0f, 0.0f };	// B0 B1 B2
   float fDenomCoeffs [2] { 0.0f, 0.0f };	// A1 A2
   float fPrevIn {};
   float fPrevPrevIn {};
   float fPrevOut {};
   float fPrevPrevOut {};
} BiquadStruct;
#else
// WARNING: This structure may need initialisation.
typedef struct {
   float* pfIn;
   float* pfOut;
   float fNumerCoeffs [3];	// B0 B1 B2
   float fDenomCoeffs [2];	// A1 A2
   float fPrevIn;
   float fPrevPrevIn;
   float fPrevOut;
   float fPrevPrevOut;
} BiquadStruct;
#endif



void Biquad_Process (BiquadStruct* pBQ, int iNumSamples);
void ComplexDiv (float fNumerR, float fNumerI, float fDenomR, float fDenomI, float* pfQuotientR, float* pfQuotientI);
bool BilinTransform (float fSX, float fSY, float* pfZX, float* pfZY);
float Calc2D_DistSqr (float fX1, float fY1, float fX2, float fY2);

#endif
