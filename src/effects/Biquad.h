#ifndef __BIQUAD_H__
#define __BIQUAD_H__
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
void Biquad_Process (BiquadStruct* pBQ, int iNumSamples);
void ComplexDiv (float fNumerR, float fNumerI, float fDenomR, float fDenomI, float* pfQuotientR, float* pfQuotientI);
bool BilinTransform (float fSX, float fSY, float* pfZX, float* pfZY);
float Calc2D_DistSqr (float fX1, float fY1, float fX2, float fY2);

#endif
