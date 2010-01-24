/*********************
This matrix transpose is in a seperate file because it should always be double precision.
*********************/
#include "dxpose.h"
void dxpose(xdouble *indata, long iRsiz, xdouble *outdata, long oRsiz, long Nrows, long Ncols){
/* not in-place double precision matrix transpose	*/
/* INPUTS */
/* *indata = input data array	*/
/* iRsiz = offset to between rows of input data array	*/
/* oRsiz = offset to between rows of output data array	*/
/* Nrows = number of rows in input data array	*/
/* Ncols = number of columns in input data array	*/
/* OUTPUTS */
/* *outdata = output data array	*/

xdouble	*irow; 		/* pointer to input row start */
xdouble	*ocol; 		/* pointer to output col start */
xdouble	*idata; 	/* pointer to input data */
xdouble	*odata; 	/* pointer to output data */
long 	RowCnt;		/* row counter */
long 	ColCnt;		/* col counter */
xdouble	T0; 		/* data storage */
xdouble	T1; 		/* data storage */
xdouble	T2; 		/* data storage */
xdouble	T3; 		/* data storage */
xdouble	T4; 		/* data storage */
xdouble	T5; 		/* data storage */
xdouble	T6; 		/* data storage */
xdouble	T7; 		/* data storage */
const long inRsizd1 = iRsiz;
const long inRsizd2 = 2*iRsiz;
const long inRsizd3 = inRsizd2+iRsiz;
const long inRsizd4 = 4*iRsiz;
const long inRsizd5 = inRsizd3+inRsizd2;
const long inRsizd6 = inRsizd4+inRsizd2;
const long inRsizd7 = inRsizd4+inRsizd3;
const long inRsizd8 = 8*iRsiz;

ocol = outdata;
irow = indata;
for (RowCnt=Nrows/8; RowCnt>0; RowCnt--){
	idata = irow;
	odata = ocol;
	for (ColCnt=Ncols; ColCnt>0; ColCnt--){
		T0 = *idata;
		T1 = *(idata+inRsizd1);
		T2 = *(idata+inRsizd2);
		T3 = *(idata+inRsizd3);
		T4 = *(idata+inRsizd4);
		T5 = *(idata+inRsizd5);
		T6 = *(idata+inRsizd6);
		T7 = *(idata+inRsizd7);
		*odata = T0;
		*(odata+1) = T1;
		*(odata+2) = T2;
		*(odata+3) = T3;
		*(odata+4) = T4;
		*(odata+5) = T5;
		*(odata+6) = T6;
		*(odata+7) = T7;
		idata++;
		odata += oRsiz;
	}
	irow += inRsizd8;
	ocol += 8;
}
if (Nrows%8 != 0){
	for (ColCnt=Ncols; ColCnt>0; ColCnt--){
		idata = irow++;
		odata = ocol;
		ocol += oRsiz;
		for (RowCnt=Nrows%8; RowCnt>0; RowCnt--){
			T0 = *idata;
			*odata++ = T0;
			idata += iRsiz;
		}
	}
}
}
