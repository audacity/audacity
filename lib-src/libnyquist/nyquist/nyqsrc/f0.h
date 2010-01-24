#ifndef _FLOAT_H
#define _FLOAT_H

float f0_estimate(float *samples, int n, int m, float threshold, float *results, float *min);

float best_f0(float *samples, int n, int m, float threshold, int Tmax);

#endif
