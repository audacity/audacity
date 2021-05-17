// f0 -- frequency estimation

#include <stdio.h>



// Estimate a local minimum (or maximum) using parabolic 
// interpolation. The parabola is defined by the points 
// (x1,y1),(x2,y2), and (x3,y3).
float parabolic_interp(float x1, float x2, float x3, float y1, float y2, float y3, float *min)
{
  float a, b, c;
  float pos;

  //  y1=a*x1^2+b*x1+c
  //  y2=a*x2^2+b*x2+c
  //  y3=a*x3^2+b*x3+c

  //  y1-y2=a*(x1^2-x2^2)+b*(x1-x2)
  //  y2-y3=a*(x2^2-x3^2)+b*(x2-x3)

  //  (y1-y2)/(x1-x2)=a*(x1+x2)+b
  //  (y2-y3)/(x2-x3)=a*(x2+x3)+b

  a= ((y1-y2)/(x1-x2)-(y2-y3)/(x2-x3))/(x1-x3);
  b= (y1-y2)/(x1-x2) - a*(x1+x2);
  c= y1-a*x1*x1-b*x1;

  *min= c;

  // dy/dx = 2a*x + b = 0
  
  pos= -b/2.0F/a;

  return pos;

}



float f0_estimate(float *samples, int n, int m, float threshold, float *results, float *min)
    // samples is a buffer of samples
    // n is the number of samples, equals twice longest period, must be even
    // m is the shortest period in samples
    // results is an array of size n/2 - m + 1, the number of different lags
{
    // work from the middle of the buffer:
    int middle = n / 2;
    int i, j; // loop counters
    // how many different lags do we compute?
    float left_energy = 0;
    float right_energy = 0;
    // for each window, we keep the energy so we can compute the next one 
    // incrementally. First, we need to compute the energies for lag m-1:
    for (i = 0; i < m - 1; i++) {
        float left = samples[middle - 1 - i];
        left_energy += left * left;
        float right = samples[middle + i];
        right_energy += right * right;
    }
    for (i = m; i <= middle; i++) {
        // i is the lag and the length of the window
        // compute the energy for left and right
        float left = samples[middle - i];
        left_energy += left * left;
        float right = samples[middle - 1 + i];
 
        right_energy += right * right;
        //  compute the autocorrelation
        float auto_corr = 0;
        for (j = 0; j < i; j++) {
            auto_corr += samples[middle - i + j] * samples[middle + j];
        }
        float non_periodic = (left_energy + right_energy - 2 * auto_corr);// / i;
        results[i - m] = non_periodic;

    }


    // normalize by the cumulative sum
    float cum_sum=0.0;
    for (i = m; i <= middle; i++) {
      cum_sum+=results[i-m];
      results[i-m]=results[i-m]/(cum_sum/(i-m+1));

    }

    int min_i=m;  // value of initial estimate
    for (i = m; i <= middle; i++) {
      if (results[i - m] < threshold) {
	min_i=i;
	break;
      } else if (results[i-m]<results[min_i-m])
	min_i=i;

    }
    


    // use parabolic interpolation to improve estimate
    float freq;
    if (i>m && i<middle) {
      freq=parabolic_interp((float)(min_i-1),(float)(min_i),(float)(min_i+1),
      				results[min_i-1-m],results[min_i-m],results[min_i+1-m], min);
      //freq=(float)min_i;
      printf("%d %f\n",min_i,freq);
    } else {
      freq=(float)min_i;
      *min=results[min_i-m];
    }
    return freq;
}



float best_f0(float *samples, int n, int m, float threshold, int Tmax)
 // samples is a buffer of samples
 // n is the number of samples, equals twice longest period plus Tmax, must be even
 // m is the shortest period in samples
 // threshold is the 
 // results is an array of size n/2 - m + 1, the number of different lags
  // Tmax is the length of the search 
{
  float* results=new float[n/2-m+1];
  float min=10000000.0;
  float temp;
  float best_f0;
  float f0;

  for (int i=0; i<Tmax; i++) {
    f0=f0_estimate(&samples[i], n, m, threshold, results, &temp);
    if (temp<min) {
      min=temp;
      best_f0=f0;
    }
  }
  delete[](results);
  return best_f0;
}
