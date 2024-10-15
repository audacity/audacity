// Regression is a class to compute a linear regression
// 
#include "regression.h"


Regression::Regression()
{
    init();
}

void Regression::init()
{
    sumxx = 0;
    sumyy = 0;
    sumxy = 0;
    sumx = 0;
    sumy = 0;
    n = 0;
}

void Regression::point(float x, float y)
{
    sumx = sumx + x;
    sumy = sumy + y;
    sumxx = sumxx + x * x;
    sumyy = sumyy + y * y;
    sumxy = sumxy + x * y;
    n = n + 1;
}

void Regression::regress()
{
    float sxx = sumxx - sumx * sumx / n;
    float sxy = sumxy - sumx * sumy / n;
    b = sxy / sxx;
    a = (sumy - b * sumx) / n;
}


float Regression::f(float x)
{
    return a + b * x;
}


float Regression::f_inv(float y)
{
    return (y - a) / b;
}
