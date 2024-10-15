// Regression is a class to compute a linear regression
// 
// call point(x, y) to add a data point
// call regress() to compute the regression
// call f(x) to evaluate the linear regression at x
// call f_inv(y) to evaluate the inverse linear regression at y
// to restart with new points, call init()
// regress() can be called after each point()
//
// other forms of regression should be added. This one does
// standard least squares regression

class Regression {
    float sumxx; // sum of x^2
    float sumyy; // sum of y^2
    float sumxy; // sum of xy
    float sumx;  // sum of x
    float sumy;  // sum of y
    int n;       // number of points
    float a, b;  // regression line is a + b*x
 public:
    Regression();
    void init();
    void point(float x, float y);
    void regress();
    float f(float x);
    float f_inv(float y);
};

