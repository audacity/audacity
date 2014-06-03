/**********************************************************************

  Audacity: A Digital Audio Editor

  Matrix.h

  Dominic Mazzoni

*******************************************************************//*!

\file Matrix.h
\brief Holds both the Matrix and Vector classes, supporting
  linear algebra operations, including matrix inversion.
  Used by InterpolateAudio.

\class Matrix
\brief Holds a matrix of doubles and supports arithmetic, subsetting,
  and matrix inversion.  Used by InterpolateAudio.

\class Vector
\brief Holds a matrix of doubles and supports arithmetic operations,
  including Vector-Matrix operations.  Used by InterpolateAudio.

*//*******************************************************************/

#ifndef __AUDACITY_MATRIX__
#define __AUDACITY_MATRIX__

class Matrix;

class Vector
{
 public:
   Vector();
   Vector(const Vector& copyFrom);
   Vector(int len, double *data=NULL, bool copy=true);
   Vector(int len, float *data);
   Vector& operator=(const Vector &other);
   virtual ~Vector();

   inline double& operator[](int i) { return mData[i]; }
   inline double operator[](int i) const { return mData[i]; }
   inline int Len() const { return mN; }

   double Sum() const;

 private:
   void CopyFrom(const Vector &other);

   int mN;
   double *mData;
   bool mCopy;
};

class Matrix
{
 public:
   Matrix(const Matrix& copyFrom);
   Matrix(int rows, int cols, double **data=NULL);
   virtual ~Matrix();

   Matrix& operator=(const Matrix& other);

   inline Vector& operator[](int i) { return *mRowVec[i]; }
   inline Vector& operator[](int i) const { return *mRowVec[i]; }
   inline int Rows() const { return mRows; }
   inline int Cols() const { return mCols; }

   void SwapRows(int i, int j);

   double Sum() const;

 private:
   void CopyFrom(const Matrix& other);

   int mRows;
   int mCols;
   Vector **mRowVec;
};

bool InvertMatrix(const Matrix& input, Matrix& Minv);

Matrix TransposeMatrix(const Matrix& M);

Matrix IdentityMatrix(int N);

Vector operator+(const Vector &left, const Vector &right);
Vector operator-(const Vector &left, const Vector &right);
Vector operator*(const Vector &left, const Vector &right);
Vector operator*(const Vector &left, double right);

Vector VectorSubset(const Vector &other, int start, int len);
Vector VectorConcatenate(const Vector& left, const Vector& right);

Vector operator*(const Vector &left, const Matrix &right);
Vector operator*(const Matrix &left, const Vector &right);

Matrix operator+(const Matrix &left, const Matrix &right);
Matrix operator*(const Matrix &left, const double right);

// No operator* on matrices due to ambiguity
Matrix ScalarMultiply(const Matrix &left, const Matrix &right);
Matrix MatrixMultiply(const Matrix &left, const Matrix &right);

Matrix MatrixSubset(const Matrix &M,
                    int startRow, int numRows, int startCol, int numCols);

Matrix MatrixConcatenateCols(const Matrix& left, const Matrix& right);

bool InvertMatrix(const Matrix& M, Matrix& Minv);

#endif // __AUDACITY_MATRIX__
