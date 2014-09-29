/**********************************************************************

  Audacity: A Digital Audio Editor

  Matrix.cpp

  Dominic Mazzoni

**********************************************************************/

#include <stdlib.h>
#include <math.h>

#include <wx/defs.h>

#include "Matrix.h"

Vector::Vector()
{
   mCopy = false;
   mN = 0;
   mData = NULL;
}

Vector::Vector(int len, double *data, bool copy)
{
   mN = len;
   mCopy = copy;
   if (mCopy || !data) {
      mCopy = true;
      mData = new double[mN];
      int i;
      for(i=0; i<mN; i++)
         if (data)
            mData[i] = data[i];
         else
            mData[i] = 0.0;
   }
   else {
      mCopy = false;
      mData = data;
   }
}

Vector& Vector::operator=(const Vector &other)
{
   wxASSERT(Len() == other.Len());
   int i;
   for(i=0; i<Len(); i++)
      mData[i] = other.mData[i];
   return *this;
}

Vector::Vector(const Vector &other)
{
   CopyFrom(other);
}

void Vector::CopyFrom(const Vector &other)
{
   mN = other.Len();
   mCopy = true;
   mData = new double[mN];
   int i;
   for(i=0; i<mN; i++)
      mData[i] = other.mData[i];
}

Vector::~Vector()
{
   if (mCopy)
      delete[] mData;
}

Vector::Vector(int len, float *data)
{
   mCopy = true;
   mN = len;
   mData = new double[mN];
   int i;
   for(i=0; i<mN; i++)
      mData[i] = (double)data[i];
}

double Vector::Sum() const
{
   int i;
   double sum = 0.0;
   for(i=0; i<Len(); i++)
      sum += mData[i];
   return sum;
}

Matrix::Matrix(int rows, int cols, double **data)
{
   mRows = rows;
   mCols = cols;
   mRowVec = new Vector *[mRows];
   int i, j;
   for(i=0; i<mRows; i++) {
      mRowVec[i] = new Vector(mCols);
      for(j=0; j<mCols; j++) {
         if (data)
            (*this)[i][j] = data[i][j];
         else
            (*this)[i][j] = 0.0;
      }
   }
}

Matrix& Matrix::operator=(const Matrix &other)
{
   CopyFrom(other);
   return *this;
}

Matrix::Matrix(const Matrix &other)
{
   CopyFrom(other);
}

void Matrix::CopyFrom(const Matrix &other)
{
   mRows = other.mRows;
   mCols = other.mCols;
   mRowVec = new Vector *[mRows];
   int i;
   for(i=0; i<mRows; i++) {
      mRowVec[i] = new Vector(mCols);
      *mRowVec[i] = *other.mRowVec[i];
   }
}

Matrix::~Matrix()
{
   int i;
   for(i=0; i<mRows; i++)
      delete mRowVec[i];
   delete[] mRowVec;
}

void Matrix::SwapRows(int i, int j)
{
   Vector *tmp = mRowVec[i];
   mRowVec[i] = mRowVec[j];
   mRowVec[j] = tmp;
}

double Matrix::Sum() const
{
   int i, j;
   double sum = 0.0;
   for(i=0; i<Rows(); i++)
      for(j=0; j<Cols(); j++)
         sum += (*mRowVec[i])[j];
   return sum;
}

Matrix IdentityMatrix(int N)
{
   Matrix M(N, N);
   int i;
   for(i=0; i<N; i++)
      M[i][i] = 1.0;
   return M;
}

Vector operator+(const Vector &left, const Vector &right)
{
   wxASSERT(left.Len() == right.Len());
   Vector v(left.Len());
   int i;
   for(i=0; i<left.Len(); i++)
      v[i] = left[i] + right[i];
   return v;
}

Vector operator-(const Vector &left, const Vector &right)
{
   wxASSERT(left.Len() == right.Len());
   Vector v(left.Len());
   int i;
   for(i=0; i<left.Len(); i++)
      v[i] = left[i] - right[i];
   return v;
}

Vector operator*(const Vector &left, const Vector &right)
{
   wxASSERT(left.Len() == right.Len());
   Vector v(left.Len());
   int i;
   for(i=0; i<left.Len(); i++)
      v[i] = left[i] * right[i];
   return v;
}

Vector operator*(const Vector &left, double right)
{
   Vector v(left.Len());
   int i;
   for(i=0; i<left.Len(); i++)
      v[i] = left[i] * right;
   return v;
}

Vector VectorSubset(const Vector &other, int start, int len)
{
   Vector v(len);
   int i;
   for(i=0; i<len; i++)
      v[i] = other[start+i];
   return v;
}

Vector VectorConcatenate(const Vector& left, const Vector& right)
{
   Vector v(left.Len() + right.Len());
   int i;
   for(i=0; i<left.Len(); i++)
      v[i] = left[i];
   for(i=0; i<right.Len(); i++)
      v[i + left.Len()] = right[i];
   return v;
}

Vector operator*(const Vector &left, const Matrix &right)
{
   wxASSERT(left.Len() == right.Rows());
   Vector v(right.Cols());
   int i, j;
   for(i=0; i<right.Cols(); i++) {
      v[i] = 0.0;
      for(j=0; j<right.Rows(); j++)
         v[i] += left[j] * right[j][i];
   }
   return v;
}

Vector operator*(const Matrix &left, const Vector &right)
{
   wxASSERT(left.Cols() == right.Len());
   Vector v(left.Rows());
   int i, j;
   for(i=0; i<left.Rows(); i++) {
      v[i] = 0.0;
      for(j=0; j<left.Cols(); j++)
         v[i] += left[i][j] * right[j];
   }
   return v;
}

Matrix operator+(const Matrix &left, const Matrix &right)
{
   wxASSERT(left.Rows() == right.Rows());
   wxASSERT(left.Cols() == right.Cols());
   Matrix M(left.Rows(), left.Cols());
   int i, j;
   for(i=0; i<left.Rows(); i++)
      for(j=0; j<left.Cols(); j++)
         M[i][j] = left[i][j] + right[i][j];
   return M;
}

Matrix operator*(const Matrix &left, const double right)
{
   Matrix M(left.Rows(), left.Cols());
   int i, j;
   for(i=0; i<left.Rows(); i++)
      for(j=0; j<left.Cols(); j++)
         M[i][j] = left[i][j] * right;
   return M;
}

Matrix ScalarMultiply(const Matrix &left, const Matrix &right)
{
   wxASSERT(left.Rows() == right.Rows());
   wxASSERT(left.Cols() == right.Cols());
   Matrix M(left.Rows(), left.Cols());
   int i, j;
   for(i=0; i<left.Rows(); i++)
      for(j=0; j<left.Cols(); j++)
         M[i][j] = left[i][j] * right[i][j];
   return M;
}

Matrix MatrixMultiply(const Matrix &left, const Matrix &right)
{
   wxASSERT(left.Cols() == right.Rows());
   Matrix M(left.Rows(), right.Cols());
   int i, j, k;
   for(i=0; i<left.Rows(); i++)
      for(j=0; j<right.Cols(); j++) {
         M[i][j] = 0.0;
         for(k=0; k<left.Cols(); k++)
            M[i][j] += left[i][k] * right[k][j];
      }
   return M;
}

Matrix MatrixSubset(const Matrix &input,
                    int startRow, int numRows, int startCol, int numCols)
{
   Matrix M(numRows, numCols);
   int i, j;
   for(i=0; i<numRows; i++)
      for(j=0; j<numCols; j++)
         M[i][j] = input[startRow+i][startCol+j];
   return M;
}

Matrix MatrixConcatenateCols(const Matrix& left, const Matrix& right)
{
   wxASSERT(left.Rows() == right.Rows());
   Matrix M(left.Rows(), left.Cols() + right.Cols());
   int i, j;
   for(i=0; i<left.Rows(); i++) {
      for(j=0; j<left.Cols(); j++)
         M[i][j] = left[i][j];
      for(j=0; j<right.Cols(); j++)
         M[i][j+left.Cols()] = right[i][j];
   }
   return M;
}

Matrix TransposeMatrix(const Matrix& other)
{
   Matrix M(other.Cols(), other.Rows());
   int i, j;
   for(i=0; i<other.Rows(); i++)
      for(j=0; j<other.Cols(); j++)
         M[j][i] = other[i][j];
   return M;
}

bool InvertMatrix(const Matrix& input, Matrix& Minv)
{
   // Very straightforward implementation of
   // Gauss-Jordan elimination to invert a matrix.
   // Returns true if successful

   wxASSERT(input.Rows() == input.Cols());
   int N = input.Rows();
   int i, j, k;

   Matrix M = input;
   Minv = IdentityMatrix(N);

   // Do the elimination one column at a time
   for(i=0; i<N; i++) {
      // Pivot the row with the largest absolute value in
      // column i, into row i
      double absmax = 0.0;
      int argmax=0;

      for(j=i; j<N; j++)
         if (fabs(M[j][i]) > absmax) {
            absmax = fabs(M[j][i]);
            argmax = j;
         }

      // If no row has a nonzero value in that column,
      // the matrix is singular and we have to give up.
      if (absmax == 0)
         return false;

      if (i != argmax) {
         M.SwapRows(i, argmax);
         Minv.SwapRows(i, argmax);
      }

      // Divide this row by the value of M[i][i]
      double factor = 1.0 / M[i][i];
      M[i] = M[i] * factor;
      Minv[i] = Minv[i] * factor;

      // Eliminate the rest of the column
      for(j=0; j<N; j++) {
         if (j==i)
            continue;
         if (fabs(M[j][i]) > 0) {
            // Subtract a multiple of row i from row j
            double factor = M[j][i];
            for(k=0; k<N; k++) {
               M[j][k] -= (M[i][k] * factor);
               Minv[j][k] -= (Minv[i][k] * factor);
            }
         }
      }
   }

   return true;
}
