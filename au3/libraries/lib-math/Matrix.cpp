/**********************************************************************

  Audacity: A Digital Audio Editor

  Matrix.cpp

  Dominic Mazzoni

**********************************************************************/

#include "Matrix.h"

#include <stdlib.h>
#include <math.h>

#include <wx/defs.h>

Vector::Vector()
{
}

Vector::Vector(unsigned len, double *data)
   : mN{ len }
   , mData(len)
{
   if (data)
      std::copy(data, data + len, mData.get());
   else
      std::fill(mData.get(), mData.get() + len, 0.0);
}

Vector::Vector(unsigned len, float *data)
   : mN{ len }
   , mData{ len }
{
   if (data)
      std::copy(data, data + len, mData.get());
   else
      std::fill(mData.get(), mData.get() + len, 0.0);
}

Vector& Vector::operator=(const Vector &other)
{
   wxASSERT(Len() == other.Len());
   std::copy(other.mData.get(), other.mData.get() + mN, mData.get());
   return *this;
}

Vector::Vector(const Vector &other)
   : mN{ other.Len() }
   , mData{ mN }
{
   std::copy(other.mData.get(), other.mData.get() + mN, mData.get());
}

Vector::~Vector()
{
}

void Vector::Reinit(unsigned len)
{
   Vector temp(len);
   Swap(temp);
}

void Vector::Swap(Vector &that)
{
   std::swap(mN, that.mN);
   mData.swap(that.mData);
}

double Vector::Sum() const
{
   double sum = 0.0;
   for(unsigned i = 0; i < Len(); i++)
      sum += mData[i];
   return sum;
}

Matrix::Matrix(unsigned rows, unsigned cols, double **data)
   : mRows{ rows }
   , mCols{ cols }
   , mRowVec{ mRows }
{
   for(unsigned i = 0; i < mRows; i++) {
      mRowVec[i].Reinit( mCols );
      for(unsigned j = 0; j < mCols; j++) {
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
   mRowVec.reinit(mRows);
   for (unsigned i = 0; i < mRows; i++) {
      mRowVec[i].Reinit( mCols );
      mRowVec[i] = other.mRowVec[i];
   }
}

Matrix::~Matrix()
{
}

void Matrix::SwapRows(unsigned i, unsigned j)
{
   mRowVec[i].Swap(mRowVec[j]);
}

Matrix IdentityMatrix(unsigned N)
{
   Matrix M(N, N);
   for(unsigned i = 0; i < N; i++)
      M[i][i] = 1.0;
   return M;
}

Vector operator+(const Vector &left, const Vector &right)
{
   wxASSERT(left.Len() == right.Len());
   Vector v(left.Len());
   for(unsigned i = 0; i < left.Len(); i++)
      v[i] = left[i] + right[i];
   return v;
}

Vector operator-(const Vector &left, const Vector &right)
{
   wxASSERT(left.Len() == right.Len());
   Vector v(left.Len());
   for(unsigned i = 0; i < left.Len(); i++)
      v[i] = left[i] - right[i];
   return v;
}

Vector operator*(const Vector &left, const Vector &right)
{
   wxASSERT(left.Len() == right.Len());
   Vector v(left.Len());
   for(unsigned i = 0; i < left.Len(); i++)
      v[i] = left[i] * right[i];
   return v;
}

Vector operator*(const Vector &left, double right)
{
   Vector v(left.Len());
   for(unsigned i = 0; i < left.Len(); i++)
      v[i] = left[i] * right;
   return v;
}

Vector VectorSubset(const Vector &other, unsigned start, unsigned len)
{
   Vector v(len);
   for(unsigned i = 0; i < len; i++)
      v[i] = other[start+i];
   return v;
}

Vector VectorConcatenate(const Vector& left, const Vector& right)
{
   Vector v(left.Len() + right.Len());
   for(unsigned i = 0; i < left.Len(); i++)
      v[i] = left[i];
   for(unsigned i = 0; i < right.Len(); i++)
      v[i + left.Len()] = right[i];
   return v;
}

Vector operator*(const Vector &left, const Matrix &right)
{
   wxASSERT(left.Len() == right.Rows());
   Vector v(right.Cols());
   for(unsigned i = 0; i < right.Cols(); i++) {
      v[i] = 0.0;
      for(unsigned j = 0; j < right.Rows(); j++)
         v[i] += left[j] * right[j][i];
   }
   return v;
}

Vector operator*(const Matrix &left, const Vector &right)
{
   wxASSERT(left.Cols() == right.Len());
   Vector v(left.Rows());
   for(unsigned i = 0; i < left.Rows(); i++) {
      v[i] = 0.0;
      for(unsigned j = 0; j < left.Cols(); j++)
         v[i] += left[i][j] * right[j];
   }
   return v;
}

Matrix operator+(const Matrix &left, const Matrix &right)
{
   wxASSERT(left.Rows() == right.Rows());
   wxASSERT(left.Cols() == right.Cols());
   Matrix M(left.Rows(), left.Cols());
   for(unsigned i = 0; i < left.Rows(); i++)
      for(unsigned j = 0; j < left.Cols(); j++)
         M[i][j] = left[i][j] + right[i][j];
   return M;
}

Matrix operator*(const Matrix &left, const double right)
{
   Matrix M(left.Rows(), left.Cols());
   for(unsigned i = 0; i < left.Rows(); i++)
      for(unsigned j = 0; j < left.Cols(); j++)
         M[i][j] = left[i][j] * right;
   return M;
}

Matrix ScalarMultiply(const Matrix &left, const Matrix &right)
{
   wxASSERT(left.Rows() == right.Rows());
   wxASSERT(left.Cols() == right.Cols());
   Matrix M(left.Rows(), left.Cols());
   for(unsigned i = 0; i < left.Rows(); i++)
      for(unsigned j = 0; j < left.Cols(); j++)
         M[i][j] = left[i][j] * right[i][j];
   return M;
}

Matrix MatrixMultiply(const Matrix &left, const Matrix &right)
{
   wxASSERT(left.Cols() == right.Rows());
   Matrix M(left.Rows(), right.Cols());
   for(unsigned i = 0; i < left.Rows(); i++)
      for(unsigned j = 0; j < right.Cols(); j++) {
         M[i][j] = 0.0;
         for(unsigned k = 0; k < left.Cols(); k++)
            M[i][j] += left[i][k] * right[k][j];
      }
   return M;
}

Matrix MatrixSubset(const Matrix &input,
                    unsigned startRow, unsigned numRows,
                    unsigned startCol, unsigned numCols)
{
   Matrix M(numRows, numCols);
   for(unsigned i = 0; i < numRows; i++)
      for(unsigned j = 0; j < numCols; j++)
         M[i][j] = input[startRow+i][startCol+j];
   return M;
}

Matrix MatrixConcatenateCols(const Matrix& left, const Matrix& right)
{
   wxASSERT(left.Rows() == right.Rows());
   Matrix M(left.Rows(), left.Cols() + right.Cols());
   for(unsigned i = 0; i < left.Rows(); i++) {
      for(unsigned j = 0; j < left.Cols(); j++)
         M[i][j] = left[i][j];
      for(unsigned j = 0; j < right.Cols(); j++)
         M[i][j+left.Cols()] = right[i][j];
   }
   return M;
}

Matrix TransposeMatrix(const Matrix& other)
{
   Matrix M(other.Cols(), other.Rows());
   for(unsigned i = 0; i < other.Rows(); i++)
      for(unsigned j = 0; j < other.Cols(); j++)
         M[j][i] = other[i][j];
   return M;
}

bool InvertMatrix(const Matrix& input, Matrix& Minv)
{
   // Very straightforward implementation of
   // Gauss-Jordan elimination to invert a matrix.
   // Returns true if successful

   wxASSERT(input.Rows() == input.Cols());
   auto N = input.Rows();

   Matrix M = input;
   Minv = IdentityMatrix(N);

   // Do the elimination one column at a time
   for(unsigned i = 0; i < N; i++) {
      // Pivot the row with the largest absolute value in
      // column i, into row i
      double absmax = 0.0;
      unsigned int argmax = 0;

      for(unsigned j = i; j < N; j++)
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
      for(unsigned j = 0; j < N; j++) {
         if (j == i)
            continue;
         if (fabs(M[j][i]) > 0) {
            // Subtract a multiple of row i from row j
            factor = M[j][i];
            for(unsigned k = 0; k < N; k++) {
               M[j][k] -= (M[i][k] * factor);
               Minv[j][k] -= (Minv[i][k] * factor);
            }
         }
      }
   }

   return true;
}
