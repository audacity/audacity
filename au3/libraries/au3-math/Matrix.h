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

#include "SampleFormat.h"

class Matrix;

class Vector
{
public:
    Vector();
    Vector(const Vector& copyFrom);
    Vector(unsigned len, double* data=NULL);
    Vector(unsigned len, float* data);
    Vector& operator=(const Vector& other);
    ~Vector();

    void Reinit(unsigned len);
    void Swap(Vector& that);

    inline double& operator[](unsigned i) { return mData[i]; }
    inline double operator[](unsigned i) const { return mData[i]; }
    inline unsigned Len() const { return mN; }

    double Sum() const;

private:
    unsigned mN{ 0 };
    Doubles mData;
};

class Matrix
{
public:
    Matrix(const Matrix& copyFrom);
    Matrix(unsigned rows, unsigned cols, double** data=NULL);
    ~Matrix();

    Matrix& operator=(const Matrix& other);

    inline Vector& operator[](unsigned i) { return mRowVec[i]; }
    inline Vector& operator[](unsigned i) const { return mRowVec[i]; }
    inline unsigned Rows() const { return mRows; }
    inline unsigned Cols() const { return mCols; }

    void SwapRows(unsigned i, unsigned j);

private:
    void CopyFrom(const Matrix& other);

    unsigned mRows;
    unsigned mCols;
    ArrayOf<Vector> mRowVec;
};

bool InvertMatrix(const Matrix& input, Matrix& Minv);

Matrix TransposeMatrix(const Matrix& M);

Matrix IdentityMatrix(unsigned N);

Vector operator+(const Vector& left, const Vector& right);
Vector operator-(const Vector& left, const Vector& right);
Vector operator*(const Vector& left, const Vector& right);
Vector operator*(const Vector& left, double right);

Vector VectorSubset(const Vector& other, unsigned start, unsigned len);
Vector VectorConcatenate(const Vector& left, const Vector& right);

Vector operator*(const Vector& left, const Matrix& right);
Vector operator*(const Matrix& left, const Vector& right);

Matrix operator+(const Matrix& left, const Matrix& right);
Matrix operator*(const Matrix& left, const double right);

// No operator* on matrices due to ambiguity
Matrix ScalarMultiply(const Matrix& left, const Matrix& right);
Matrix MatrixMultiply(const Matrix& left, const Matrix& right);

Matrix MatrixSubset(const Matrix& M, unsigned startRow, unsigned numRows, unsigned startCol, unsigned numCols);

Matrix MatrixConcatenateCols(const Matrix& left, const Matrix& right);

bool InvertMatrix(const Matrix& M, Matrix& Minv);

#endif // __AUDACITY_MATRIX__
