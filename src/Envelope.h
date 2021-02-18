/**********************************************************************

  Audacity: A Digital Audio Editor

  Envelope.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_ENVELOPE__
#define __AUDACITY_ENVELOPE__

#include <stdlib.h>
#include <algorithm>
#include <vector>

#include "XMLTagHandler.h"

class wxRect;
class wxMouseEvent;
class wxTextFile;

class Envelope;
class EnvPoint;

class ZoomInfo;

class EnvPoint final : public XMLTagHandler {

public:
   EnvPoint() {}
   inline EnvPoint( double t, double val ) : mT{ t }, mVal{ val } {}

   double GetT() const { return mT; }
   void SetT(double t) { mT = t; }
   double GetVal() const { return mVal; }
   inline void SetVal( Envelope *pEnvelope, double val );

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override
   {
      if (!wxStrcmp(tag, wxT("controlpoint"))) {
         while (*attrs) {
            const wxChar *attr = *attrs++;
            const wxChar *value = *attrs++;
            if (!wxStrcmp(attr, wxT("t")))
               SetT(Internat::CompatibleToDouble(value));
            else if (!wxStrcmp(attr, wxT("val")))
               SetVal( nullptr, Internat::CompatibleToDouble(value) );
         }
         return true;
      }
      else
         return false;
   }

   XMLTagHandler *HandleXMLChild(const wxChar * WXUNUSED(tag)) override
   {
      return NULL;
   }

private:
   double mT {};
   double mVal {};

};

typedef std::vector<EnvPoint> EnvArray;
struct TrackPanelDrawingContext;

class AUDACITY_DLL_API Envelope /* not final */ : public XMLTagHandler {
public:
   // Envelope can define a piecewise linear function, or piecewise exponential.
   Envelope(bool exponential, double minValue, double maxValue, double defaultValue);

   Envelope(const Envelope &orig);

   // Create from a subrange of another envelope.
   Envelope(const Envelope &orig, double t0, double t1);

   void Initialize(int numPoints);

   virtual ~Envelope();

   /** \brief Get many envelope points for pixel columns at once,
    * but don't assume uniform time per pixel.
   */
   static void GetValues
      ( const Envelope &env,
        double aligned_time, double sampleDur,
        double *buffer, int bufferLen, int leftOffset,
        const ZoomInfo &zoomInfo);

   // Return true if violations of point ordering invariants were detected
   // and repaired
   bool ConsistencyCheck();

   double GetOffset() const { return mOffset; }
   double GetTrackLen() const { return mTrackLen; }

   bool GetExponential() const { return mDB; }
   void SetExponential(bool db) { mDB = db; }

   void Flatten(double value);

   double GetMinValue() const { return mMinValue; }
   double GetMaxValue() const { return mMaxValue; }
   void SetRange(double minValue, double maxValue);

   double ClampValue(double value) { return std::max(mMinValue, std::min(mMaxValue, value)); }

   // Newfangled XML file I/O
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) const /* not override */;

   // Handling Cut/Copy/Paste events
   // sampleDur determines when the endpoint of the collapse is near enough
   // to an endpoint of the domain, that an extra control point is not needed.
   void CollapseRegion(double t0, double t1, double sampleDur);

   // Envelope has no notion of rate and control point times are not quantized;
   // but a tolerance is needed in the Paste routine, and better to inform it
   // of an appropriate number, than use hidden arbitrary constants.
   // The function is called 'PasteEnvelope' rather than 'Paste' to make it
   // easier to find where it is used in source code.
   void PasteEnvelope(double t0, const Envelope *e, double sampleDur);

   void InsertSpace(double t0, double tlen);

   // Control
   void SetOffset(double newOffset);
   void SetTrackLen( double trackLen, double sampleDur = 0.0 );
   void RescaleValues(double minValue, double maxValue);
   void RescaleTimes( double newLength );

   // Accessors
   /** \brief Get envelope value at time t */
   double GetValue( double t, double sampleDur = 0 ) const;

   /** \brief Get many envelope points at once.
    *
    * This is much faster than calling GetValue() multiple times if you need
    * more than one value in a row. */
   void GetValues(double *buffer, int len, double t0, double tstep) const;

   // Guarantee an envelope point at the end of the domain.
   void Cap( double sampleDur );

private:
   std::pair< int, int > ExpandRegion
      ( double t0, double tlen, double *pLeftVal, double *pRightVal );

   void RemoveUnneededPoints
      ( size_t startAt, bool rightward, bool testNeighbors = true );

   double GetValueRelative(double t, bool leftLimit = false) const;
   void GetValuesRelative
      (double *buffer, int len, double t0, double tstep, bool leftLimit = false)
      const;
   // relative time
   int NumberOfPointsAfter(double t) const;
   // relative time
   double NextPointAfter(double t) const;

public:
   double Average( double t0, double t1 ) const;
   double AverageOfInverse( double t0, double t1 ) const;
   double Integral( double t0, double t1 ) const;
   double IntegralOfInverse( double t0, double t1 ) const;
   double SolveIntegralOfInverse( double t0, double area) const;

   void print() const;
   void testMe();

   bool IsDirty() const;

   void Clear() { mEnv.clear(); }

   /** \brief Add a point at a particular absolute time coordinate */
   int InsertOrReplace(double when, double value)
   { return InsertOrReplaceRelative( when - mOffset, value ); }

   /** \brief Move a point at when to value
    *
    * Returns 0 if point moved, -1 if not found.*/
   int Reassign(double when, double value);

   /** \brief DELETE a point by its position in array */
   void Delete(int point);

   /** \brief insert a point */
   void Insert(int point, const EnvPoint &p);

   // Insert a point (without replacement)
   // for now assumed sequential.
   void Insert(double when, double value);

   /** \brief Return number of points */
   size_t GetNumberOfPoints() const;

   /** \brief Accessor for points */
   const EnvPoint &operator[] (int index) const
   {
      return mEnv[index];
   }

private:
   int InsertOrReplaceRelative(double when, double value);

   std::pair<int, int> EqualRange( double when, double sampleDur ) const;

public:
   /** \brief Returns the sets of when and value pairs */
   void GetPoints(double *bufferWhen,
      double *bufferValue,
      int bufferLen) const;

   // UI-related
   // The drag point needs to display differently.
   int GetDragPoint() const { return mDragPoint; }
   // Choose the drag point.
   void SetDragPoint(int dragPoint);
   // Mark or unmark the drag point for deletion.
   void SetDragPointValid(bool valid);
   bool GetDragPointValid() const { return mDragPointValid; }
   // Modify the dragged point and change its value.
   // But consistency constraints may move it less then you ask for.
   void MoveDragPoint(double newWhen, double value);
   // May delete the drag point.  Restores envelope consistency.
   void ClearDragPoint();

private:
   void AddPointAtEnd( double t, double val );
   void CopyRange(const Envelope &orig, size_t begin, size_t end);
   // relative time
   void BinarySearchForTime( int &Lo, int &Hi, double t ) const;
   void BinarySearchForTime_LeftLimit( int &Lo, int &Hi, double t ) const;
   double GetInterpolationStartValueAtPoint( int iPoint ) const;

   // The list of envelope control points.
   EnvArray mEnv;

   /** \brief The time at which the envelope starts, i.e. the start offset */
   double mOffset { 0.0 };
   /** \brief The length of the envelope, which is the same as the length of the
    * underlying track (normally) */
   double mTrackLen { 0.0 };

   // TODO: mTrackEpsilon based on assumption of 200KHz.  Needs review if/when
   // we support higher sample rates.
   /** \brief The shortest distance apart that points on an envelope can be
    * before being considered the same point */
   double mTrackEpsilon { 1.0 / 200000.0 };
   bool mDB;
   double mMinValue, mMaxValue;
   double mDefaultValue;

   // UI stuff
   bool mDragPointValid { false };
   int mDragPoint { -1 };

   mutable int mSearchGuess { -2 };
};

inline void EnvPoint::SetVal( Envelope *pEnvelope, double val )
{
   if ( pEnvelope )
      val = pEnvelope->ClampValue(val);
   mVal = val;
}

/*
PRL: This class gives access to all the important numerical data in a TimeTrack,
without need for the entire TimeTrack.

Confusingly, Envelope already carried its own limiting values, but those
in the TimeTrack were not guaranteed to be the same.

I'm just preserving behavior as I break file dependencies and won't try to fix
that confusion now.
*/
class BoundedEnvelope final : public Envelope
{
public:
   using Envelope::Envelope;

   double GetRangeLower() const { return mRangeLower; }
   double GetRangeUpper() const { return mRangeUpper; }

   void SetRangeLower(double lower) { mRangeLower = lower; }
   void SetRangeUpper(double upper) { mRangeUpper = upper; }

private:
   double mRangeLower{}, mRangeUpper{};
};

#endif
