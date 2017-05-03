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

#include <wx/dynarray.h>
#include <wx/brush.h>
#include <wx/pen.h>

#include "xml/XMLTagHandler.h"
#include "Internat.h"

class wxRect;
class wxDC;
class wxMouseEvent;
class wxTextFile;

class DirManager;
class Envelope;
class EnvPoint;

class ZoomInfo;

class EnvPoint final : public XMLTagHandler {

public:
   inline EnvPoint(Envelope *envelope, double t, double val);

   double GetT() const { return mT; }
   void SetT(double t) { mT = t; }
   double GetVal() const { return mVal; }
   inline void SetVal(double val);

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override
   {
      if (!wxStrcmp(tag, wxT("controlpoint"))) {
         while (*attrs) {
            const wxChar *attr = *attrs++;
            const wxChar *value = *attrs++;
            if (!wxStrcmp(attr, wxT("t")))
               SetT(Internat::CompatibleToDouble(value));
            else if (!wxStrcmp(attr, wxT("val")))
               SetVal(Internat::CompatibleToDouble(value));
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
   Envelope *mEnvelope;
   double mT;
   double mVal;

};

typedef std::vector<EnvPoint> EnvArray;

class Envelope final : public XMLTagHandler {
public:
   // Envelope can define a piecewise linear function, or piecewise exponential.
   Envelope(bool exponential, double minValue, double maxValue, double defaultValue);

   Envelope(const Envelope &orig);

   // Create from a subrange of another envelope.
   Envelope(const Envelope &orig, double t0, double t1);

   void Initialize(int numPoints);

   virtual ~ Envelope();

   double GetOffset() const { return mOffset; }
   double GetTrackLen() const { return mTrackLen; }

   bool GetInterpolateDB() { return mDB; }
   void SetInterpolateDB(bool db) { mDB = db; }
   void Rescale(double minValue, double maxValue);

   void Flatten(double value);

   double GetMinValue() const { return mMinValue; }
   double GetMaxValue() const { return mMaxValue; }
   void SetRange(double minValue, double maxValue);

   double ClampValue(double value) { return std::max(mMinValue, std::min(mMaxValue, value)); }

#if LEGACY_PROJECT_FILE_SUPPORT
   // File I/O

   bool Load(wxTextFile * in, DirManager * dirManager) override;
   bool Save(wxTextFile * out, bool overwrite) override;
#endif
   // Newfangled XML file I/O
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) const /* not override */;

   void DrawPoints(wxDC & dc, const wxRect & r, const ZoomInfo &zoomInfo,
      bool dB, double dBRange,
      float zoomMin, float zoomMax, bool mirrored) const;

   // Handling Cut/Copy/Paste events
   void CollapseRegion(double t0, double t1);
   void Paste(double t0, const Envelope *e);
   void InsertSpace(double t0, double tlen);
   void RemoveUnneededPoints(double time = -1, double tolerence = 0.001);

   // Control
   void SetOffset(double newOffset);
   void SetTrackLen(double trackLen);

   // Accessors
   /** \brief Get envelope value at time t */
   double GetValue(double t) const;

   /** \brief Get many envelope points at once.
    *
    * This is much faster than calling GetValue() multiple times if you need
    * more than one value in a row. */
   void GetValues(double *buffer, int len, double t0, double tstep) const;

   /** \brief Get many envelope points at once, but don't assume uniform time step.
   */
   void GetValues
      (double *buffer, int bufferLen, int leftOffset, const ZoomInfo &zoomInfo) const;

   int NumberOfPointsAfter(double t) const;
   double NextPointAfter(double t) const;

   double Average( double t0, double t1 ) const;
   double AverageOfInverse( double t0, double t1 ) const;
   double Integral( double t0, double t1 ) const;
   double IntegralOfInverse( double t0, double t1 ) const;
   double SolveIntegralOfInverse( double t0, double area) const;

   void print() const;
   void testMe();

   bool IsDirty() const;

   /** \brief Add a point at a particular spot */
   int Insert(double when, double value);

   /** \brief Move a point at when to value
    *
    * Returns 0 if point moved, -1 if not found.*/
   int Move(double when, double value);

   /** \brief DELETE a point by its position in array */
   void Delete(int point);

   /** \brief insert a point */
   void Insert(int point, const EnvPoint &p);

   /** \brief Return number of points */
   size_t GetNumberOfPoints() const;

private:
   friend class EnvelopeEditor;
   /** \brief Accessor for points */
   const EnvPoint &operator[] (int index) const
   {
      return mEnv[index];
   }

   std::pair<int, int> EqualRange( double when, double sampleTime ) const;

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
   EnvPoint *  AddPointAtEnd( double t, double val );
   void CopyRange(const Envelope &orig, size_t begin, size_t end);
   void BinarySearchForTime( int &Lo, int &Hi, double t ) const;
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
   /** \brief The shortest distance appart that points on an envelope can be
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

inline EnvPoint::EnvPoint(Envelope *envelope, double t, double val)
{
   mEnvelope = envelope;
   mT = t;
   mVal = mEnvelope->ClampValue(val);
}

inline void EnvPoint::SetVal(double val)
{
   mVal = mEnvelope->ClampValue(val);
}

// A class that holds state for the duration of dragging
// of an envelope point.
class EnvelopeEditor
{
public:
   EnvelopeEditor(Envelope &envelope, bool mirrored);
   ~EnvelopeEditor();

   // Event Handlers
   // Each of these returns true if the envelope needs to be redrawn
   bool MouseEvent(const wxMouseEvent & event, wxRect & r,
      const ZoomInfo &zoomInfo, bool dB, double dBRange,
      float zoomMin = -1.0, float zoomMax = 1.0);

private:
   bool HandleMouseButtonDown(const wxMouseEvent & event, wxRect & r,
      const ZoomInfo &zoomInfo, bool dB, double dBRange,
      float zoomMin = -1.0, float zoomMax = 1.0);
   bool HandleDragging(const wxMouseEvent & event, wxRect & r,
      const ZoomInfo &zoomInfo, bool dB, double dBRange,
      float zoomMin = -1.0, float zoomMax = 1.0, float eMin = 0., float eMax = 2.);
   bool HandleMouseButtonUp();

private:
   float ValueOfPixel(int y, int height, bool upper,
      bool dB, double dBRange,
      float zoomMin, float zoomMax);
   void MoveDragPoint(const wxMouseEvent & event, wxRect & r,
      const ZoomInfo &zoomInfo, bool dB, double dBRange,
      float zoomMin, float zoomMax);

   Envelope &mEnvelope;
   const bool mMirrored;

   /** \brief Number of pixels contour is from the true envelope. */
   int mContourOffset;

   // double mInitialVal;

   // int mInitialY;
   bool mUpper;
   int mButton;
   bool mDirty;
};

#endif
