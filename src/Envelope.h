/**********************************************************************

  Audacity: A Digital Audio Editor

  Envelope.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_ENVELOPE__
#define __AUDACITY_ENVELOPE__

#include <stdlib.h>
#include <algorithm>

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

class ZoomInfo;

class EnvPoint : public XMLTagHandler {

public:
   EnvPoint(Envelope *envelope, double t, double val)
   {
      mEnvelope = envelope;
      mT = t;
      mVal = ClampValue(val);
   }

   double ClampValue(double val); // this calls mEnvelope->ClampValue(), implementation is below the Envelope class

   double GetT() { return mT; }
   void SetT(double t) { mT = t; }
   double GetVal() { return mVal; }
   void SetVal(double val) { mVal = ClampValue(val); }

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs)
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

   XMLTagHandler *HandleXMLChild(const wxChar * WXUNUSED(tag))
   {
      return NULL;
   }

private:
   Envelope *mEnvelope;
   double mT;
   double mVal;

};

// TODO: Become an array of EnvPoint rather than of pointers to.
//    Really? wxWidgets help says:
//    "wxArray is suitable for storing integer types and pointers which it does not
//       treat as objects in any way..."
//    And why is this a TODO in any case, if it works correctly?
WX_DEFINE_ARRAY(EnvPoint *, EnvArray);

class Envelope : public XMLTagHandler {
 public:
   Envelope();
   void Initialize(int numPoints);

   virtual ~ Envelope();

   bool GetInterpolateDB() { return mDB; }
   void SetInterpolateDB(bool db) { mDB = db; }
   void Mirror(bool mirror);
   void Rescale(double minValue, double maxValue);

   void Flatten(double value);
   int GetDragPoint(void)   {return mDragPoint;}

   double GetMinValue() const { return mMinValue; }
   double GetMaxValue() const { return mMaxValue; }
   void SetRange(double minValue, double maxValue);

   double ClampValue(double value) { return std::max(mMinValue, std::min(mMaxValue, value)); }

#if LEGACY_PROJECT_FILE_SUPPORT
   // File I/O

   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);
#endif
   // Newfangled XML file I/O
   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   virtual void WriteXML(XMLWriter &xmlFile);

   void DrawPoints(wxDC & dc, const wxRect & r, const ZoomInfo &zoomInfo,
             bool dB, double dBRange,
             float zoomMin, float zoomMax);

   // Event Handlers
   // Each ofthese returns true if parents needs to be redrawn
   bool MouseEvent(wxMouseEvent & event, wxRect & r,
                   const ZoomInfo &zoomInfo, bool dB, double dBRange,
                   float zoomMin, float zoomMax);
   bool HandleMouseButtonDown( wxMouseEvent & event, wxRect & r,
                               const ZoomInfo &zoomInfo, bool dB, double dBRange,
                               float zoomMin, float zoomMax);
   bool HandleDragging( wxMouseEvent & event, wxRect & r,
                        const ZoomInfo &zoomInfo, bool dB, double dBRange,
                        float zoomMin, float zoomMax);
   bool HandleMouseButtonUp();

   // Handling Cut/Copy/Paste events
   void CollapseRegion(double t0, double t1);
   void CopyFrom(const Envelope * e, double t0, double t1);
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

   int NumberOfPointsAfter(double t);
   double NextPointAfter(double t);

   double Average( double t0, double t1 );
   double AverageOfInverse( double t0, double t1 );
   double Integral( double t0, double t1 );
   double IntegralOfInverse( double t0, double t1 );
   double SolveIntegralOfInverse( double t0, double area);

   void print();
   void testMe();

   bool IsDirty() const;

   /** \brief Add a point at a particular spot */
   int Insert(double when, double value);

   /** \brief Move a point at when to value
    *
    * Returns 0 if point moved, -1 if not found.*/
   int Move(double when, double value);

   /** \brief delete a point by it's position in array */
   void Delete(int point);

   /** \brief Return number of points */
   int GetNumberOfPoints() const;

   /** \brief Returns the sets of when and value pairs */
   void GetPoints(double *bufferWhen,
                  double *bufferValue,
                  int bufferLen) const;

private:
   EnvPoint *  AddPointAtEnd( double t, double val );
   void MarkDragPointForDeletion();
   float ValueOfPixel( int y, int height, bool upper,
                       bool dB, double dBRange,
                       float zoomMin, float zoomMax);
   void BinarySearchForTime( int &Lo, int &Hi, double t ) const;
   double GetInterpolationStartValueAtPoint( int iPoint ) const;
   void MoveDraggedPoint( wxMouseEvent & event, wxRect & r,
                               const ZoomInfo &zoomInfo, bool dB, double dBRange,
                               float zoomMin, float zoomMax);

   // Possibly inline functions:
   // This function resets them integral memoizers (call whenever the Envelope changes)
   void resetIntegralMemoizer() { lastIntegral_t0=0; lastIntegral_t1=0; lastIntegral_result=0; }

   // The list of envelope control points.
   EnvArray mEnv;
   bool mMirror;

   /** \brief The time at which the envelope starts, i.e. the start offset */
   double mOffset;
   /** \brief The length of the envelope, which is the same as the length of the
    * underlying track (normally) */
   double mTrackLen;

   // TODO: mTrackEpsilon based on assumption of 200KHz.  Needs review if/when
   // we support higher sample rates.
   /** \brief The shortest distance appart that points on an envelope can be
    * before being considered the same point */
   double mTrackEpsilon;
   double mDefaultValue;

   /** \brief Number of pixels contour is from the true envelope. */
   int mContourOffset;
   double mInitialVal;

   // These are used in dragging.
   int mDragPoint;
   int mInitialY;
   bool mUpper;
   bool mIsDeleting;
   int mButton;
   bool mDB;
   bool mDirty;

   double mMinValue, mMaxValue;

   // These are memoizing variables for Integral()
   double lastIntegral_t0;
   double lastIntegral_t1;
   double lastIntegral_result;

   mutable int mSearchGuess;

};

inline double EnvPoint::ClampValue(double val)
{
   return mEnvelope->ClampValue(val);
}

#endif

