/**********************************************************************

  Audacity: A Digital Audio Editor

  Envelope.cpp

  Dominic Mazzoni (original author)
  Dr William Bland (integration - the Calculus kind)
  Monty (xiphmont) (important bug fixes)

*******************************************************************//**

\class Envelope
\brief Draggable curve used in TrackPanel for varying amplification.

  This class manages an envelope - i.e. a piecewise linear funtion
  that the user can edit by dragging control points around.  The
  envelope is most commonly used to control the amplitude of a
  waveform, but it is also used to shape the Equalization curve.

*//****************************************************************//**

\class EnvPoint
\brief EnvPoint, derived from XMLTagHandler, provides Envelope with 
a draggable point type.

*//*******************************************************************/

#include "Envelope.h"

#include <math.h>

#include <wx/dc.h>
#include <wx/brush.h>
#include <wx/event.h>
#include <wx/pen.h>
#include <wx/textfile.h>
#include <wx/log.h>

#include "AColor.h"
#include "Prefs.h"
#include "DirManager.h"
#include "TrackArtist.h"

Envelope::Envelope()
{
   mOffset = 0.0;
   mTrackLen = 0.0;

   // Anything with a sample rate of no more than 200 KHz
   // will have samples spaced apart by at least this amount,
   // "epsilon".  We use this to enforce that there aren't
   // allowed to be multiple control points at the same t
   // value.
   mTrackEpsilon = 1.0 / 200000.0;

   mDB = true;
   mDefaultValue = 1.0;
   mDragPoint = -1;
   mDirty = false;
   mIsDeleting = false;
   mMirror = true;

   mButton = wxMOUSE_BTN_NONE;
}

Envelope::~Envelope()
{
   mDragPoint = -1;// TODO: Remove - Isn't this line totally pointless since we're in the destructor?
   WX_CLEAR_ARRAY(mEnv);
}

// TODO: Move Getters/Setters to Envelope.h and
// name them consistently Get/Set

void Envelope::SetInterpolateDB(bool db)
{
   mDB = db;
}

void Envelope::Mirror(bool mirror)
{
   mMirror = mirror;
}

/// Flatten removes all points from the envelope to 
/// make it horizontal at a chosen y-value.
/// @value - the y-value for the flat envelope.
void Envelope::Flatten(double value)
{
   WX_CLEAR_ARRAY(mEnv);
   SetDefaultValue(value);
}

EnvPoint * Envelope::AddPointAtEnd( double t, double val )
{
   // JC: Gross! (allocating one point at a time.)
   // TODO: switch over to using an array of EnvPoints
   // rather than an array of pointers to EnvPoints.
   //    What value does that add?
   EnvPoint *pt = new EnvPoint();
   pt->t = t;
   pt->val = val;
   mEnv.Add(pt);
   return pt;
}

void Envelope::CopyFrom(const Envelope *e, double t0, double t1)
{
   wxASSERT( t0 < t1 );

   mOffset   = wxMax(t0, e->mOffset);
   mTrackLen = wxMin(t1, e->mOffset + e->mTrackLen) - mOffset;

   WX_CLEAR_ARRAY(mEnv);
   int len = e->mEnv.Count();
   int i = 0;

   // Skip the points that come before the copied region
   while( (i < len) && e->mOffset + e->mEnv[i]->t <= t0)
      i++;

   // Create the point at 0 if it needs interpolated representation
   if (i>0) 
      AddPointAtEnd( 0, e->GetValue(mOffset) );

   // Copy points from inside the copied region
   while ( (i < len) && e->mOffset + e->mEnv[i]->t - mOffset < mTrackLen) {
      AddPointAtEnd( e->mOffset + e->mEnv[i]->t - mOffset, e->mEnv[i]->val );
      i++;
   }

   // Create the final point if it needs interpolated representation
   if (mTrackLen > 0 && i < len)
      AddPointAtEnd( mTrackLen, e->GetValue(mOffset + mTrackLen));
}

/// Limit() limits a double value to a range.
/// TODO: Move to a general utilities source file.
double Limit( double Lo, double Value, double Hi )
{
   if( Value < Lo )
      return Lo;
   if( Value > Hi )
      return Hi;
   return Value;
}

double Envelope::toDB(double value)
{
   if (value == 0)
      return 0;

   // TODO: Cache the gPrefs value.  Reading it every time is inefficient.
   double dBRange = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);
   double sign = (value >= 0 ? 1 : -1);

   wxASSERT( dBRange > 0 );
   double db = 20 * log10(fabs(value));
   double val = (db + dBRange) / dBRange;

   val = Limit( 0.0, val, 1.0 );
   return sign * val;
}

double Envelope::fromDB(double value) const
{
   if (value == 0)
      return 0;

   double sign = (value >= 0 ? 1 : -1);
   // TODO: Cache the gPrefs value.  Reading it every time is inefficient.
   double dBRange = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);
   return pow(10.0, ((fabs(value) * dBRange) - dBRange) / 20.0)*sign;;
}

/// TODO: This should probably move to track artist.
void DrawPoint(wxDC & dc, const wxRect & r, int x, int y, bool top)
{
   if (y >= 0 && y <= r.height) {
      wxRect circle(r.x + x, r.y + (top ? y - 1: y - 2), 4, 4);
      dc.DrawEllipse(circle);
   }
}

/// TODO: This should probably move to track artist.
void Envelope::DrawPoints(wxDC & dc, const wxRect & r, double h, double pps, bool dB,
                    float zoomMin, float zoomMax)
{
   h -= mOffset;

   wxASSERT( pps > 0 );
   double tright = h + (r.width / pps);
   // TODO: Cache the gPrefs value.  Reading it every time is inefficient.
   double dBRange = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);

   dc.SetPen(AColor::envelopePen);
   dc.SetBrush(*wxWHITE_BRUSH);

   for (int i = 0; i < (int)mEnv.Count(); i++) {
      if (mEnv[i]->t >= h && mEnv[i]->t <= tright) {
         // Change colour if this is the draggable point...
         if (i == mDragPoint) {
            dc.SetPen(AColor::envelopePen);
            dc.SetBrush(AColor::envelopeBrush);
         }

         double v = mEnv[i]->val;
         int x = int ((mEnv[i]->t - h) * pps);
         int y, y2;

         y = GetWaveYPos(v, zoomMin, zoomMax, r.height, dB,
            true, dBRange, false);
         if (!mMirror) {
            DrawPoint(dc, r, x, y, true);
         }
         else {
            y2 = GetWaveYPos(-v-.000000001, zoomMin, zoomMax, r.height, dB,
               true, dBRange, false);

            // This follows the same logic as the envelop drawing in
            // TrackArtist::DrawEnvelope().
            // TODO: make this calculation into a reusable function.
            if (y2 - y < 9) {
               int value = (int)((zoomMax / (zoomMax - zoomMin)) * r.height);
               y = value - 4;
               y2 = value + 4;
            }

            DrawPoint(dc, r, x, y, true);
            DrawPoint(dc, r, x, y2, false);

            // Contour
            y = GetWaveYPos(v, zoomMin, zoomMax, r.height, dB,
               false, dBRange, false);
            y2 = GetWaveYPos(-v-.000000001, zoomMin, zoomMax, r.height, dB,
               false, dBRange, false);
            if (y <= y2) {
               DrawPoint(dc, r, x, y, true);
               DrawPoint(dc, r, x, y2, false);
            }
         }

         // Change colour back again if was the draggable point.
         if (i == mDragPoint) {
            dc.SetPen(AColor::envelopePen);
            dc.SetBrush(*wxWHITE_BRUSH);
         }
      }
   }
}

bool Envelope::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   // Return unless it's the envelope tag.
   if (wxStrcmp(tag, wxT("envelope")))
      return false;

   int numPoints = 0;
   long nValue = -1;

   while (*attrs) {
      const wxChar *attr = *attrs++;
      const wxChar *value = *attrs++;
      if (!value)
         break;
      const wxString strValue = value;
      if( !wxStrcmp(attr, wxT("numpoints")) && 
            XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue)) 
         numPoints = nValue;
   }
   if (numPoints < 0)
      return false;

   WX_CLEAR_ARRAY(mEnv);
   mEnv.Alloc(numPoints);
   return true;
}

XMLTagHandler *Envelope::HandleXMLChild(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("controlpoint"))) 
      return NULL;

   return AddPointAtEnd(0,0);
}

void Envelope::WriteXML(XMLWriter &xmlFile)
{
   unsigned int ctrlPt;

   xmlFile.StartTag(wxT("envelope"));
   xmlFile.WriteAttr(wxT("numpoints"), mEnv.GetCount());

   for (ctrlPt = 0; ctrlPt < mEnv.GetCount(); ctrlPt++) {
      xmlFile.StartTag(wxT("controlpoint"));
      xmlFile.WriteAttr(wxT("t"), mEnv[ctrlPt]->t, 12);
      xmlFile.WriteAttr(wxT("val"), mEnv[ctrlPt]->val, 12);
      xmlFile.EndTag(wxT("controlpoint"));
   }

   xmlFile.EndTag(wxT("envelope"));
}

#ifndef SQR
#define SQR(X) ((X)*(X))
#endif

/// ValueOfPixel() converts a y position on screen to an envelope value.
/// @param y - y position, usually of the mouse.relative to the clip.
/// @param height - height of the rectangle we are in.
/// @upper - true if we are on the upper line, false if on lower.
/// @dB - display mode either linear or log.
/// @zoomMin - vertical scale, typically -1.0
/// @zoomMax - vertical scale, typically +1.0
/// @eMin - clips result to this range.
/// @eMax - clips result to this range.
float Envelope::ValueOfPixel( int y, int height, bool upper, bool dB,
                              float zoomMin, float zoomMax, float eMin, float eMax )
{
   float v;

   wxASSERT( height > 0 );
   v = zoomMax - (y/(float)height) * (zoomMax - zoomMin);

   if (mContourOffset) {
     if( v > 0.0 )
       v += .5;
     else
       v -= .5;
   }
   if (dB)
      v = fromDB(v);

   // JC: The eMin/eMax code originally assumed a symmetrical range.
   // so only eMin was needed.  I'm very dubious about the lower curve
   // behaviour and expect some funny behaviour when dragging
   // the lower curve when we have asymmetric ranges.

   // Upper curve: gets clamped between eMin and eMax.  Looks OK.
   // Lower curve: reset to eMin if > 0 otherwise made positive and <=eMax.
   if ((upper && v < eMin) || (!upper && v > 0))
      v = eMin;
   else if (!upper)
      v = -v;

   if(v > eMax)
      v = eMax;

   return v;
}

/// HandleMouseButtonDown either finds an existing control point or adds a new one
/// which is then recorded as the point to drag.
/// This is slightly complicated by there possibly being four control points for 
/// a given time value:
/// We have an upper and lower envelope line.
/// Also we may be showing an inner envelope (at 0.5 the range).
bool Envelope::HandleMouseButtonDown(wxMouseEvent & event, wxRect & r,
                                     double h, double pps, bool dB,
                                     float zoomMin, float zoomMax, float eMin, float eMax)
{
   int ctr = (int)(r.height * zoomMax / (zoomMax - zoomMin));
   bool upper = (zoomMin == eMin) || (event.m_y - r.y < ctr);

   int clip_y = event.m_y - r.y;
   if(clip_y < 0) clip_y = 0; //keeps point in rect r, even if mouse isn't
   if(clip_y > r.GetBottom()) clip_y = r.GetBottom();

   double tleft = h - mOffset;
   double tright = tleft + (r.width / pps);
   int bestNum = -1;
   int bestDist = 10; // Must be within 10 pixel radius.

   // TODO: Cache the gPrefs value.  Reading it every time is inefficient.
   double dBr = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);

   // Member variables hold state that will be needed in dragging.
   mButton        = event.GetButton();
   mIsDeleting    = false;
   mContourOffset = false;

   //   wxLogDebug(wxT("Y:%i Height:%i Offset:%i"), y, height, mContourOffset );
   int len = mEnv.Count();

   // TODO: extract this into a function FindNearestControlPoint()
   // TODO: also fix it so that we can drag the last point on an envelope.
   for (int i = 0; i < len; i++) { //search for control point nearest click
      if (mEnv[i]->t >= tleft && mEnv[i]->t <= tright) {
         
         int x = int ((mEnv[i]->t + mOffset - h) * pps) + r.x;
         int y[4];
         int numControlPoints;

         // Outer control points
         y[0] = GetWaveYPos( mEnv[i]->val, zoomMin, zoomMax, r.height,
                                dB, true, dBr, false);
         y[1] = GetWaveYPos( -mEnv[i]->val, zoomMin, zoomMax, r.height,
                                dB, true, dBr, false);

         // Inner control points(contour)
         y[2] = GetWaveYPos( mEnv[i]->val, zoomMin, zoomMax, r.height,
                                dB, false, dBr, false);
         y[3] = GetWaveYPos( -mEnv[i]->val-.00000001, zoomMin, zoomMax, 
                                r.height, dB, false, dBr, false);

         numControlPoints = 4;

         if (y[2] > y[3])
            numControlPoints = 2;

         if (!mMirror)
            numControlPoints = 1;
         
         for(int j=0; j<numControlPoints; j++){
            
            int d = (int)(sqrt((double)(SQR(x-event.m_x) + SQR(y[j]-(event.m_y-r.y)))) + 0.5);
            if (d < bestDist) {
               bestNum = i;
               bestDist = d;
               mContourOffset = (bool)(j > 1);
            }
         }
      }
   }

   if (bestNum >= 0) {
      mDragPoint = bestNum;
   }
   else {
      // TODO: Extract this into a function CreateNewPoint
      double when = h + (event.m_x - r.x) / pps - mOffset;
      
      //      if (when <= 0 || when >= mTrackLen)
      //         return false;
      
      double v = GetValueAtX( event.m_x, r, h, pps );
      
      int ct = GetWaveYPos( v, zoomMin, zoomMax, r.height, dB, 
                               false, dBr, false) ;
      int cb = GetWaveYPos( -v-.000000001, zoomMin, zoomMax, r.height, dB, 
                               false, dBr, false) ;
      if( ct <= cb || !mMirror ){
         int t = GetWaveYPos( v, zoomMin, zoomMax, r.height, dB, 
                                 true, dBr, false) ;
         int b = GetWaveYPos( -v, zoomMin, zoomMax, r.height, dB, 
                                 true, dBr, false) ;

         ct = (t + ct) / 2;
         cb = (b + cb) / 2;

         if(mMirror &&
            (event.m_y - r.y) > ct &&
            ((event.m_y - r.y) < cb))
            mContourOffset = true;
         else
            mContourOffset = false;
      }

      double newVal = ValueOfPixel(clip_y, r.height, upper, dB,
                                   zoomMin, zoomMax, eMin, eMax);

      mDragPoint = Insert(when, newVal);
      mDirty = true;
   }

   mUpper = upper;

   mInitialWhen = mEnv[mDragPoint]->t;
   mInitialVal = mEnv[mDragPoint]->val;

   mInitialX = event.m_x;
   mInitialY = event.m_y+mContourOffset;

   return true;
}

/// Mark dragged point for deletion.
/// It will be deleted on mouse button up.
void Envelope::MarkDragPointForDeletion()
{
   mIsDeleting = true;

   // We're going to be deleting the point; On
   // screen we show this by having the envelope move to
   // the position it will have after deletion of the point.
   // Without delting the point we move it left or right 
   // to the same position as the previous or next point.

   if( mEnv.Count()<=1) 
   {
      // There is only one point - just move it
      // off screen and at default height.
      // temporary state when dragging only! 
      mEnv[mDragPoint]->t = -1000000.0;
      mEnv[mDragPoint]->val = mDefaultValue;
      return;
   }

   // Place it exactly on one of its neighbours.
   int iNeighbourPoint = mDragPoint + ((mDragPoint > 0) ? -1:+1);
   mEnv[mDragPoint]->t   = mEnv[iNeighbourPoint]->t;
   mEnv[mDragPoint]->val = mEnv[iNeighbourPoint]->val;
}

void Envelope::MoveDraggedPoint( wxMouseEvent & event, wxRect & r,
                               double h, double pps, bool dB,
                               float zoomMin, float zoomMax, float eMin, float eMax)
{
   int clip_y = event.m_y - r.y;
   if(clip_y < 0) clip_y = 0;
   if(clip_y > r.height) clip_y = r.height;
   double newVal = ValueOfPixel(clip_y, r.height, mUpper, dB,
                                zoomMin, zoomMax, eMin, eMax);   

   wxASSERT( pps > 0 );
   // We no longer tolerate multiple envelope points at the same t.
   // epsilon is less than the time offset of a single sample
   // TODO: However because mTrackEpsilon assumes 200KHz this use
   // of epsilon is a tad bogus.  What we need to do instead is delete 
   // a duplicated point on a mouse up.
   double newWhen = mInitialWhen + (event.m_x - mInitialX) / pps;

   // We'll limit the drag point time to be between those of the preceding
   // and next envelope point.
   double limitLo = 0.0;
   double limitHi = mTrackLen;

   if (mDragPoint > 0)
      limitLo = mEnv[mDragPoint - 1]->t + mTrackEpsilon;
   if (mDragPoint < (int)mEnv.Count() - 1 )
      limitHi = mEnv[mDragPoint + 1]->t - mTrackEpsilon;

   newWhen = Limit( limitLo, newWhen, limitHi );
   newWhen = Limit( mTrackEpsilon, newWhen, mTrackLen - mTrackEpsilon);

   mEnv[mDragPoint]->t = newWhen;
   mEnv[mDragPoint]->val = newVal;

}

bool Envelope::HandleDragging( wxMouseEvent & event, wxRect & r,
                               double h, double pps, bool dB,
                               float zoomMin, float zoomMax, float eMin, float eMax)
{
   mDirty = true;

   wxRect larger = r;
   larger.Inflate(10, 10);

   if (larger.Contains(event.m_x, event.m_y))
   {
      // IF we're in the rect THEN we're not deleting this point (anymore).
      mIsDeleting = false;
      // ...we're dragging it.
      MoveDraggedPoint( event, r,h,pps,dB, zoomMin, zoomMax, eMin, eMax );
      return true;
   }
   
   if(mIsDeleting )
      // IF we already know we're deleting THEN no envelope point to update.
      return false;

   MarkDragPointForDeletion();
   return true;
}

// Exit dragging mode and deletes dragged point if neccessary.
bool Envelope::HandleMouseButtonUp( wxMouseEvent & event, wxRect & r,
                                    double h, double pps, bool dB,
                                    float zoomMin, float zoomMax )
{
   if (mIsDeleting) {
      delete mEnv[mDragPoint];
      mEnv.RemoveAt(mDragPoint);
   }
   mDragPoint = -1;
   mButton = wxMOUSE_BTN_NONE;
   return true;
}

void Envelope::Delete( int point )
{
   delete mEnv[point];
   mEnv.RemoveAt(point);
}

// Returns true if parent needs to be redrawn
bool Envelope::MouseEvent(wxMouseEvent & event, wxRect & r,
                          double h, double pps, bool dB,
                          float zoomMin, float zoomMax, float eMin, float eMax)
{
   if (event.ButtonDown() && mButton == wxMOUSE_BTN_NONE)
      return HandleMouseButtonDown( event, r, h, pps,dB,
                                    zoomMin, zoomMax, eMin, eMax);
   if (event.Dragging() && mDragPoint >= 0) 
      return HandleDragging( event, r, h, pps,dB,
                             zoomMin, zoomMax, eMin, eMax);
   if (event.ButtonUp() && event.GetButton() == mButton)
      return HandleMouseButtonUp( event, r, h, pps, dB,
                                  zoomMin, zoomMax);
   return false;
}

void Envelope::CollapseRegion(double t0, double t1)
{
   // This gets called when somebody clears samples.  All of the
   // control points within the region disappear and the points
   // to the right get shifted over.

   t0 -= mOffset;
   t1 -= mOffset;

   t0 = Limit( 0, t0, mTrackLen );
   t1 = Limit( 0, t1, mTrackLen );

   int len = mEnv.Count();
   int i;

   // Remove points in deleted region.
   for (i = 0; i < len - 0; i++)
      if (mEnv[i]->t >= t0 && mEnv[i]->t < t1) {
         delete mEnv[i];
         mEnv.RemoveAt(i);
         len--;
         i--;
      }

   // Shift points left after deleted region.
   for (i = 0; i < len; i++)
      if (mEnv[i]->t >= t1)
         mEnv[i]->t -= (t1 - t0);
   
   mTrackLen -= (t1-t0);
}

// This operation is trickier than it looks; the basic rub is that
// a track's envelope runs the range from t=0 to t=tracklen; the t=0
// envelope point applies to the first sample, but the t=tracklen
// envelope point applies one-past the last actual sample.
// Rather than going to a .5-offset-index, we special case the framing.
void Envelope::Paste(double t0, Envelope *e)
{
   bool pointsAdded = false;

// JC: The old analysis of cases and the resulting code here is way more complex than needed.
// TODO: simplify the analysis and simplify the code.

   if (e->mEnv.Count() == 0 && this->mEnv.Count() == 0 && e->mDefaultValue == this->mDefaultValue)
   {
      // msmeyer: The envelope is empty and has the same default value, so
      // there is nothing that must be inserted, just return. This avoids
      // the creation of unnecessary duplicate control points
      // MJS: but the envelope does get longer
      mTrackLen += e->mTrackLen;
      return;
   }
   if (this->mEnv.Count() != 0)
   {
      // inserting a clip with a possibly empty envelope into one with an envelope
      // so add end points to e, in case they are not there
      double leftval  = e->GetValue(0+e->mOffset);
      double rightval = e->GetValue(e->mTrackLen+e->mOffset);
      e->Insert(0, leftval);
      e->Insert(e->mTrackLen, rightval);
      pointsAdded = true;  // we need to delete them later so's not to corrupt 'e' for later use
   }
   
   t0 = wxMin(t0 - mOffset, mTrackLen);   // t0 now has origin of zero
   double deltat = e->mTrackLen;

   unsigned int i;
   unsigned int pos = 0;
   bool someToShift = false;
   bool atStart = false;
   bool beforeStart = false;
   bool atEnd = false;
   bool afterEnd = false;
   bool onPoint = false;
   unsigned int len = mEnv.Count();

   // get values to perform framing of the insertion 
   double splitval = GetValue(t0 + mOffset);

   if(len != 0) {   // Not case 10: there are point/s in the envelope


/*
Old analysis of cases:
(see discussions on audacity-devel around 19/8/7 - 23/8/7 and beyond, "Envelopes and 'Join'")
1  9     11  2    3 5  7   8   6 4   13              12
0-----0--0---0    -----0---0------       --(0)----

1   The insert point is at the beginning of the current env, and it is a control point.
2   The insert point is at the end of the current env, and it is a control point.
3   The insert point is at the beginning of the current env, and it is not a control point.
4   The insert point is at the end of the current env, and it is not a control point.
5   The insert point is not at a control point, and there is space either side.
6   As 5.
7   The insert point is at a control point, and there is space either side.
8   Same as 7.
9   Same as 5.
10  There are no points in the current envelope (commonly called by the 'undo' stuff, and not in the diagrams).
11  As 7.
12  Insert beyond the RH end of the current envelope (should not happen, at the moment)
13  Insert beyond the LH end of the current envelope (should not happen, at the moment)
*/

// JC: Simplified Analysis:
// In pasting in a clip we choose to preserve the envelope so that the loudness of the
// parts is unchanged.
//
// 1) This may introduce a discontnuity in the envelope at a boundary between the 
//    old and new clips.  In that case we must ensure there are envelope points 
//    at sample positions immediately before and immediately after the boundary.
// 2) If the points have the same value we only need one of them.
// 3) If the points have the same value AND it is the same as the value interpolated
//    from the rest of the envelope then we don't need it at all.
//
// We do the same for the left and right edge of the new clip.
//
// Even simpler: we could always add two points at a boundary and then call 
// RemoveUnneededPoints() (provided that function behaves correctly).  

      // See if existing points need shifting to the right, and what Case we are in
      for (i = 0; i < len; i++) {
         if (mEnv[i]->t > t0)
            someToShift = true;
         else {
            pos = i; // last point not moved
            if ( fabs(mEnv[i]->t - t0) - 1/500000.0 < 0.0 ) // close enough to a point
               onPoint = true;
         }
      }

      // In these statements, remember we subtracted mOffset from t0
      if( t0 < mTrackEpsilon )
         atStart = true;
      if( (mTrackLen - t0) < mTrackEpsilon )
         atEnd = true;
      if(0 > t0)
         beforeStart = true;  // Case 13
      if(mTrackLen < t0)
         afterEnd = true;  // Case 12

      // Now test for the various Cases, and try to do the right thing
      if(atStart) {   // insertion at the beginning
         if(onPoint) {  // first env point is at LH end
            mEnv[0]->t +=mTrackEpsilon;   // Case 1: move it R slightly to avoid duplicate point
            someToShift = true;  // there is now, even if there wasn't before
            //wxLogDebug(wxT("Case 1"));
         }
         else {
            Insert(t0 + mTrackEpsilon, splitval);   // Case 3: insert a point to maintain the envelope
            someToShift = true;
            //wxLogDebug(wxT("Case 3"));
         }
      }
      else {
         if(atEnd) { // insertion at the end
            if(onPoint) {  // last env point is at RH end, Case 2: 
               mEnv[0]->t -= mTrackEpsilon;  // move it L slightly to avoid duplicate point
               //wxLogDebug(wxT("Case 2"));
            }
            else {   // Case 4:
               Insert(t0 - mTrackEpsilon, splitval);   // insert a point to maintain the envelope
               //wxLogDebug(wxT("Case 4"));
            }
         }
         else {
            if(onPoint) {  // Case 7: move the point L and insert a new one to the R
               mEnv[pos]->t -= mTrackEpsilon;
               Insert(t0 + mTrackEpsilon, splitval);
               someToShift = true;
               //wxLogDebug(wxT("Case 7"));
            }
            else {
               if( !beforeStart && !afterEnd ) {// Case 5: Insert points to L and R
                  Insert(t0 - mTrackEpsilon, splitval);
                  Insert(t0 + mTrackEpsilon, splitval);
                  someToShift = true;
                  //wxLogDebug(wxT("Case 5"));
               }
               else {
                  if( beforeStart ) {  // Case 13:
                     //wxLogDebug(wxT("Case 13"));
                  }
                  else {   // Case 12:
                     //wxLogDebug(wxT("Case 12"));
                  }
               }
            }
         }
      }

      // Now shift existing points to the right, if required
      if(someToShift) {
         len = mEnv.Count();  // it may well have changed
         for (i = 0; i < len; i++)
            if (mEnv[i]->t > t0)
               mEnv[i]->t += deltat;
      }
      mTrackLen += deltat;
   }
   else {   // Case 10:
      if( mTrackLen == 0 ) // creating a new envelope
      {
         mTrackLen = e->mTrackLen;
         mOffset = e->mOffset;
         //wxLogDebug(wxT("Case 10, new env/clip: mTrackLen %f mOffset %f t0 %f"), mTrackLen, mOffset, t0);
      }
      else
      {
         mTrackLen += e->mTrackLen;
         //wxLogDebug(wxT("Case 10, paste into current env: mTrackLen %f mOffset %f t0 %f"), mTrackLen, mOffset, t0);
      }
   }

   // Copy points from inside the selection
   len = e->mEnv.Count();
   for (i = 0; i < len; i++)
      pos=Insert(t0 + e->mEnv[i]->t, e->mEnv[i]->val);

/*   if(len != 0)
      for (i = 0; i < mEnv.Count(); i++)
         wxLogDebug(wxT("Fixed i %d when %.18f val %f"),i,mEnv[i]->t,mEnv[i]->val); */

   if(pointsAdded)
      while(e->mEnv.Count() != 0)
         e->Delete(0);  // they were not there when we entered this
}

// Deletes 'unneeded' points, starting from the left.
// If 'time' is set and positive, just deletes points in a small region
// around that value.
// 'Unneeded' means that the envelope doesn't change by more than
// 'tolerence' without the point being there.
void Envelope::RemoveUnneededPoints(double time, double tolerence)
{
   unsigned int len = mEnv.Count();
   unsigned int i;
   double when, val, val1;

   if(mEnv.Count() == 0)
      return;

   for (i = 0; i < len; i++) {
      when = mEnv[i]->t;
      if(time >= 0)
      {
         if(fabs(when + mOffset - time) > 0.00025) // 2 samples at 8kHz, 11 at 44.1kHz
            continue;
      }
      val = mEnv[i]->val;
      Delete(i);  // try it to see if it's doing anything
      val1 = GetValue(when + mOffset);
      bool bExcludePoint = true;
      if( fabs(val -val1) > tolerence )
      {
         Insert(when,val); // put it back, we needed it
         
         //Insert may have modified instead of inserting, if two points were at the same time.
         // in which case len needs to shrink i and len, because the array size decreased.
         bExcludePoint = (mEnv.Count() < len);
      }

      if( bExcludePoint ) {   // it made no difference so leave it out
         len--;
         i--;
      }
   }
}

void Envelope::InsertSpace(double t0, double tlen)
{
   unsigned int len = mEnv.Count();
   unsigned int i;

   for (i = 0; i < len; i++)
      if (mEnv[i]->t > t0)
         mEnv[i]->t += tlen;
   mTrackLen += tlen;
}

int Envelope::Move(double when, double value)
{
   int len = mEnv.Count();
   if (len == 0)
      return -1;

   int i = 0;
   while (i < len && when > mEnv[i]->t)
      i++;

   if (i >= len || when < mEnv[i]->t)
      return -1;

   mEnv[i]->val = value;
   return 0;
}


int Envelope::GetNumberOfPoints() const
{
   return mEnv.Count();
}

void Envelope::GetPoints(double *bufferWhen,
                         double *bufferValue,
                         int bufferLen) const
{
   int n = mEnv.Count();
   if (n > bufferLen)
      n = bufferLen;
   int i;
   for (i = 0; i < n; i++) {
      bufferWhen[i] = mEnv[i]->t;
      bufferValue[i] = mEnv[i]->val;
   }
}

// Private methods

// We no longer tolerate multiple envelope control points at the exact
// same t; the behavior can be well-defined, but it is still incorrect
// in that it vastly complicates paste operations behaving as a user
// reasonably expects.  The most common problem occurs pasting an
// envelope into another track; the boundary behavior causes the
// t=insert_point envelope level of the insertee to apply to sample 0
// of the inserted sample, causing a pop.  This most visibly manifests
// itself in undo and mixing when a v=1.0 sample magically shows
// up at boundaries causing a pop.

// Although this renders the name a slight misnomer, a duplicate
// 'replaces' the current control point.

/** @brief Add a control point to the envelope
 *
 * Control point positions start at zero and are measured in seconds from the
 * start of the envelope. The position of the envelope on the project-wide
 * time scale is store in seconds in Envelope::mOffset.
 * This is worth remembering.
 * If you call Envelope::Insert() from WaveClip, or anywhere else outside the
 * Envelope class that is using project timing, subtract the envelope's mOffset
 * from the time.
 * If you call Envelope::Insert() from within Envelope, don't subtract mOffset 
 * because you are working in relative time inside the envelope
 * @param when the time in seconds when the envelope point should be created.
 * @param value the envelope value to use at the given point.
 * @return the index of the new envelope point within array of envelope points.
 */
int Envelope::Insert(double when, double value)
{
#if defined(__WXDEBUG__)
   // in debug builds, do a spot of argument checking
   if(when > mTrackLen + 0.0000001)
   {
      wxString msg;
      msg = wxString::Format(wxT("when %.20f mTrackLen %.20f diff %.20f"), when, mTrackLen, when-mTrackLen);
      wxASSERT_MSG(when <= (mTrackLen), msg);
   }
   if(when < 0)
   {
      wxString msg;
      msg = wxString::Format(wxT("when %.20f mTrackLen %.20f"), when, mTrackLen);
      wxASSERT_MSG(when >= 0, msg);
   }
#endif

   int len = mEnv.Count();

   if (len && when < 0.0)
      return 0;
   if ((len > 1) && when > mTrackLen)
      return len - 1;

   if (when < 0.0)
      when = 0.0;
   if ((len>1) && when > mTrackLen)
      when = mTrackLen;

   int i = 0;
   
   while (i < len && when > mEnv[i]->t)
      i++;

   if(i < len && when == mEnv[i]->t) {

     // modify existing
     mEnv[i]->val = value;

   }
   else{

     // Add new
     EnvPoint *e = new EnvPoint();
     e->t = when;
     e->val = value;
     if (i < len) {
        mEnv.Insert(e, i);
     } else {
        mEnv.Add(e);
     }
   }
   return i;
}

// Control

void Envelope::SetOffset(double newOffset)
{
   mOffset = newOffset;
}

void Envelope::SetTrackLen(double trackLen)
{
   mTrackLen = trackLen;

   int len = mEnv.Count();
   for (int i = 0; i < len; i++)
      if (mEnv[i]->t > mTrackLen) {
         delete mEnv[i];
         mEnv.RemoveAt(i);
         len--;
         i--;
      }
}

// Accessors
double Envelope::GetValue(double t) const
{
   double temp;

   GetValues(&temp, 1, t, 1.0);
   return temp;
}

// 'X' is in pixels and relative to track.
double Envelope::GetValueAtX(int x, const wxRect & r, double h, double pps)
{
   // Convert x to time.
   double t = (x - r.x) / pps + h ;//-mOffset;
   return GetValue(t);
}

/// @param Lo returns index before this time.
/// @param Hi returns index after this time.
void Envelope::BinarySearchForTime( int &Lo, int &Hi, double t ) const
{
   Lo = 0;
   Hi = mEnv.Count() - 1;
   // JC: Do we have a problem if the envelope only has one point??
   wxASSERT( Hi > Lo );
   while (Hi > (Lo + 1)) {
      int mid = (Lo + Hi) / 2;
      if (t < mEnv[mid]->t)
         Hi = mid;
      else
         Lo = mid;
   }
   wxASSERT( Hi == ( Lo+1 ));
}

/// GetInterpolationStartValueAtPoint() is used to select either the
/// envelope value or its log depending on whether we are doing linear
/// or log interpolation.  
/// @param iPoint index in env array to look at.
/// @return value there, or its (safe) log10.
double Envelope::GetInterpolationStartValueAtPoint( int iPoint ) const
{
   double v = mEnv[ iPoint ]->val;
   if( !mDB )
      return v;
   // Special case for the log of zero
   if (v <= 0.0)
      return -7.0; // This corresponds to -140 dB
   return log10( v );
}

void Envelope::GetValues(double *buffer, int bufferLen,
                         double t0, double tstep) const
{
   t0 -= mOffset;

   // JC: If bufferLen ==0 we have probably just allocated a zero sized buffer.
   wxASSERT( bufferLen > 0 );

   int len = mEnv.Count();

   double t = t0;
   double tprev, vprev, tnext = 0, vnext, vstep = 0;

   for (int b = 0; b < bufferLen; b++) {

      // Get easiest cases out the way first...
      // IF empty envelope THEN default value
      if (len <= 0) {
         buffer[b] = mDefaultValue;
         t += tstep;
         continue;
      }
      // IF before envelope THEN first value
      if (t <= mEnv[0]->t) {
         buffer[b] = mEnv[0]->val;
         t += tstep;
         continue;
      }
      // IF after envelope THEN last value
      if (t >= mEnv[len - 1]->t) {
         buffer[b] = mEnv[len - 1]->val;
         t += tstep;
         continue;
      }

      if (b == 0 || t > tnext) {

         // We're beyond our tnext, so find the next one.
         // Don't just increment lo or hi because we might
         // be zoomed far out and that could be a large number of
         // points to move over.  That's why we binary search.

         int lo,hi;
         BinarySearchForTime( lo, hi, t );
         tprev = mEnv[lo]->t;
         tnext = mEnv[hi]->t;

         vprev = GetInterpolationStartValueAtPoint( lo );
         vnext = GetInterpolationStartValueAtPoint( hi );
         
         // Interpolate, either linear or log depending on mDB.
         double dt = (tnext - tprev);
         double to = t - tprev;
         double v;
         if (dt > 0.0)
         {
            v = (vprev * (dt - to) + vnext * to) / dt;
            vstep = (vnext - vprev) * tstep / dt;
         }
         else
         {
            v = vnext;
            vstep = 0.0;
         }

         // An adjustment if logarithmic scale.
         if( mDB )
         {
            v = pow(10.0, v);
            vstep = pow( 10.0, vstep );
         }

         buffer[b] = v;
      } else {
         if (mDB){
            buffer[b] = buffer[b - 1] * vstep;
         }else{
            buffer[b] = buffer[b - 1] + vstep;
         }
      }

      t += tstep;
   }
}

int Envelope::NumberOfPointsAfter(double t)
{
   if( t >= mEnv[mEnv.Count()-1]->t )
      return 0;
   else if( t < mEnv[0]->t )
      return mEnv.Count();
   else
   {
      int lo,hi;
      BinarySearchForTime( lo, hi, t );

      if( mEnv[hi]->t == t )
         return mEnv.Count() - (hi+1);
      else
         return mEnv.Count() - hi;
   }
}

double Envelope::NextPointAfter(double t)
{
   if( mEnv[mEnv.Count()-1]->t < t )
      return t;
   else if( t < mEnv[0]->t )
      return mEnv[0]->t;
   else
   {
      int lo,hi;
      BinarySearchForTime( lo, hi, t );
      if( mEnv[hi]->t == t )
         return mEnv[hi+1]->t;
      else
         return mEnv[hi]->t;
   }
}

double Envelope::Average( double t0, double t1 )
{
  if( t0 == t1 )
    return GetValue( t0 );
  else
    return Integral( t0, t1 ) / (t1 - t0);
}

//
// Integration and debugging functions
//
// The functions below are used by the TimeTrack and possibly for
// other debugging.  They do not affect normal amplitude envelopes
// for waveforms, nor frequency envelopes for equalization.
//

// We should be able to write a very efficient memoizer for this
// but make sure it gets reset when the envelope is changed.
double Envelope::Integral( double t0, double t1 )
{
   //printf( "\n\nIntegral:  t0=%f, t1=%f\n", t0, t1 );
   double total=0;
   
   if( t0 == t1 )
      return 0;
   if( t0 > t1 )
   {
      printf( "Odd things happening in Integral!\n" );
      return mDefaultValue;
   }
   
   unsigned int i = 0;
   double lastT, lastVal;
   
   // t0 is one of three cases:

   // 0) in an 'empty' envelope
   // 1) preceeding the first point
   // 2) enclosed by points
   // 3) following the last point

   if( mEnv.Count() < 1 )                      // 0: 'empty' envelope
   {
      return (t1 - t0) * mDefaultValue;  
   }
   else if( t0 < mEnv[0]->t )                  // 1: preceeds the first
   {
      if( t1 <= mEnv[0]->t ){
         return (t1 - t0) * mEnv[0]->val;
      }
      total += (mEnv[0]->t - t0) * mEnv[0]->val;
      lastT = mEnv[0]->t;
      lastVal = mEnv[0]->val;
   }
   else if( t0 >= mEnv[mEnv.Count()-1]->t )    // 3: follows the last
   {
      return (t1 - t0) * mEnv[mEnv.Count()-1]->val;
   }
   else 
   {                                         // 2: bracketed
      // Skip any points that come before t0 using binary search
      int lo,hi;
      BinarySearchForTime( lo, hi, t0 );
      i = lo;
      // i is now the point immediately before t0.
      lastVal = ((mEnv[i]->val * (mEnv[i+1]->t - t0))
         + (mEnv[i+1]->val *(t0 - mEnv[i]->t)))
         / (mEnv[i+1]->t - mEnv[i]->t); // value at t0
      lastT = t0;
   }

   // loop through the rest of the envelope points until we get to t1
   while (1)
   {

      if(i >= mEnv.Count()-1)
      {
         // the requested range extends beyond last point
         return total + (t1 - lastT) * lastVal;
      }
      else if (mEnv[i+1]->t >= t1)
      {
         // last,i+1 bracket t1
         double thisVal = ((mEnv[i]->val * (mEnv[i+1]->t - t1))
            + (mEnv[i+1]->val *(t1 - mEnv[i]->t)))
            / (mEnv[i+1]->t - mEnv[i]->t); 

         return total + (t1 - lastT) * (thisVal + lastVal) / 2;
      }
      else
      {
         // t1 still follows last,i+1
         total += (mEnv[i+1]->t - lastT) *  (mEnv[i+1]->val + lastVal) / 2;
         lastT = mEnv[i+1]->t;
         lastVal = mEnv[i+1]->val;
         i++;
      }
   }
}

// This one scales the y-axis before integrating.
// To re-scale [0,1] to [minY,maxY] we use the mapping y -> minY + (maxY - minY)y
// So we want to find the integral of (minY + (maxY - minY)f(t)), where f is our envelope.
// But that's just (t1 - t0)minY + (maxY - minY)Integral( t0, t1 ).
double Envelope::Integral( double t0, double t1, double minY, double maxY )
{
   return ((t1 - t0) * minY) + ((maxY - minY) * Integral( t0, t1 ));
}

void Envelope::print()
{
   for( unsigned int i = 0; i < mEnv.Count(); i++ )
      printf( "(%.2f, %.2f)\n", mEnv[i]->t, mEnv[i]->val );
}

void checkResult( int n, double a, double b )
{
   if( (a-b > 0 ? a-b : b-a) > 0.0000001 )
   {
      printf( "Envelope:  Result #%d is: %f, should be %f\n", n, a, b );
      //exit( -1 );
   }
}

void Envelope::testMe()
{
   double t0=0, t1=0;

   SetInterpolateDB(false);
   Mirror(false);

   SetDefaultValue(0.5);
   Flatten(0.5);
   checkResult( 1, Integral(0.0,100.0), 50);
   checkResult( 2, Integral(-10.0,10.0), 10);

   SetDefaultValue(1.0);
   Flatten(0.5);
   checkResult( 3, Integral(0.0,100.0), 50);
   checkResult( 4, Integral(-10.0,10.0), 10);
   checkResult( 5, Integral(-20.0,-10.0), 5);

   SetDefaultValue(0.5);
   Flatten(0.5);
   Insert( 5.0, 0.5 );
   checkResult( 6, Integral(0.0,100.0), 50);
   checkResult( 7, Integral(-10.0,10.0), 10);

   SetDefaultValue(0.5);
   Flatten(0.0);
   Insert( 0.0, 0.0 );
   Insert( 5.0, 1.0 );
   Insert( 10.0, 0.0 );
   t0 = 10.0 - .1;
   t1 = 10.0 + .1;
   double result = Integral(0.0,t1);
   double resulta = Integral(0.0,t0);
   double resultb = Integral(t0,t1);
   // Integrals should be additive
   checkResult( 8, result - resulta - resultb, 0);

   SetDefaultValue(0.5);
   Flatten(0.0);
   Insert( 0.0, 0.0 );
   Insert( 5.0, 1.0 );
   Insert( 10.0, 0.0 );
   t0 = 10.0 - .1;
   t1 = 10.0 + .1;
   checkResult( 9, Integral(0.0,t1), 5);
   checkResult( 10, Integral(0.0,t0), 4.999);
   checkResult( 11, Integral(t0,t1), .001);

   WX_CLEAR_ARRAY(mEnv);
   Insert( 0.0, 0.0 );
   Insert( 5.0, 1.0 );
   Insert( 10.0, 0.0 );
   checkResult( 12, NumberOfPointsAfter( -1 ), 3 );
   checkResult( 13, NumberOfPointsAfter( 0 ), 2 );
   checkResult( 14, NumberOfPointsAfter( 1 ), 2 );
   checkResult( 15, NumberOfPointsAfter( 5 ), 1 );
   checkResult( 16, NumberOfPointsAfter( 7 ), 1 );
   checkResult( 17, NumberOfPointsAfter( 10 ), 0 );
   checkResult( 18, NextPointAfter( 0 ), 5 );
   checkResult( 19, NextPointAfter( 5 ), 10 );
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 35b619bd-685f-45ee-89f0-bea14839de88

