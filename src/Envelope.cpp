/**********************************************************************

  Audacity: A Digital Audio Editor

  Envelope.cpp

  Dominic Mazzoni (original author)
  Dr William Bland (integration - the Calculus kind)
  Monty (xiphmont) (important bug fixes)

*******************************************************************//**

\class Envelope
\brief Piecewise linear or piecewise exponential function from double to double

  This class manages an envelope - i.e. a function
  that the user can edit by dragging control points around.  The
  envelope is most commonly used to control the amplitude of a
  waveform, but it is also used to shape the Equalization curve, and in
  TimeTrack to determine a time warp.

*//****************************************************************//**

\class EnvPoint
\brief EnvPoint, derived from XMLTagHandler, provides Envelope with
a draggable point type.

*//*******************************************************************/

#include "Envelope.h"



#include <math.h>

#include <wx/wxcrtvararg.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/textfile.h>
#include <wx/log.h>
#include <wx/utils.h>

static const double VALUE_TOLERANCE = 0.001;

Envelope::Envelope(bool exponential, double minValue, double maxValue, double defaultValue)
   : mDB(exponential)
   , mMinValue(minValue)
   , mMaxValue(maxValue)
   , mDefaultValue { ClampValue(defaultValue) }
{
}

Envelope::~Envelope()
{
}

bool Envelope::ConsistencyCheck()
{
   bool consistent = true;

   bool disorder;
   do {
      disorder = false;
      for ( size_t ii = 0, count = mEnv.size(); ii < count; ) {
         // Find range of points with equal T
         const double thisT = mEnv[ii].GetT();
         double nextT = 0.0f;
         auto nextI = ii + 1;
         while ( nextI < count && thisT == ( nextT = mEnv[nextI].GetT() ) )
            ++nextI;

         if ( nextI < count && nextT < thisT )
            disorder = true;

         while ( nextI - ii > 2 ) {
            // too many coincident time values
            if ((int)ii == mDragPoint || (int)nextI - 1 == mDragPoint)
               // forgivable
               ;
            else {
               consistent = false;
               // repair it
               Delete( nextI - 2 );
               if (mDragPoint >= (int)nextI - 2)
                  --mDragPoint;
               --nextI, --count;
               // wxLogError
            }
         }

         ii = nextI;
      }

      if (disorder) {
         consistent = false;
         // repair it
         std::stable_sort( mEnv.begin(), mEnv.end(),
            []( const EnvPoint &a, const EnvPoint &b )
               { return a.GetT() < b.GetT(); } );
      }
   } while ( disorder );

   return consistent;
}

/// Rescale function for time tracks (could also be used for other tracks though).
/// This is used to load old time track project files where the envelope used a 0 to 1
/// range instead of storing the actual time track values. This function will change the range of the envelope
/// and rescale all envelope points accordingly (unlike SetRange, which clamps the envelope points to the NEW range).
/// @minValue - the NEW minimum value
/// @maxValue - the NEW maximum value
void Envelope::RescaleValues(double minValue, double maxValue)
{
   double oldMinValue = mMinValue;
   double oldMaxValue = mMaxValue;
   mMinValue = minValue;
   mMaxValue = maxValue;

   // rescale the default value
   double factor = (mDefaultValue - oldMinValue) / (oldMaxValue - oldMinValue);
   mDefaultValue = ClampValue(mMinValue + (mMaxValue - mMinValue) * factor);

   // rescale all points
   for( unsigned int i = 0; i < mEnv.size(); i++ ) {
      factor = (mEnv[i].GetVal() - oldMinValue) / (oldMaxValue - oldMinValue);
      mEnv[i].SetVal( this, mMinValue + (mMaxValue - mMinValue) * factor );
   }

}

/// Flatten removes all points from the envelope to
/// make it horizontal at a chosen y-value.
/// @value - the y-value for the flat envelope.
void Envelope::Flatten(double value)
{
   mEnv.clear();
   mDefaultValue = ClampValue(value);
}

void Envelope::SetDragPoint(int dragPoint)
{
   mDragPoint = std::max(-1, std::min(int(mEnv.size() - 1), dragPoint));
   mDragPointValid = (mDragPoint >= 0);
}

void Envelope::SetDragPointValid(bool valid)
{
   mDragPointValid = (valid && mDragPoint >= 0);
   if (mDragPoint >= 0 && !valid) {
      // We're going to be deleting the point; On
      // screen we show this by having the envelope move to
      // the position it will have after deletion of the point.
      // Without deleting the point we move it left or right
      // to the same position as the previous or next point.

      static const double big = std::numeric_limits<double>::max();
      auto size = mEnv.size();

      if( size <= 1) {
         // There is only one point - just move it
         // off screen and at default height.
         // temporary state when dragging only!
         mEnv[mDragPoint].SetT(big);
         mEnv[mDragPoint].SetVal( this, mDefaultValue );
         return;
      }
      else if ( mDragPoint + 1 == (int)size ) {
         // Put the point at the height of the last point, but also off screen.
         mEnv[mDragPoint].SetT(big);
         mEnv[mDragPoint].SetVal( this, mEnv[ size - 1 ].GetVal() );
      }
      else {
         // Place it exactly on its right neighbour.
         // That way the drawing code will overpaint the dark dot with
         // a light dot, as if it were deleted.
         const auto &neighbor = mEnv[mDragPoint + 1];
         mEnv[mDragPoint].SetT(neighbor.GetT());
         mEnv[mDragPoint].SetVal( this, neighbor.GetVal() );
      }
   }
}

void Envelope::MoveDragPoint(double newWhen, double value)
{
   SetDragPointValid(true);
   if (!mDragPointValid)
      return;

   // We'll limit the drag point time to be between those of the preceding
   // and next envelope point.
   double limitLo = 0.0;
   double limitHi = mTrackLen;

   if (mDragPoint > 0)
      limitLo = std::max(limitLo, mEnv[mDragPoint - 1].GetT());
   if (mDragPoint + 1 < (int)mEnv.size())
      limitHi = std::min(limitHi, mEnv[mDragPoint + 1].GetT());

   EnvPoint &dragPoint = mEnv[mDragPoint];
   const double tt =
      std::max(limitLo, std::min(limitHi, newWhen));

   // This might temporary violate the constraint that at most two
   // points share a time value.
   dragPoint.SetT(tt);
   dragPoint.SetVal( this, value );
}

void Envelope::ClearDragPoint()
{
   if (!mDragPointValid && mDragPoint >= 0)
      Delete(mDragPoint);

   mDragPoint = -1;
   mDragPointValid = false;
}

void Envelope::SetRange(double minValue, double maxValue) {
   mMinValue = minValue;
   mMaxValue = maxValue;
   mDefaultValue = ClampValue(mDefaultValue);
   for( unsigned int i = 0; i < mEnv.size(); i++ )
      mEnv[i].SetVal( this, mEnv[i].GetVal() ); // this clamps the value to the NEW range
}

// This is used only during construction of an Envelope by complete or partial
// copy of another, or when truncating a track.
void Envelope::AddPointAtEnd( double t, double val )
{
   mEnv.push_back( EnvPoint{ t, val } );

   // Assume copied points were stored by nondecreasing time.
   // Allow no more than two points at exactly the same time.
   // Maybe that happened, because extra points were inserted at the boundary
   // of the copied range, which were not in the source envelope.
   auto nn = mEnv.size() - 1;
   while ( nn >= 2 && mEnv[ nn - 2 ].GetT() == t ) {
      // Of three or more points at the same time, erase one in the middle,
      // not the one newly added.
      mEnv.erase( mEnv.begin() + nn - 1 );
      --nn;
   }
}

Envelope::Envelope(const Envelope &orig, double t0, double t1)
   : mDB(orig.mDB)
   , mMinValue(orig.mMinValue)
   , mMaxValue(orig.mMaxValue)
   , mDefaultValue(orig.mDefaultValue)
{
   mOffset = wxMax(t0, orig.mOffset);
   mTrackLen = wxMin(t1, orig.mOffset + orig.mTrackLen) - mOffset;

   auto range1 = orig.EqualRange( t0 - orig.mOffset, 0 );
   auto range2 = orig.EqualRange( t1 - orig.mOffset, 0 );
   CopyRange(orig, range1.first, range2.second);
}

Envelope::Envelope(const Envelope &orig)
   : mDB(orig.mDB)
   , mMinValue(orig.mMinValue)
   , mMaxValue(orig.mMaxValue)
   , mDefaultValue(orig.mDefaultValue)
{
   mOffset = orig.mOffset;
   mTrackLen = orig.mTrackLen;
   CopyRange(orig, 0, orig.GetNumberOfPoints());
}

void Envelope::CopyRange(const Envelope &orig, size_t begin, size_t end)
{
   size_t len = orig.mEnv.size();
   size_t i = begin;

   // Create the point at 0 if it needs interpolated representation
   if ( i > 0 )
      AddPointAtEnd(0, orig.GetValue(mOffset));

   // Copy points from inside the copied region
   for (; i < end; ++i) {
      const EnvPoint &point = orig[i];
      const double when = point.GetT() + (orig.mOffset - mOffset);
      AddPointAtEnd(when, point.GetVal());
   }

   // Create the final point if it needs interpolated representation
   // If the last point of e was exactly at t1, this effectively copies it too.
   if (mTrackLen > 0 && i < len)
      AddPointAtEnd( mTrackLen, orig.GetValue(mOffset + mTrackLen));
}

#if 0
/// Limit() limits a double value to a range.
/// TODO: Move to a general utilities source file.
static double Limit( double Lo, double Value, double Hi )
{
   if( Value < Lo )
      return Lo;
   if( Value > Hi )
      return Hi;
   return Value;
}
#endif

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

   mEnv.clear();
   mEnv.reserve(numPoints);
   return true;
}

XMLTagHandler *Envelope::HandleXMLChild(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("controlpoint")))
      return NULL;

   mEnv.push_back( EnvPoint{} );
   return &mEnv.back();
}

void Envelope::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   unsigned int ctrlPt;

   xmlFile.StartTag(wxT("envelope"));
   xmlFile.WriteAttr(wxT("numpoints"), mEnv.size());

   for (ctrlPt = 0; ctrlPt < mEnv.size(); ctrlPt++) {
      const EnvPoint &point = mEnv[ctrlPt];
      xmlFile.StartTag(wxT("controlpoint"));
      xmlFile.WriteAttr(wxT("t"), point.GetT(), 12);
      xmlFile.WriteAttr(wxT("val"), point.GetVal(), 12);
      xmlFile.EndTag(wxT("controlpoint"));
   }

   xmlFile.EndTag(wxT("envelope"));
}

void Envelope::Delete( int point )
{
   mEnv.erase(mEnv.begin() + point);
}

void Envelope::Insert(int point, const EnvPoint &p)
{
   mEnv.insert(mEnv.begin() + point, p);
}

void Envelope::Insert(double when, double value)
{
   mEnv.push_back( EnvPoint{ when, value });
}

/*! @excsafety{No-fail} */
void Envelope::CollapseRegion( double t0, double t1, double sampleDur )
{
   if ( t1 <= t0 )
      return;

   // This gets called when somebody clears samples.

   // Snip points in the interval (t0, t1), shift values left at times after t1.
   // For the boundaries of the interval, preserve the left-side limit at the
   // start and right-side limit at the end.

   const auto epsilon = sampleDur / 2;
   t0 = std::max( 0.0, std::min( mTrackLen, t0 - mOffset ) );
   t1 = std::max( 0.0, std::min( mTrackLen, t1 - mOffset ) );
   bool leftPoint = true, rightPoint = true;

   // Determine the start of the range of points to remove from the array.
   auto range0 = EqualRange( t0, 0 );
   auto begin = range0.first;
   if ( begin == range0.second ) {
      if ( t0 > epsilon ) {
         // There was no point exactly at t0;
         // insert a point to preserve the value.
         auto val = GetValueRelative( t0 );
         InsertOrReplaceRelative( t0, val );
         ++begin;
      }
      else
         leftPoint = false;
   }
   else
      // We will keep the first (or only) point that was at t0.
      ++begin;

   // We want end to be the index one past the range of points to remove from
   // the array.
   // At first, find index of the first point after t1:
   auto range1 = EqualRange( t1, 0 );
   auto end = range1.second;
   if ( range1.first == end ) {
      if ( mTrackLen - t1 > epsilon ) {
         // There was no point exactly at t1; insert a point to preserve the value.
         auto val = GetValueRelative( t1 );
         InsertOrReplaceRelative( t1, val );
         // end is now the index of this NEW point and that is correct.
      }
      else
         rightPoint = false;
   }
   else
      // We will keep the last (or only) point that was at t1.
      --end;

   if ( end < begin ) {
      if ( leftPoint )
         rightPoint = false;
   }
   else
      mEnv.erase( mEnv.begin() + begin, mEnv.begin() + end );

   // Shift points left after deleted region.
   auto len = mEnv.size();
   for ( size_t i = begin; i < len; ++i ) {
      auto &point = mEnv[i];
      if (rightPoint && (int)i == begin)
         // Avoid roundoff error.
         // Make exactly equal times of neighboring points so that we have
         // a real discontinuity.
         point.SetT( t0 );
      else
         point.SetT( point.GetT() - (t1 - t0) );
   }

   // See if the discontinuity is removable.
   if ( rightPoint )
      RemoveUnneededPoints( begin, true );
   if ( leftPoint )
      RemoveUnneededPoints( begin - 1, false );

   mTrackLen -= ( t1 - t0 );
}

// This operation is trickier than it looks; the basic rub is that
// a track's envelope runs the range from t=0 to t=tracklen; the t=0
// envelope point applies to the first sample, but the t=tracklen
// envelope point applies one-past the last actual sample.
// t0 should be in the domain of this; if not, it is trimmed.
/*! @excsafety{No-fail} */
void Envelope::PasteEnvelope( double t0, const Envelope *e, double sampleDur )
{
   const bool wasEmpty = (this->mEnv.size() == 0);
   auto otherSize = e->mEnv.size();
   const double otherDur = e->mTrackLen;
   const auto otherOffset = e->mOffset;
   const auto deltat = otherOffset + otherDur;

   if ( otherSize == 0 && wasEmpty && e->mDefaultValue == this->mDefaultValue )
   {
      // msmeyer: The envelope is empty and has the same default value, so
      // there is nothing that must be inserted, just return. This avoids
      // the creation of unnecessary duplicate control points
      // MJS: but the envelope does get longer
      // PRL:  Assuming t0 is in the domain of the envelope
      mTrackLen += deltat;
      return;
   }

   // Make t0 relative to the offset of the envelope we are pasting into, 
   // and trim it to the domain of this
   t0 = std::min( mTrackLen, std::max( 0.0, t0 - mOffset ) );

   // Adjust if the insertion point rounds off near a discontinuity in this
   if ( true )
   {
      double newT0;
      auto range = EqualRange( t0, sampleDur );
      auto index = range.first;
      if ( index + 2 == range.second &&
           ( newT0 = mEnv[ index ].GetT() ) == mEnv[ 1 + index ].GetT() )
         t0 = newT0;
   }

   // Open up a space
   double leftVal = e->GetValue( 0 );
   double rightVal = e->GetValueRelative( otherDur );
   // This range includes the right-side limit of the left end of the space,
   // and the left-side limit of the right end:
   const auto range = ExpandRegion( t0, deltat, &leftVal, &rightVal );
   // Where to put the copied points from e -- after the first of the
   // two points in range:
   auto insertAt = range.first + 1;

   // Copy points from e -- maybe skipping those at the extremes
   auto end = e->mEnv.end();
   if ( otherSize != 0 && e->mEnv[ otherSize - 1 ].GetT() == otherDur )
      // ExpandRegion already made an equivalent limit point
      --end, --otherSize;
   auto begin = e->mEnv.begin();
   if ( otherSize != 0 && otherOffset == 0.0 && e->mEnv[ 0 ].GetT() == 0.0 )
      ++begin, --otherSize;
   mEnv.insert( mEnv.begin() + insertAt, begin, end );

   // Adjust their times
   for ( size_t index = insertAt, last = insertAt + otherSize;
         index < last; ++index ) {
      auto &point = mEnv[ index ];
      // The mOffset of the envelope-pasted-from is irrelevant.
      // The GetT() times in it are relative to its start.
      // The new GetT() times are relative to the envelope-pasted-to start.
      // We are pasting at t0 relative to the envelope-pasted-to start.
      // Hence we adjust by just t0.
      // Bug 1844 was that we also adjusted by the envelope-pasted-from offset.
      point.SetT( point.GetT() + /*otherOffset +*/ t0 );
   }

   // Treat removable discontinuities
   // Right edge outward:
   RemoveUnneededPoints( insertAt + otherSize + 1, true );
   // Right edge inward:
   RemoveUnneededPoints( insertAt + otherSize, false, false );

   // Left edge inward:
   RemoveUnneededPoints( range.first, true, false );
   // Left edge outward:
   RemoveUnneededPoints( range.first - 1, false );

   // Guarantee monotonicity of times, against little round-off mistakes perhaps
   ConsistencyCheck();
}

/*! @excsafety{No-fail} */
void Envelope::RemoveUnneededPoints
   ( size_t startAt, bool rightward, bool testNeighbors )
{
   // startAt is the index of a recently inserted point which might make no
   // difference in envelope evaluation, or else might cause nearby points to
   // make no difference.

   auto isDiscontinuity = [this]( size_t index ) {
      // Assume array accesses are in-bounds
      const EnvPoint &point1 = mEnv[ index ];
      const EnvPoint &point2 = mEnv[ index + 1 ];
      return point1.GetT() == point2.GetT() &&
         fabs( point1.GetVal() - point2.GetVal() ) > VALUE_TOLERANCE;
   };

   auto remove = [this]( size_t index, bool leftLimit ) {
      // Assume array accesses are in-bounds
      const auto &point = mEnv[ index ];
      auto when = point.GetT();
      auto val = point.GetVal();
      Delete( index );  // try it to see if it's doing anything
      auto val1 = GetValueRelative ( when, leftLimit );
      if( fabs( val - val1 ) > VALUE_TOLERANCE ) {
         // put it back, we needed it
         Insert( index, EnvPoint{ when, val } );
         return false;
      }
      else
         return true;
   };

   auto len = mEnv.size();

   bool leftLimit =
      !rightward && startAt + 1 < len && isDiscontinuity( startAt );

   bool removed = remove( startAt, leftLimit );

   if ( removed )
      // The given point was removable.  Done!
      return;

   if ( !testNeighbors )
      return;

   // The given point was not removable.  But did its insertion make nearby
   // points removable?

   int index = startAt + ( rightward ? 1 : -1 );
   while ( index >= 0 && index < (int)len ) {
      // Stop at any discontinuity
      if ( index > 0       && isDiscontinuity( index - 1 ) )
         break;
      if ( (index + 1) < (int)len && isDiscontinuity( index ) )
         break;

      if ( ! remove( index, false ) )
         break;

      --len;
      if ( ! rightward )
         --index;
   }
}

/*! @excsafety{No-fail} */
std::pair< int, int > Envelope::ExpandRegion
   ( double t0, double tlen, double *pLeftVal, double *pRightVal )
{
   // t0 is relative time

   double val = GetValueRelative( t0 );
   const auto range = EqualRange( t0, 0 );

   // Preserve the left-side limit.
   int index = 1 + range.first;
   if ( index <= range.second )
      // There is already a control point.
      ;
   else {
      // Make a control point.
      Insert( range.first, EnvPoint{ t0, val } );
   }

   // Shift points.
   auto len = mEnv.size();
   for ( unsigned int ii = index; ii < len; ++ii ) {
      auto &point = mEnv[ ii ];
      point.SetT( point.GetT() + tlen );
   }

   mTrackLen += tlen;
   
   // Preserve the right-side limit.
   if ( index < range.second )
      // There was a control point already.
      ;
   else
      // Make a control point.
      Insert( index, EnvPoint{ t0 + tlen, val } );

   // Make discontinuities at ends, maybe:

   if ( pLeftVal )
      // Make a discontinuity at the left side of the expansion
      Insert( index++, EnvPoint{ t0, *pLeftVal } );

   if ( pRightVal )
      // Make a discontinuity at the right side of the expansion
      Insert( index++, EnvPoint{ t0 + tlen, *pRightVal } );

   // Return the range of indices that includes the inside limiting points,
   // none, one, or two
   return { 1 + range.first, index };
}

/*! @excsafety{No-fail} */
void Envelope::InsertSpace( double t0, double tlen )
{
   auto range = ExpandRegion( t0 - mOffset, tlen, nullptr, nullptr );

   // Simplify the boundaries if possible
   RemoveUnneededPoints( range.second, true );
   RemoveUnneededPoints( range.first - 1, false );
}

int Envelope::Reassign(double when, double value)
{
   when -= mOffset;

   int len = mEnv.size();
   if (len == 0)
      return -1;

   int i = 0;
   while (i < len && when > mEnv[i].GetT())
      i++;

   if (i >= len || when < mEnv[i].GetT())
      return -1;

   mEnv[i].SetVal( this, value );
   return 0;
}


size_t Envelope::GetNumberOfPoints() const
{
   return mEnv.size();
}

void Envelope::GetPoints(double *bufferWhen,
                         double *bufferValue,
                         int bufferLen) const
{
   int n = mEnv.size();
   if (n > bufferLen)
      n = bufferLen;
   int i;
   for (i = 0; i < n; i++) {
      bufferWhen[i] = mEnv[i].GetT() - mOffset;
      bufferValue[i] = mEnv[i].GetVal();
   }
}

void Envelope::Cap( double sampleDur )
{
   auto range = EqualRange( mTrackLen, sampleDur );
   if ( range.first == range.second )
      InsertOrReplaceRelative( mTrackLen, GetValueRelative( mTrackLen ) );
}

// Private methods

/** @brief Add a control point to the envelope
 *
 * @param when the time in seconds when the envelope point should be created.
 * @param value the envelope value to use at the given point.
 * @return the index of the NEW envelope point within array of envelope points.
 */
int Envelope::InsertOrReplaceRelative(double when, double value)
{
#if defined(_DEBUG)
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

   when = std::max( 0.0, std::min( mTrackLen, when ) );

   auto range = EqualRange( when, 0 );
   int index = range.first;

   if ( index < range.second )
      // modify existing
      // In case of a discontinuity, ALWAYS CHANGING LEFT LIMIT ONLY!
      mEnv[ index ].SetVal( this, value );
   else
     // Add NEW
      Insert( index, EnvPoint { when, value } );

   return index;
}

std::pair<int, int> Envelope::EqualRange( double when, double sampleDur ) const
{
   // Find range of envelope points matching the given time coordinate
   // (within an interval of length sampleDur)
   // by binary search; if empty, it still indicates where to
   // insert.
   const auto tolerance = sampleDur / 2;
   auto begin = mEnv.begin();
   auto end = mEnv.end();
   auto first = std::lower_bound(
      begin, end,
      EnvPoint{ when - tolerance, 0.0 },
      []( const EnvPoint &point1, const EnvPoint &point2 )
         { return point1.GetT() < point2.GetT(); }
   );
   auto after = first;
   while ( after != end && after->GetT() <= when + tolerance )
      ++after;
   return { first - begin, after - begin };
}

// Control

/*! @excsafety{No-fail} */
void Envelope::SetOffset(double newOffset)
{
   mOffset = newOffset;
}

/*! @excsafety{No-fail} */
void Envelope::SetTrackLen( double trackLen, double sampleDur )
{
   // Preserve the left-side limit at trackLen.
   auto range = EqualRange( trackLen, sampleDur );
   bool needPoint = ( range.first == range.second && trackLen < mTrackLen );
   double value=0.0;
   if ( needPoint )
      value = GetValueRelative( trackLen );

   mTrackLen = trackLen;

   // Shrink the array.
   // If more than one point already at the end, keep only the first of them.
   int newLen = std::min( 1 + range.first, range.second );
   mEnv.resize( newLen );

   if ( needPoint )
      AddPointAtEnd( mTrackLen, value );
}

/*! @excsafety{No-fail} */
void Envelope::RescaleTimes( double newLength )
{
   if ( mTrackLen == 0 ) {
      for ( auto &point : mEnv )
         point.SetT( 0 );
   }
   else {
      auto ratio = newLength / mTrackLen;
      for ( auto &point : mEnv )
         point.SetT( point.GetT() * ratio );
   }
   mTrackLen = newLength;
}

// Accessors
double Envelope::GetValue( double t, double sampleDur ) const
{
   // t is absolute time
   double temp;

   GetValues( &temp, 1, t, sampleDur );
   return temp;
}

double Envelope::GetValueRelative(double t, bool leftLimit) const
{
   double temp;

   GetValuesRelative(&temp, 1, t, 0.0, leftLimit);
   return temp;
}

// relative time
/// @param Lo returns last index at or before this time, maybe -1
/// @param Hi returns first index after this time, maybe past the end
void Envelope::BinarySearchForTime( int &Lo, int &Hi, double t ) const
{
   // Optimizations for the usual pattern of repeated calls with
   // small increases of t.
   {
      if (mSearchGuess >= 0 && mSearchGuess < (int)mEnv.size()) {
         if (t >= mEnv[mSearchGuess].GetT() &&
             (1 + mSearchGuess == (int)mEnv.size() ||
              t < mEnv[1 + mSearchGuess].GetT())) {
            Lo = mSearchGuess;
            Hi = 1 + mSearchGuess;
            return;
         }
      }

      ++mSearchGuess;
      if (mSearchGuess >= 0 && mSearchGuess < (int)mEnv.size()) {
         if (t >= mEnv[mSearchGuess].GetT() &&
             (1 + mSearchGuess == (int)mEnv.size() ||
              t < mEnv[1 + mSearchGuess].GetT())) {
            Lo = mSearchGuess;
            Hi = 1 + mSearchGuess;
            return;
         }
      }
   }

   Lo = -1;
   Hi = mEnv.size();

   // Invariants:  Lo is not less than -1, Hi not more than size
   while (Hi > (Lo + 1)) {
      int mid = (Lo + Hi) / 2;
      // mid must be strictly between Lo and Hi, therefore a valid index
      if (t < mEnv[mid].GetT())
         Hi = mid;
      else
         Lo = mid;
   }
   wxASSERT( Hi == ( Lo+1 ));

   mSearchGuess = Lo;
}

// relative time
/// @param Lo returns last index before this time, maybe -1
/// @param Hi returns first index at or after this time, maybe past the end
void Envelope::BinarySearchForTime_LeftLimit( int &Lo, int &Hi, double t ) const
{
   Lo = -1;
   Hi = mEnv.size();

   // Invariants:  Lo is not less than -1, Hi not more than size
   while (Hi > (Lo + 1)) {
      int mid = (Lo + Hi) / 2;
      // mid must be strictly between Lo and Hi, therefore a valid index
      if (t <= mEnv[mid].GetT())
         Hi = mid;
      else
         Lo = mid;
   }
   wxASSERT( Hi == ( Lo+1 ));

   mSearchGuess = Lo;
}

/// GetInterpolationStartValueAtPoint() is used to select either the
/// envelope value or its log depending on whether we are doing linear
/// or log interpolation.
/// @param iPoint index in env array to look at.
/// @return value there, or its (safe) log10.
double Envelope::GetInterpolationStartValueAtPoint( int iPoint ) const
{
   double v = mEnv[ iPoint ].GetVal();
   if( !mDB )
      return v;
   else
      return log10(v);
}

void Envelope::GetValues( double *buffer, int bufferLen,
                          double t0, double tstep ) const
{
   // Convert t0 from absolute to clip-relative time
   t0 -= mOffset;
   GetValuesRelative( buffer, bufferLen, t0, tstep);
}

void Envelope::GetValuesRelative
   (double *buffer, int bufferLen, double t0, double tstep, bool leftLimit)
   const
{
   // JC: If bufferLen ==0 we have probably just allocated a zero sized buffer.
   // wxASSERT( bufferLen > 0 );

   const auto epsilon = tstep / 2;
   int len = mEnv.size();

   double t = t0;
   double increment = 0;
   if ( len > 1 && t <= mEnv[0].GetT() && mEnv[0].GetT() == mEnv[1].GetT() )
      increment = leftLimit ? -epsilon : epsilon;

   double tprev, vprev, tnext = 0, vnext, vstep = 0;

   for (int b = 0; b < bufferLen; b++) {

      // Get easiest cases out the way first...
      // IF empty envelope THEN default value
      if (len <= 0) {
         buffer[b] = mDefaultValue;
         t += tstep;
         continue;
      }

      auto tplus = t + increment;

      // IF before envelope THEN first value
      if ( leftLimit ? tplus <= mEnv[0].GetT() : tplus < mEnv[0].GetT() ) {
         buffer[b] = mEnv[0].GetVal();
         t += tstep;
         continue;
      }
      // IF after envelope THEN last value
      if ( leftLimit
            ? tplus > mEnv[len - 1].GetT() : tplus >= mEnv[len - 1].GetT() ) {
         buffer[b] = mEnv[len - 1].GetVal();
         t += tstep;
         continue;
      }

      // be careful to get the correct limit even in case epsilon == 0
      if ( b == 0 ||
           ( leftLimit ? tplus > tnext : tplus >= tnext ) ) {

         // We're beyond our tnext, so find the next one.
         // Don't just increment lo or hi because we might
         // be zoomed far out and that could be a large number of
         // points to move over.  That's why we binary search.

         int lo,hi;
         if ( leftLimit )
            BinarySearchForTime_LeftLimit( lo, hi, tplus );
         else
            BinarySearchForTime( lo, hi, tplus );

         // mEnv[0] is before tplus because of eliminations above, therefore lo >= 0
         // mEnv[len - 1] is after tplus, therefore hi <= len - 1
         wxASSERT( lo >= 0 && hi <= len - 1 );

         tprev = mEnv[lo].GetT();
         tnext = mEnv[hi].GetT();

         if ( hi + 1 < len && tnext == mEnv[ hi + 1 ].GetT() )
            // There is a discontinuity after this point-to-point interval.
            // Usually will stop evaluating in this interval when time is slightly
            // before tNext, then use the right limit.
            // This is the right intent
            // in case small roundoff errors cause a sample time to be a little
            // before the envelope point time.
            // Less commonly we want a left limit, so we continue evaluating in
            // this interval until shortly after the discontinuity.
            increment = leftLimit ? -epsilon : epsilon;
         else
            increment = 0;

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

// relative time
int Envelope::NumberOfPointsAfter(double t) const
{
   int lo,hi;
   BinarySearchForTime( lo, hi, t );

   return mEnv.size() - hi;
}

// relative time
double Envelope::NextPointAfter(double t) const
{
   int lo,hi;
   BinarySearchForTime( lo, hi, t );
   if (hi >= (int)mEnv.size())
      return t;
   else
      return mEnv[hi].GetT();
}

double Envelope::Average( double t0, double t1 ) const
{
  if( t0 == t1 )
    return GetValue( t0 );
  else
    return Integral( t0, t1 ) / (t1 - t0);
}

double Envelope::AverageOfInverse( double t0, double t1 ) const
{
  if( t0 == t1 )
    return 1.0 / GetValue( t0 );
  else
    return IntegralOfInverse( t0, t1 ) / (t1 - t0);
}

//
// Integration and debugging functions
//
// The functions below are used by the TimeTrack and possibly for
// other debugging.  They do not affect normal amplitude envelopes
// for waveforms, nor frequency envelopes for equalization.
// The 'Average' function also uses 'Integral'.
//

// A few helper functions to make the code below more readable.
static double InterpolatePoints(double y1, double y2, double factor, bool logarithmic)
{
   if(logarithmic)
      // you can use any base you want, it doesn't change the result
      return exp(log(y1) * (1.0 - factor) + log(y2) * factor);
   else
      return y1 * (1.0 - factor) + y2 * factor;
}
static double IntegrateInterpolated(double y1, double y2, double time, bool logarithmic)
{
   // Calculates: integral(interpolate(y1, y2, x), x = 0 .. time)
   // Integrating logarithmic interpolated segments is surprisingly simple. You can check this formula here:
   // http://www.wolframalpha.com/input/?i=integrate+10%5E%28log10%28y1%29*%28T-x%29%2FT%2Blog10%28y2%29*x%2FT%29+from+0+to+T
   // Again, the base you use for interpolation is irrelevant, the formula below should always use the natural
   // logarithm (i.e. 'log' in C/C++). If the denominator is too small, it's better to use linear interpolation
   // because the rounding errors would otherwise get too large. The threshold value is 1.0e-5 because at that
   // point the rounding errors become larger than the difference between linear and logarithmic (I tested this in Octave).
   if(logarithmic)
   {
      double l = log(y1 / y2);
      if(fabs(l) < 1.0e-5) // fall back to linear interpolation
         return (y1 + y2) * 0.5 * time;
      return (y1 - y2) / l * time;
   }
   else
   {
      return (y1 + y2) * 0.5 * time;
   }
}
static double IntegrateInverseInterpolated(double y1, double y2, double time, bool logarithmic)
{
   // Calculates: integral(1 / interpolate(y1, y2, x), x = 0 .. time)
   // This one is a bit harder. Linear:
   // http://www.wolframalpha.com/input/?i=integrate+1%2F%28y1*%28T-x%29%2FT%2By2*x%2FT%29+from+0+to+T
   // Logarithmic:
   // http://www.wolframalpha.com/input/?i=integrate+1%2F%2810%5E%28log10%28y1%29*%28T-x%29%2FT%2Blog10%28y2%29*x%2FT%29%29+from+0+to+T
   // Here both cases need a special case for y1 == y2. The threshold is 1.0e5 again, this is still the
   // best value in both cases.
   double l = log(y1 / y2);
   if(fabs(l) < 1.0e-5) // fall back to average
      return 2.0 / (y1 + y2) * time;
   if(logarithmic)
      return (y1 - y2) / (l * y1 * y2) * time;
   else
      return l / (y1 - y2) * time;
}
static double SolveIntegrateInverseInterpolated(double y1, double y2, double time, double area, bool logarithmic)
{
   // Calculates: solve (integral(1 / interpolate(y1, y2, x), x = 0 .. res) = area) for res
   // Don't try to derive these formulas by hand :). The threshold is 1.0e5 again.
   double a = area / time, res;
   if(logarithmic)
   {
      double l = log(y1 / y2);
      if(fabs(l) < 1.0e-5) // fall back to average
         res = a * (y1 + y2) * 0.5;
      else if(1.0 + a * y1 * l <= 0.0)
         res = 1.0;
      else
         res = log1p(a * y1 * l) / l;
   }
   else
   {
      if(fabs(y2 - y1) < 1.0e-5) // fall back to average
         res = a * (y1 + y2) * 0.5;
      else
         res = y1 * expm1(a * (y2 - y1)) / (y2 - y1);
   }
   return std::max(0.0, std::min(1.0, res)) * time;
}

// We should be able to write a very efficient memoizer for this
// but make sure it gets reset when the envelope is changed.
double Envelope::Integral( double t0, double t1 ) const
{
   if(t0 == t1)
      return 0.0;
   if(t0 > t1)
   {
      return -Integral(t1, t0); // this makes more sense than returning the default value
   }

   unsigned int count = mEnv.size();
   if(count == 0) // 'empty' envelope
      return (t1 - t0) * mDefaultValue;

   t0 -= mOffset;
   t1 -= mOffset;

   double total = 0.0, lastT, lastVal;
   unsigned int i; // this is the next point to check
   if(t0 < mEnv[0].GetT()) // t0 preceding the first point
   {
      if(t1 <= mEnv[0].GetT())
         return (t1 - t0) * mEnv[0].GetVal();
      i = 1;
      lastT = mEnv[0].GetT();
      lastVal = mEnv[0].GetVal();
      total += (lastT - t0) * lastVal;
   }
   else if(t0 >= mEnv[count - 1].GetT()) // t0 at or following the last point
   {
      return (t1 - t0) * mEnv[count - 1].GetVal();
   }
   else // t0 enclosed by points
   {
      // Skip any points that come before t0 using binary search
      int lo, hi;
      BinarySearchForTime(lo, hi, t0);
      lastVal = InterpolatePoints(mEnv[lo].GetVal(), mEnv[hi].GetVal(), (t0 - mEnv[lo].GetT()) / (mEnv[hi].GetT() - mEnv[lo].GetT()), mDB);
      lastT = t0;
      i = hi; // the point immediately after t0.
   }

   // loop through the rest of the envelope points until we get to t1
   while (1)
   {
      if(i >= count) // the requested range extends beyond the last point
      {
         return total + (t1 - lastT) * lastVal;
      }
      else if(mEnv[i].GetT() >= t1) // this point follows the end of the range
      {
         double thisVal = InterpolatePoints(mEnv[i - 1].GetVal(), mEnv[i].GetVal(), (t1 - mEnv[i - 1].GetT()) / (mEnv[i].GetT() - mEnv[i - 1].GetT()), mDB);
         return total + IntegrateInterpolated(lastVal, thisVal, t1 - lastT, mDB);
      }
      else // this point precedes the end of the range
      {
         total += IntegrateInterpolated(lastVal, mEnv[i].GetVal(), mEnv[i].GetT() - lastT, mDB);
         lastT = mEnv[i].GetT();
         lastVal = mEnv[i].GetVal();
         i++;
      }
   }
}

double Envelope::IntegralOfInverse( double t0, double t1 ) const
{
   if(t0 == t1)
      return 0.0;
   if(t0 > t1)
   {
      return -IntegralOfInverse(t1, t0); // this makes more sense than returning the default value
   }

   unsigned int count = mEnv.size();
   if(count == 0) // 'empty' envelope
      return (t1 - t0) / mDefaultValue;

   t0 -= mOffset;
   t1 -= mOffset;

   double total = 0.0, lastT, lastVal;
   unsigned int i; // this is the next point to check
   if(t0 < mEnv[0].GetT()) // t0 preceding the first point
   {
      if(t1 <= mEnv[0].GetT())
         return (t1 - t0) / mEnv[0].GetVal();
      i = 1;
      lastT = mEnv[0].GetT();
      lastVal = mEnv[0].GetVal();
      total += (lastT - t0) / lastVal;
   }
   else if(t0 >= mEnv[count - 1].GetT()) // t0 at or following the last point
   {
      return (t1 - t0) / mEnv[count - 1].GetVal();
   }
   else // t0 enclosed by points
   {
      // Skip any points that come before t0 using binary search
      int lo, hi;
      BinarySearchForTime(lo, hi, t0);
      lastVal = InterpolatePoints(mEnv[lo].GetVal(), mEnv[hi].GetVal(), (t0 - mEnv[lo].GetT()) / (mEnv[hi].GetT() - mEnv[lo].GetT()), mDB);
      lastT = t0;
      i = hi; // the point immediately after t0.
   }

   // loop through the rest of the envelope points until we get to t1
   while (1)
   {
      if(i >= count) // the requested range extends beyond the last point
      {
         return total + (t1 - lastT) / lastVal;
      }
      else if(mEnv[i].GetT() >= t1) // this point follows the end of the range
      {
         double thisVal = InterpolatePoints(mEnv[i - 1].GetVal(), mEnv[i].GetVal(), (t1 - mEnv[i - 1].GetT()) / (mEnv[i].GetT() - mEnv[i - 1].GetT()), mDB);
         return total + IntegrateInverseInterpolated(lastVal, thisVal, t1 - lastT, mDB);
      }
      else // this point precedes the end of the range
      {
         total += IntegrateInverseInterpolated(lastVal, mEnv[i].GetVal(), mEnv[i].GetT() - lastT, mDB);
         lastT = mEnv[i].GetT();
         lastVal = mEnv[i].GetVal();
         i++;
      }
   }
}

double Envelope::SolveIntegralOfInverse( double t0, double area ) const
{
   if(area == 0.0)
      return t0;

   const auto count = mEnv.size();
   if(count == 0) // 'empty' envelope
      return t0 + area * mDefaultValue;

   // Correct for offset!
   t0 -= mOffset;
   return mOffset + [&] {
      // Now we can safely assume t0 is relative time!
      double lastT, lastVal;
      int i; // this is the next point to check
      if(t0 < mEnv[0].GetT()) // t0 preceding the first point
      {
         if (area < 0) {
            return t0 + area * mEnv[0].GetVal();
         }
         else {
            i = 1;
            lastT = mEnv[0].GetT();
            lastVal = mEnv[0].GetVal();
            double added = (lastT - t0) / lastVal;
            if(added >= area)
               return t0 + area * mEnv[0].GetVal();
            area -= added;
         }
      }
      else if(t0 >= mEnv[count - 1].GetT()) // t0 at or following the last point
      {
         if (area < 0) {
            i = (int)count - 2;
            lastT = mEnv[count - 1].GetT();
            lastVal = mEnv[count - 1].GetVal();
            double added = (lastT - t0) / lastVal; // negative
            if(added <= area)
               return t0 + area * mEnv[count - 1].GetVal();
            area -= added;
         }
         else {
            return t0 + area * mEnv[count - 1].GetVal();
         }
      }
      else // t0 enclosed by points
      {
         // Skip any points that come before t0 using binary search
         int lo, hi;
         BinarySearchForTime(lo, hi, t0);
         lastVal = InterpolatePoints(mEnv[lo].GetVal(), mEnv[hi].GetVal(), (t0 - mEnv[lo].GetT()) / (mEnv[hi].GetT() - mEnv[lo].GetT()), mDB);
         lastT = t0;
         if (area < 0)
            i = lo;
         else
            i = hi; // the point immediately after t0.
      }

      if (area < 0) {
         // loop BACKWARDS through the rest of the envelope points until we get to t1
         // (which is less than t0)
         while (1)
         {
            if(i < 0) // the requested range extends beyond the leftmost point
            {
               return lastT + area * lastVal;
            }
            else
            {
               double added =
                  -IntegrateInverseInterpolated(mEnv[i].GetVal(), lastVal, lastT - mEnv[i].GetT(), mDB);
               if(added <= area)
                  return lastT - SolveIntegrateInverseInterpolated(lastVal, mEnv[i].GetVal(), lastT - mEnv[i].GetT(), -area, mDB);
               area -= added;
               lastT = mEnv[i].GetT();
               lastVal = mEnv[i].GetVal();
               --i;
            }
         }
      }
      else {
         // loop through the rest of the envelope points until we get to t1
         while (1)
         {
            if(i >= (int)count) // the requested range extends beyond the last point
            {
               return lastT + area * lastVal;
            }
            else
            {
               double added = IntegrateInverseInterpolated(lastVal, mEnv[i].GetVal(), mEnv[i].GetT() - lastT, mDB);
               if(added >= area)
                  return lastT + SolveIntegrateInverseInterpolated(lastVal, mEnv[i].GetVal(), mEnv[i].GetT() - lastT, area, mDB);
               area -= added;
               lastT = mEnv[i].GetT();
               lastVal = mEnv[i].GetVal();
               i++;
            }
         }
      }
   }();
}

void Envelope::print() const
{
   for( unsigned int i = 0; i < mEnv.size(); i++ )
      wxPrintf( "(%.2f, %.2f)\n", mEnv[i].GetT(), mEnv[i].GetVal() );
}

static void checkResult( int n, double a, double b )
{
   if( (a-b > 0 ? a-b : b-a) > 0.0000001 )
   {
      wxPrintf( "Envelope:  Result #%d is: %f, should be %f\n", n, a, b );
      //exit( -1 );
   }
}

void Envelope::testMe()
{
   double t0=0, t1=0;

   SetExponential(false);

   Flatten(0.5);
   checkResult( 1, Integral(0.0,100.0), 50);
   checkResult( 2, Integral(-10.0,10.0), 10);

   Flatten(0.5);
   checkResult( 3, Integral(0.0,100.0), 50);
   checkResult( 4, Integral(-10.0,10.0), 10);
   checkResult( 5, Integral(-20.0,-10.0), 5);

   Flatten(0.5);
   InsertOrReplaceRelative( 5.0, 0.5 );
   checkResult( 6, Integral(0.0,100.0), 50);
   checkResult( 7, Integral(-10.0,10.0), 10);

   Flatten(0.0);
   InsertOrReplaceRelative( 0.0, 0.0 );
   InsertOrReplaceRelative( 5.0, 1.0 );
   InsertOrReplaceRelative( 10.0, 0.0 );
   t0 = 10.0 - .1;
   t1 = 10.0 + .1;
   double result = Integral(0.0,t1);
   double resulta = Integral(0.0,t0);
   double resultb = Integral(t0,t1);
   // Integrals should be additive
   checkResult( 8, result - resulta - resultb, 0);

   Flatten(0.0);
   InsertOrReplaceRelative( 0.0, 0.0 );
   InsertOrReplaceRelative( 5.0, 1.0 );
   InsertOrReplaceRelative( 10.0, 0.0 );
   t0 = 10.0 - .1;
   t1 = 10.0 + .1;
   checkResult( 9, Integral(0.0,t1), 5);
   checkResult( 10, Integral(0.0,t0), 4.999);
   checkResult( 11, Integral(t0,t1), .001);

   mEnv.clear();
   InsertOrReplaceRelative( 0.0, 0.0 );
   InsertOrReplaceRelative( 5.0, 1.0 );
   InsertOrReplaceRelative( 10.0, 0.0 );
   checkResult( 12, NumberOfPointsAfter( -1 ), 3 );
   checkResult( 13, NumberOfPointsAfter( 0 ), 2 );
   checkResult( 14, NumberOfPointsAfter( 1 ), 2 );
   checkResult( 15, NumberOfPointsAfter( 5 ), 1 );
   checkResult( 16, NumberOfPointsAfter( 7 ), 1 );
   checkResult( 17, NumberOfPointsAfter( 10 ), 0 );
   checkResult( 18, NextPointAfter( 0 ), 5 );
   checkResult( 19, NextPointAfter( 5 ), 10 );
}

#include "ZoomInfo.h"
void Envelope::GetValues
   ( const Envelope &env,
     double alignedTime, double sampleDur,
     double *buffer, int bufferLen, int leftOffset,
     const ZoomInfo &zoomInfo )
{
   // Getting many envelope values, corresponding to pixel columns, which may
   // not be uniformly spaced in time when there is a fisheye.

   double prevDiscreteTime=0.0, prevSampleVal=0.0, nextSampleVal=0.0;
   for ( int xx = 0; xx < bufferLen; ++xx ) {
      auto time = zoomInfo.PositionToTime( xx, -leftOffset );
      if ( sampleDur <= 0 )
         // Sample interval not defined (as for time track)
         buffer[xx] = env.GetValue( time );
      else {
         // The level of zoom-in may resolve individual samples.
         // If so, then instead of evaluating the envelope directly,
         // we draw a piecewise curve with knees at each sample time.
         // This actually makes clearer what happens as you drag envelope
         // points and make discontinuities.
         auto leftDiscreteTime = alignedTime +
            sampleDur * floor( ( time - alignedTime ) / sampleDur );
         if ( xx == 0 || leftDiscreteTime != prevDiscreteTime ) {
            prevDiscreteTime = leftDiscreteTime;
            prevSampleVal =
               env.GetValue( prevDiscreteTime, sampleDur );
            nextSampleVal =
               env.GetValue( prevDiscreteTime + sampleDur, sampleDur );
         }
         auto ratio = ( time - leftDiscreteTime ) / sampleDur;
         if ( env.GetExponential() )
            buffer[ xx ] = exp(
               ( 1.0 - ratio ) * log( prevSampleVal )
                  + ratio * log( nextSampleVal ) );
         else
            buffer[ xx ] =
               ( 1.0 - ratio ) * prevSampleVal + ratio * nextSampleVal;
      }
   }
}
