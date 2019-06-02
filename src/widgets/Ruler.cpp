/**********************************************************************

  Audacity: A Digital Audio Editor

  Ruler.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Ruler
\brief Used to display a Ruler.

  This is a generic class which can be used to display just about
  any kind of ruler.

  At a minimum, the user must specify the dimensions of the
  ruler, its orientation (horizontal or vertical), and the
  values displayed at the two ends of the ruler (min and max).
  By default, this class will display tick marks at reasonable
  round numbers and fractions, for example, 100, 50, 10, 5, 1,
  0.5, 0.1, etc.

  The class is designed to display a small handful of
  labeled Major ticks, and a few Minor ticks between each of
  these.  Minor ticks are labeled if there is enough space.
  Labels will never run into each other.

  In addition to Real numbers, the Ruler currently supports
  two other formats for its display:

  Integer - never shows tick marks for fractions of an integer

  Time - Assumes values represent seconds, and labels the tick
         marks in "HH:MM:SS" format, e.g. 4000 seconds becomes
         "1:06:40", for example.  Will display fractions of
         a second, and tick marks are all reasonable round
         numbers for time (i.e. 15 seconds, 30 seconds, etc.)
*//***************************************************************//**

\class RulerPanel
\brief RulerPanel class allows you to work with a Ruler like
  any other wxWindow.

*//***************************************************************//**


\class Ruler::Label
\brief An array of these created by the Ruler is used to determine
what and where text annotations to the numbers on the Ruler get drawn.

\todo Check whether Ruler is costing too much time in allocation/free of
array of Ruler::Label.

*//******************************************************************/

#include "../Audacity.h"
#include "Ruler.h"

#include "../Experimental.h"

#include <wx/dcclient.h>
#include <wx/dcscreen.h>

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../NumberScale.h"
#include "../TimeTrack.h"
#include "../ViewInfo.h"

using std::min;
using std::max;

//wxColour Ruler::mTickColour{ 153, 153, 153 };

//
// Ruler
//

Ruler::Ruler()
   : mpNumberScale{}
{
   mMin = mHiddenMin = 0.0;
   mMax = mHiddenMax = 100.0;
   mOrientation = wxHORIZONTAL;
   mSpacing = 6;
   mHasSetSpacing = false;
   mFormat = RealFormat;
   mFlip = false;
   mLog = false;
   mLabelEdges = false;

   mLeft = -1;
   mTop = -1;
   mRight = -1;
   mBottom = -1;
   mbTicksOnly = true;
   mbTicksAtExtremes = false;
   mTickColour = wxColour( theTheme.Colour( clrTrackPanelText ));
   mPen.SetColour(mTickColour);
   mDbMirrorValue = 0.0;

   // Note: the font size is now adjusted automatically whenever
   // Invalidate is called on a horizontal Ruler, unless the user
   // calls SetFonts manually.  So the defaults here are not used
   // often.

   int fontSize = 10;
#ifdef __WXMSW__
   fontSize = 8;
#endif

   mMinorMinorFont = std::make_unique<wxFont>(fontSize - 1, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   mMinorFont = std::make_unique<wxFont>(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   mMajorFont = std::make_unique<wxFont>(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD);

   mUserFonts = false;

   mLengthOld = 0;
   mLength = 0;
   mUserBitLen = 0;

   mValid = false;

   mCustom = false;
   mbMinor = true;

   mGridLineLength = 0;
   mMajorGrid = false;
   mMinorGrid = false;

   mTwoTone = false;

   mUseZoomInfo = NULL;
}

Ruler::~Ruler()
{
   Invalidate();  // frees up our arrays
}

void Ruler::SetTwoTone(bool twoTone)
{
   mTwoTone = twoTone;
}

void Ruler::SetFormat(RulerFormat format)
{
   // IntFormat, RealFormat, RealLogFormat, TimeFormat, or LinearDBFormat

   if (mFormat != format) {
      mFormat = format;

      Invalidate();
   }
}

void Ruler::SetLog(bool log)
{
   // Logarithmic

   if (mLog != log) {
      mLog = log;

      Invalidate();
   }
}

void Ruler::SetUnits(const wxString &units)
{
   // Specify the name of the units (like "dB") if you
   // want numbers like "1.6" formatted as "1.6 dB".

   if (mUnits != units) {
      mUnits = units;

      Invalidate();
   }
}

void Ruler::SetOrientation(int orient)
{
   // wxHORIZONTAL || wxVERTICAL

   if (mOrientation != orient) {
      mOrientation = orient;

      if (mOrientation == wxVERTICAL && !mHasSetSpacing)
         mSpacing = 2;

      Invalidate();
   }
}

void Ruler::SetRange(double min, double max)
{
   SetRange(min, max, min, max);
}

void Ruler::SetRange
   (double min, double max, double hiddenMin, double hiddenMax)
{
   // For a horizontal ruler,
   // min is the value in the center of pixel "left",
   // max is the value in the center of pixel "right".

   // In the special case of a time ruler,
   // hiddenMin and hiddenMax are values that would be shown with the fisheye
   // turned off.  In other cases they equal min and max respectively.

   if (mMin != min || mMax != max ||
      mHiddenMin != hiddenMin || mHiddenMax != hiddenMax) {
      mMin = min;
      mMax = max;
      mHiddenMin = hiddenMin;
      mHiddenMax = hiddenMax;

      Invalidate();
   }
}

void Ruler::SetSpacing(int spacing)
{
   mHasSetSpacing = true;

   if (mSpacing != spacing) {
      mSpacing = spacing;

      Invalidate();
   }
}

void Ruler::SetLabelEdges(bool labelEdges)
{
   // If this is true, the edges of the ruler will always
   // receive a label.  If not, the nearest round number is
   // labeled (which may or may not be the edge).

   if (mLabelEdges != labelEdges) {
      mLabelEdges = labelEdges;

      Invalidate();
   }
}

void Ruler::SetFlip(bool flip)
{
   // If this is true, the orientation of the tick marks
   // is reversed from the default; eg. above the line
   // instead of below

   if (mFlip != flip) {
      mFlip = flip;

      Invalidate();
   }
}

void Ruler::SetMinor(bool value)
{
   mbMinor = value;
}

void Ruler::SetFonts(const wxFont &minorFont, const wxFont &majorFont, const wxFont &minorMinorFont)
{
   *mMinorMinorFont = minorMinorFont;
   *mMinorFont = minorFont;
   *mMajorFont = majorFont;

   // Won't override these fonts
   mUserFonts = true;

   Invalidate();
}

void Ruler::SetNumberScale(const NumberScale *pScale)
{
   if (!pScale) {
      if (mpNumberScale) {
         mpNumberScale.reset();
         Invalidate();
      }
   }
   else {
      if (!mpNumberScale || *mpNumberScale != *pScale) {
         mpNumberScale = std::make_unique<NumberScale>(*pScale);
         Invalidate();
      }
   }
}

void Ruler::OfflimitsPixels(int start, int end)
{
   if (!mUserBits) {
      if (mOrientation == wxHORIZONTAL)
         mLength = mRight-mLeft;
      else
         mLength = mBottom-mTop;
      if( mLength < 0 )
         return;
      mUserBits.reinit(static_cast<size_t>(mLength+1), true);
      mUserBitLen  = mLength+1;
   }

   if (end < start)
      std::swap( start, end );

   if (start < 0)
      start = 0;
   if (end > mLength)
      end = mLength;

   for(int i = start; i <= end; i++)
      mUserBits[i] = 1;
}

void Ruler::SetBounds(int left, int top, int right, int bottom)
{
   if (mLeft != left || mTop != top ||
       mRight != right || mBottom != bottom) {
      mLeft = left;
      mTop = top;
      mRight = right;
      mBottom = bottom;

      Invalidate();
   }
}

void Ruler::Invalidate()
{
   mValid = false;

   if (mOrientation == wxHORIZONTAL)
      mLength = mRight-mLeft;
   else
      mLength = mBottom-mTop;

   mBits.reset();
   if (mUserBits && mLength+1 != mUserBitLen) {
      mUserBits.reset();
      mUserBitLen = 0;
   }
}

void Ruler::FindLinearTickSizes(double UPP)
{
   // Given the dimensions of the ruler, the range of values it
   // has to display, and the format (i.e. Int, Real, Time),
   // figure out how many units are in one Minor tick, and
   // in one Major tick.
   //
   // The goal is to always put tick marks on nice round numbers
   // that are easy for humans to grok.  This is the most tricky
   // with time.

   double d;

   // As a heuristic, we want at least 22 pixels between each 
   // minor tick.  We want to show numbers like "-48"
   // in that space.
   // If vertical, we don't need as much space.
   double units = ((mOrientation == wxHORIZONTAL) ? 22 : 16) * fabs(UPP);

   mDigits = 0;

   switch(mFormat) {
   case LinearDBFormat:
      if (units < 0.001) {
         mMinor = 0.001;
         mMajor = 0.005;
         return;
      }
      if (units < 0.01) {
         mMinor = 0.01;
         mMajor = 0.05;
         return;
      }
      if (units < 0.1) {
         mMinor = 0.1;
         mMajor = 0.5;
         return;
      }
      if (units < 1.0) {
         mMinor = 1.0;
         mMajor = 6.0;
         return;
      }
      if (units < 3.0) {
         mMinor = 3.0;
         mMajor = 12.0;
         return;
      }
      if (units < 6.0) {
         mMinor = 6.0;
         mMajor = 24.0;
         return;
      }
      if (units < 12.0) {
         mMinor = 12.0;
         mMajor = 48.0;
         return;
      }
      if (units < 24.0) {
         mMinor = 24.0;
         mMajor = 96.0;
         return;
      }
      d = 20.0;
      for(;;) {
         if (units < d) {
            mMinor = d;
            mMajor = d*5.0;
            return;
         }
         d *= 5.0;
         if (units < d) {
            mMinor = d;
            mMajor = d*5.0;
            return;
         }
         d *= 2.0;
      }
      break;

   case IntFormat:
      d = 1.0;
      for(;;) {
         if (units < d) {
            mMinor = d;
            mMajor = d*5.0;
            return;
         }
         d *= 5.0;
         if (units < d) {
            mMinor = d;
            mMajor = d*2.0;
            return;
         }
         d *= 2.0;
      }
      break;

   case TimeFormat:
      if (units > 0.5) {
         if (units < 1.0) { // 1 sec
            mMinor = 1.0;
            mMajor = 5.0;
            return;
         }
         if (units < 5.0) { // 5 sec
            mMinor = 5.0;
            mMajor = 15.0;
            return;
         }
         if (units < 10.0) {
            mMinor = 10.0;
            mMajor = 30.0;
            return;
         }
         if (units < 15.0) {
            mMinor = 15.0;
            mMajor = 60.0;
            return;
         }
         if (units < 30.0) {
            mMinor = 30.0;
            mMajor = 60.0;
            return;
         }
         if (units < 60.0) { // 1 min
            mMinor = 60.0;
            mMajor = 300.0;
            return;
         }
         if (units < 300.0) { // 5 min
            mMinor = 300.0;
            mMajor = 900.0;
            return;
         }
         if (units < 600.0) { // 10 min
            mMinor = 600.0;
            mMajor = 1800.0;
            return;
         }
         if (units < 900.0) { // 15 min
            mMinor = 900.0;
            mMajor = 3600.0;
            return;
         }
         if (units < 1800.0) { // 30 min
            mMinor = 1800.0;
            mMajor = 3600.0;
            return;
         }
         if (units < 3600.0) { // 1 hr
            mMinor = 3600.0;
            mMajor = 6*3600.0;
            return;
         }
         if (units < 6*3600.0) { // 6 hrs
            mMinor = 6*3600.0;
            mMajor = 24*3600.0;
            return;
         }
         if (units < 24*3600.0) { // 1 day
            mMinor = 24*3600.0;
            mMajor = 7*24*3600.0;
            return;
         }

         mMinor = 24.0 * 7.0 * 3600.0; // 1 week
         mMajor = 24.0 * 7.0 * 3600.0;
      }

      // Otherwise fall through to RealFormat
      // (fractions of a second should be dealt with
      // the same way as for RealFormat)

   case RealFormat:
      d = 0.000001;
      // mDigits is number of digits after the decimal point.
      mDigits = 6;
      for(;;) {
         if (units < d) {
            mMinor = d;
            mMajor = d*5.0;
            return;
         }
         d *= 5.0;
         if (units < d) {
            mMinor = d;
            mMajor = d*2.0;
            return;
         }
         d *= 2.0;
         mDigits--;
         // More than 10 digit numbers?  Something is badly wrong.
         // Probably units is coming in with too high a value.
         wxASSERT( mDigits >= -10 );
         if( mDigits < -10 )
            break;
      }
      mMinor = d;
      mMajor = d * 2.0;
      break;

   case RealLogFormat:
      d = 0.000001;
      // mDigits is number of digits after the decimal point.
      mDigits = 6;
      for(;;) {
         if (units < d) {
            mMinor = d;
            mMajor = d*5.0;
            return;
         }
         d *= 5.0;
         if (units < d) {
            mMinor = d;
            mMajor = d*2.0;
            return;
         }
         d *= 2.0;
         mDigits--;
         // More than 10 digit numbers?  Something is badly wrong.
         // Probably units is coming in with too high a value.
         wxASSERT( mDigits >= -10 );
         if( mDigits < -10 )
            break;
      }
      mDigits++;
      mMinor = d;
      mMajor = d * 2.0;
      break;
   }
}

wxString Ruler::LabelString(double d, bool major)
{
   // Given a value, turn it into a string according
   // to the current ruler format.  The number of digits of
   // accuracy depends on the resolution of the ruler,
   // i.e. how far zoomed in or out you are.

   wxString s;

   // Replace -0 with 0
   if (d < 0.0 && (d+mMinor > 0.0) && ( mFormat != RealLogFormat ))
      d = 0.0;

   switch(mFormat) {
   case IntFormat:
      s.Printf(wxT("%d"), (int)floor(d+0.5));
      break;
   case LinearDBFormat:
      if (mMinor >= 1.0)
         s.Printf(wxT("%d"), (int)floor(d+0.5));
      else {
         int precision = -log10(mMinor);
         s.Printf(wxT("%.*f"), precision, d);
      }
      break;
   case RealFormat:
      if (mMinor >= 1.0)
         s.Printf(wxT("%d"), (int)floor(d+0.5));
      else {
         s.Printf(wxString::Format(wxT("%%.%df"), mDigits), d);
      }
      break;
   case RealLogFormat:
      if (mMinor >= 1.0)
         s.Printf(wxT("%d"), (int)floor(d+0.5));
      else {
         s.Printf(wxString::Format(wxT("%%.%df"), mDigits), d);
      }
      break;
   case TimeFormat:
      if (major) {
         if (d < 0) {
            s = wxT("-");
            d = -d;
         }

         #if ALWAYS_HH_MM_SS
         int secs = (int)(d + 0.5);
         if (mMinor >= 1.0) {
            s.Printf(wxT("%d:%02d:%02d"), secs/3600, (secs/60)%60, secs%60);
         }
         else {
            wxString t1, t2, format;
            t1.Printf(wxT("%d:%02d:"), secs/3600, (secs/60)%60);
            format.Printf(wxT("%%0%d.%dlf"), mDigits+3, mDigits);
            t2.Printf(format, fmod(d, 60.0));
            s += t1 + t2;
         }
         break;
         #endif

         if (mMinor >= 3600.0) {
            int hrs = (int)(d / 3600.0 + 0.5);
            wxString h;
            h.Printf(wxT("%d:00:00"), hrs);
            s += h;
         }
         else if (mMinor >= 60.0) {
            int minutes = (int)(d / 60.0 + 0.5);
            wxString m;
            if (minutes >= 60)
               m.Printf(wxT("%d:%02d:00"), minutes/60, minutes%60);
            else
               m.Printf(wxT("%d:00"), minutes);
            s += m;
         }
         else if (mMinor >= 1.0) {
            int secs = (int)(d + 0.5);
            wxString t;
            if (secs >= 3600)
               t.Printf(wxT("%d:%02d:%02d"), secs/3600, (secs/60)%60, secs%60);
            else if (secs >= 60)
               t.Printf(wxT("%d:%02d"), secs/60, secs%60);
            else
               t.Printf(wxT("%d"), secs);
            s += t;
         }
         else {
// Commented out old and incorrect code for avoiding the 40mins and 60 seconds problem
// It was causing Bug 463 - Incorrect Timeline numbering (where at high zoom and long tracks,
// numbers did not change.
#if 0
            // The casting to float is working around an issue where 59 seconds
            // would show up as 60 when using g++ (Ubuntu 4.3.3-5ubuntu4) 4.3.3.
            int secs = (int)(float)(d);
            wxString t1, t2, format;

            if (secs >= 3600)
               t1.Printf(wxT("%d:%02d:"), secs/3600, (secs/60)%60);
            else if (secs >= 60)
               t1.Printf(wxT("%d:"), secs/60);

            if (secs >= 60)
               format.Printf(wxT("%%0%d.%dlf"), mDigits+3, mDigits);
            else
               format.Printf(wxT("%%%d.%dlf"), mDigits+3, mDigits);
            // The casting to float is working around an issue where 59 seconds
            // would show up as 60 when using g++ (Ubuntu 4.3.3-5ubuntu4) 4.3.3.
            t2.Printf(format, fmod((float)d, (float)60.0));
#else
            // For d in the range of hours, d is just very slightly below the value it should 
            // have, because of using a double, which in turn yields values like 59:59:999999 
            // mins:secs:nanosecs when we want 1:00:00:000000
            // so adjust by less than a nano second per hour to get nicer number formatting.
            double dd = d * 1.000000000000001;
            int secs = (int)(dd);
            wxString t1, t2, format;

            if (secs >= 3600)
               t1.Printf(wxT("%d:%02d:"), secs/3600, (secs/60)%60);
            else if (secs >= 60)
               t1.Printf(wxT("%d:"), secs/60);

            if (secs >= 60)
               format.Printf(wxT("%%0%d.%dlf"), mDigits+3, mDigits);
            else
               format.Printf(wxT("%%%d.%dlf"), mDigits+3, mDigits);
            // dd will be reduced to just the seconds and fractional part.
            dd = dd - secs + (secs%60);
            // truncate to appropriate number of digits, so that the print formatting 
            // doesn't round up 59.9999999 to 60.
            double multiplier = pow( 10, mDigits);
            dd = ((int)(dd * multiplier))/multiplier;
            t2.Printf(format, dd);
#endif
            s += t1 + t2;
         }
      }
      else {
      }
   }

   if (!mUnits.empty())
      s = (s + mUnits);

   return s;
}

void Ruler::Tick(int pos, double d, bool major, bool minor)
{
   wxString l;
   wxCoord strW, strH, strD, strL;
   int strPos, strLen, strLeft, strTop;

   // FIXME: We don't draw a tick if off end of our label arrays
   // But we shouldn't have an array of labels.
   if( mNumMinorMinor >= mLength )
      return;
   if( mNumMinor >= mLength )
      return;
   if( mNumMajor >= mLength )
      return;

   Label *label;
   if (major)
      label = &mMajorLabels[mNumMajor++];
   else if (minor)
      label = &mMinorLabels[mNumMinor++];
   else
      label = &mMinorMinorLabels[mNumMinorMinor++];

   label->value = d;
   label->pos = pos;
   label->lx = mLeft - 1000; // don't display
   label->ly = mTop - 1000;  // don't display
   label->text = wxT("");

   mDC->SetFont(major? *mMajorFont: minor? *mMinorFont : *mMinorMinorFont);
   // Bug 521.  dB view for waveforms needs a 2-sided scale.
   if(( mDbMirrorValue > 1.0 ) && ( -d > mDbMirrorValue ))
      d = -2*mDbMirrorValue - d;
   l = LabelString(d, major);
   mDC->GetTextExtent(l, &strW, &strH, &strD, &strL);

   if (mOrientation == wxHORIZONTAL) {
      strLen = strW;
      strPos = pos - strW/2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strW >= mLength)
         strPos = mLength - strW;
      strLeft = mLeft + strPos;
      if (mFlip) {
         strTop = mTop + 4;
         mMaxHeight = max(mMaxHeight, strH + 4);
      }
      else {
         strTop =-strH-mLead;
         mMaxHeight = max(mMaxHeight, strH + 6);
      }
   }
   else {
      strLen = strH;
      strPos = pos - strH/2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strH >= mLength)
         strPos = mLength - strH;
      strTop = mTop + strPos;
      if (mFlip) {
         strLeft = mLeft + 5;
         mMaxWidth = max(mMaxWidth, strW + 5);
      }
      else
         strLeft =-strW-6;
   }


   // FIXME: we shouldn't even get here if strPos < 0.
   // Ruler code currently does  not handle very small or
   // negative sized windows (i.e. don't draw) properly.
   if( strPos < 0 )
      return;

   // See if any of the pixels we need to draw this
   // label is already covered

   int i;
   for(i=0; i<strLen; i++)
      if (mBits[strPos+i])
         return;

   // If not, position the label and give it text

   label->lx = strLeft;
   label->ly = strTop;
   label->text = l;

   // And mark these pixels, plus some surrounding
   // ones (the spacing between labels), as covered
   int leftMargin = mSpacing;
   if (strPos < leftMargin)
      leftMargin = strPos;
   strPos -= leftMargin;
   strLen += leftMargin;

   int rightMargin = mSpacing;
   if (strPos + strLen > mLength - mSpacing)
      rightMargin = mLength - strPos - strLen;
   strLen += rightMargin;

   for(i=0; i<strLen; i++)
      mBits[strPos+i] = 1;

   wxRect r(strLeft, strTop, strW, strH);
   mRect.Union(r);

}

void Ruler::TickCustom(int labelIdx, bool major, bool minor)
{
   //This should only used in the mCustom case
   // Many code comes from 'Tick' method: this should
   // be optimized.

   int pos;
   wxString l;
   wxCoord strW, strH, strD, strL;
   int strPos, strLen, strLeft, strTop;

   // FIXME: We don't draw a tick if of end of our label arrays
   // But we shouldn't have an array of labels.
   if( mNumMinor >= mLength )
      return;
   if( mNumMajor >= mLength )
      return;

   Label *label;
   if (major)
      label = &mMajorLabels[labelIdx];
   else if (minor)
      label = &mMinorLabels[labelIdx];
   else
      label = &mMinorMinorLabels[labelIdx];

   label->value = 0.0;
   pos = label->pos;         // already stored in label class
   l   = label->text;
   label->lx = mLeft - 1000; // don't display
   label->ly = mTop - 1000;  // don't display

   mDC->SetFont(major? *mMajorFont: minor? *mMinorFont : *mMinorMinorFont);

   mDC->GetTextExtent(l, &strW, &strH, &strD, &strL);

   if (mOrientation == wxHORIZONTAL) {
      strLen = strW;
      strPos = pos - strW/2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strW >= mLength)
         strPos = mLength - strW;
      strLeft = mLeft + strPos;
      if (mFlip) {
         strTop = mTop + 4;
         mMaxHeight = max(mMaxHeight, strH + 4);
      }
      else {

         strTop = mTop- mLead+4;// More space was needed...
         mMaxHeight = max(mMaxHeight, strH + 6);
      }
   }
   else {
      strLen = strH;
      strPos = pos - strH/2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strH >= mLength)
         strPos = mLength - strH;
      strTop = mTop + strPos;
      if (mFlip) {
         strLeft = mLeft + 5;
         mMaxWidth = max(mMaxWidth, strW + 5);
      }
      else {

         strLeft =-strW-6;
       }
   }


   // FIXME: we shouldn't even get here if strPos < 0.
   // Ruler code currently does  not handle very small or
   // negative sized windows (i.e. don't draw) properly.
   if( strPos < 0 )
      return;

   // See if any of the pixels we need to draw this
   // label is already covered

   int i;
   for(i=0; i<strLen; i++)
      if (mBits[strPos+i])
         return;

   // If not, position the label

   label->lx = strLeft;
   label->ly = strTop;

   // And mark these pixels, plus some surrounding
   // ones (the spacing between labels), as covered
   int leftMargin = mSpacing;
   if (strPos < leftMargin)
      leftMargin = strPos;
   strPos -= leftMargin;
   strLen += leftMargin;

   int rightMargin = mSpacing;
   if (strPos + strLen > mLength - mSpacing)
      rightMargin = mLength - strPos - strLen;
   strLen += rightMargin;

   for(i=0; i<strLen; i++)
      mBits[strPos+i] = 1;


   wxRect r(strLeft, strTop, strW, strH);
   mRect.Union(r);

}

void Ruler::Update()
{
  Update(NULL);
}

void Ruler::Update(const TimeTrack* timetrack)// Envelope *speedEnv, long minSpeed, long maxSpeed )
{
   const ZoomInfo *zoomInfo = NULL;
   if (!mLog && mOrientation == wxHORIZONTAL)
      zoomInfo = mUseZoomInfo;

   // This gets called when something has been changed
   // (i.e. we've been invalidated).  Recompute all
   // tick positions and font size.

   int i;
   int j;

   if (!mUserFonts) {
      int fontSize = 4;
      wxCoord strW, strH, strD, strL;
      wxString exampleText = wxT("0.9");   //ignored for height calcs on all platforms
      int desiredPixelHeight;


      static const int MinPixelHeight = 10; // 8;
      static const int MaxPixelHeight =
#ifdef __WXMAC__
            10
#else
            12
#endif
      ;

      if (mOrientation == wxHORIZONTAL)
         desiredPixelHeight = mBottom - mTop - 5; // height less ticks and 1px gap
      else
         desiredPixelHeight = MaxPixelHeight;

      desiredPixelHeight =
         std::max(MinPixelHeight, std::min(MaxPixelHeight,
            desiredPixelHeight));

      // Keep making the font bigger until it's too big, then subtract one.
      mDC->SetFont(wxFont(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD));
      mDC->GetTextExtent(exampleText, &strW, &strH, &strD, &strL);
      while ((strH - strD - strL) <= desiredPixelHeight && fontSize < 40) {
         fontSize++;
         mDC->SetFont(wxFont(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD));
         mDC->GetTextExtent(exampleText, &strW, &strH, &strD, &strL);
      }
      fontSize--;
      mDC->SetFont(wxFont(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
      mDC->GetTextExtent(exampleText, &strW, &strH, &strD, &strL);
      mLead = strL;

      mMajorFont = std::make_unique<wxFont>(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD);

      mMinorFont = std::make_unique<wxFont>(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);

      mMinorMinorFont = std::make_unique<wxFont>(fontSize - 1, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   }

   // If ruler is being resized, we could end up with it being too small.
   // Values of mLength of zero or below cause bad array allocations and
   // division by zero.  So...
   // IF too small THEN bail out and don't draw.
   if( mLength <= 0 )
      return;

   if (mOrientation == wxHORIZONTAL) {
      mMaxWidth = mLength;
      mMaxHeight = 0;
      mRect = wxRect(0,0, mLength,0);
   }
   else {
      mMaxWidth = 0;
      mMaxHeight = mLength;
      mRect = wxRect(0,0, 0,mLength);
   }

   // FIXME: Surely we do not need to allocate storage for the labels?
   // We can just recompute them as we need them?  Yes, but only if
   // mCustom is false!!!!

   auto size = static_cast<size_t>(mLength + 1);
   if(!mCustom) {
      mNumMajor = 0;
      mNumMinor = 0;
      mNumMinorMinor = 0;
      if (mLength!=mLengthOld) {
         mMajorLabels.reinit(size);
         mMinorLabels.reinit(size);
         mMinorMinorLabels.reinit(size);
         mLengthOld = mLength;
      }
   }

   mBits.reinit(size);
   if (mUserBits)
      for(i=0; i<=mLength; i++)
         mBits[i] = mUserBits[i];
   else
      for(i=0; i<=mLength; i++)
         mBits[i] = 0;

   // *************** Label calculation routine **************
   if(mCustom == true) {

      // SET PARAMETER IN MCUSTOM CASE
      // Works only with major labels

      int numLabel = mNumMajor;

      i = 0;
      while((i<numLabel) && (i<=mLength)) {

         TickCustom(i, true, false);
         i++;
      }

   } else if(mLog==false) {

      // Use the "hidden" min and max to determine the tick size.
      // That may make a difference with fisheye.
      // Otherwise you may see the tick size for the whole ruler change
      // when the fisheye approaches start or end.
      double UPP = (mHiddenMax-mHiddenMin)/mLength;  // Units per pixel
      FindLinearTickSizes(UPP);

      // Left and Right Edges
      if (mLabelEdges) {
         Tick(0, mMin, true, false);
         Tick(mLength, mMax, true, false);
      }

      // Zero (if it's in the middle somewhere)
      if (mMin * mMax < 0.0) {
         int mid;
         if (zoomInfo != NULL)
            mid = (int)(zoomInfo->TimeToPosition(0.0, mLeftOffset));
         else
            mid = (int)(mLength*(mMin / (mMin - mMax)) + 0.5);
         const int iMaxPos = (mOrientation == wxHORIZONTAL) ? mRight : mBottom - 5;
         if (mid >= 0 && mid < iMaxPos)
            Tick(mid, 0.0, true, false);
      }

      double sg = UPP > 0.0? 1.0: -1.0;

      int nDroppedMinorLabels=0;
      // Major and minor ticks
      for (int jj = 0; jj < 2; ++jj) {
         const double denom = jj == 0 ? mMajor : mMinor;
         i = -1; j = 0;
         double d, warpedD, nextD;

         double prevTime = 0.0, time = 0.0;
         if (zoomInfo != NULL) {
            j = zoomInfo->TimeToPosition(mMin);
            prevTime = zoomInfo->PositionToTime(--j);
            time = zoomInfo->PositionToTime(++j);
            d = (prevTime + time) / 2.0;
         }
         else
            d = mMin - UPP / 2;
         if (timetrack)
            warpedD = timetrack->ComputeWarpedLength(0.0, d);
         else
            warpedD = d;
         // using ints doesn't work, as
         // this will overflow and be negative at high zoom.
         double step = floor(sg * warpedD / denom);
         while (i <= mLength) {
            i++;
            if (zoomInfo)
            {
               prevTime = time;
               time = zoomInfo->PositionToTime(++j);
               nextD = (prevTime + time) / 2.0;
               // wxASSERT(time >= prevTime);
            }
            else
               nextD = d + UPP;
            if (timetrack)
               warpedD += timetrack->ComputeWarpedLength(d, nextD);
            else
               warpedD = nextD;
            d = nextD;

            if (floor(sg * warpedD / denom) > step) {
               step = floor(sg * warpedD / denom);
               bool major = jj == 0;
               Tick(i, sg * step * denom, major, !major);
               if( !major && mMinorLabels[mNumMinor-1].text.empty() ){
                  nDroppedMinorLabels++;
               }
            }
         }
      }

      // If we've dropped minor labels through overcrowding, then don't show
      // any of them.  We're allowed though to drop ones which correspond to the
      // major numbers.
      if( nDroppedMinorLabels > (mNumMajor+ (mLabelEdges ? 2:0)) ){
         // Old code dropped the labels AND their ticks, like so:
         //    mNumMinor = 0;
         // Nowadays we just drop the labels.
         for(i=0; i<mNumMinor; i++)
            mMinorLabels[i].text = "";
      }

      // Left and Right Edges
      if (mLabelEdges) {
         Tick(0, mMin, true, false);
         Tick(mLength, mMax, true, false);
      }
   }
   else {
      // log case

      NumberScale numberScale(mpNumberScale
         ? *mpNumberScale
         : NumberScale(nstLogarithmic, mMin, mMax)
      );

      mDigits=2; //TODO: implement dynamic digit computation
      double loLog = log10(mMin);
      double hiLog = log10(mMax);
      int loDecade = (int) floor(loLog);

      double val;
      double startDecade = pow(10., (double)loDecade);

      // Major ticks are the decades
      double decade = startDecade;
      double delta=hiLog-loLog, steps=fabs(delta);
      double step = delta>=0 ? 10 : 0.1;
      double rMin=std::min(mMin, mMax), rMax=std::max(mMin, mMax);
      for(i=0; i<=steps; i++)
      {  // if(i!=0)
         {  val = decade;
            if(val >= rMin && val < rMax) {
               const int pos(0.5 + mLength * numberScale.ValueToPosition(val));
               Tick(pos, val, true, false);
            }
         }
         decade *= step;
      }

      // Minor ticks are multiples of decades
      decade = startDecade;
      float start, end, mstep;
      if (delta > 0)
      {  start=2; end=10; mstep=1;
      }else
      {  start=9; end=1; mstep=-1;
      }
      steps++;
      for(i=0; i<=steps; i++) {
         for(j=start; j!=end; j+=mstep) {
            val = decade * j;
            if(val >= rMin && val < rMax) {
               const int pos(0.5 + mLength * numberScale.ValueToPosition(val));
               Tick(pos, val, false, true);
            }
         }
         decade *= step;
      }

      // MinorMinor ticks are multiples of decades
      decade = startDecade;
      if (delta > 0)
      {  start= 10; end=100; mstep= 1;
      }else
      {  start=100; end= 10; mstep=-1;
      }
      steps++;
      for (i = 0; i <= steps; i++) {
         // PRL:  Bug1038.  Don't label 1.6, rounded, as a duplicate tick for "2"
         if (!(mFormat == IntFormat && decade < 10.0)) {
            for (int f = start; f != (int)(end); f += mstep) {
               if ((int)(f / 10) != f / 10.0f) {
                  val = decade * f / 10;
                  if (val >= rMin && val < rMax) {
                     const int pos(0.5 + mLength * numberScale.ValueToPosition(val));
                     Tick(pos, val, false, false);
                  }
               }
            }
         }
         decade *= step;
      }
   }

   int displacementx=0, displacementy=0;
   if (!mFlip) {
      if (mOrientation==wxHORIZONTAL) {
         int d=mTop+mRect.GetHeight()+5;
         mRect.Offset(0,d);
         mRect.Inflate(0,5);
         displacementx=0;
         displacementy=d;
      }
      else {
         int d=mLeft-mRect.GetLeft()+5;
         mRect.Offset(d,0);
         mRect.Inflate(5,0);
         displacementx=d;
         displacementy=0;
      }
   }
   else {
      if (mOrientation==wxHORIZONTAL) {
         mRect.Inflate(0,5);
         displacementx=0;
         displacementy=0;
      }
   }
   for(i=0; i<mNumMajor; i++) {
      mMajorLabels[i].lx+= displacementx;
      mMajorLabels[i].ly+= displacementy;
   }
   for(i=0; i<mNumMinor; i++) {
      mMinorLabels[i].lx+= displacementx;
      mMinorLabels[i].ly+= displacementy;
   }
   for(i=0; i<mNumMinorMinor; i++) {
      mMinorMinorLabels[i].lx+= displacementx;
      mMinorMinorLabels[i].ly+= displacementy;
   }
   mMaxWidth = mRect.GetWidth ();
   mMaxHeight= mRect.GetHeight();
   mValid = true;
}

void Ruler::Draw(wxDC& dc)
{
   Draw( dc, NULL);
}

void Ruler::Draw(wxDC& dc, const TimeTrack* timetrack)
{
   mDC = &dc;
   if( mLength <=0 )
      return;

   if (!mValid)
      Update(timetrack);

   mDC->SetTextForeground( mTickColour );
#ifdef EXPERIMENTAL_THEMING
   mDC->SetPen(mPen);
#else
   mDC->SetPen(*wxBLACK_PEN);
#endif

   // Draws a long line the length of the ruler.
   if( !mbTicksOnly )
   {
      if (mOrientation == wxHORIZONTAL) {
         if (mFlip)
            AColor::Line(*mDC, mLeft, mTop, mRight, mTop);
         else
            AColor::Line(*mDC, mLeft, mBottom, mRight, mBottom);
      }
      else {
         if (mFlip)
            AColor::Line(*mDC, mLeft, mTop, mLeft, mBottom);
         else
         {
            // These calculations appear to be wrong, and to never have been used (so not tested) prior to MixerBoard.
            //    AColor::Line(*mDC, mRect.x-mRect.width, mTop, mRect.x-mRect.width, mBottom);
            const int nLineX = mRight - 1;
            AColor::Line(*mDC, nLineX, mTop, nLineX, mBottom);
         }
      }
   }

   int i;

   mDC->SetFont(*mMajorFont);

   // We may want to not show the ticks at the extremes,
   // though still showing the labels.
   // This gives a better look when the ruler is on a bevelled
   // button, since otherwise the tick is drawn on the bevel.
   int iMaxPos = (mOrientation==wxHORIZONTAL)? mRight : mBottom-5;

   for(i=0; i<mNumMajor; i++) {
      int pos = mMajorLabels[i].pos;

      if( mbTicksAtExtremes || ((pos!=0)&&(pos!=iMaxPos)))
      {
         if (mOrientation == wxHORIZONTAL) {
            if (mFlip)
               AColor::Line(*mDC, mLeft + pos, mTop,
                             mLeft + pos, mTop + 4);
            else
               AColor::Line(*mDC, mLeft + pos, mBottom - 4,
                             mLeft + pos, mBottom);
         }
         else {
            if (mFlip)
               AColor::Line(*mDC, mLeft, mTop + pos,
                             mLeft + 4, mTop + pos);
            else
               AColor::Line(*mDC, mRight - 4, mTop + pos,
                             mRight, mTop + pos);
         }
      }

      mMajorLabels[i].Draw(*mDC, mTwoTone, mTickColour);
   }

   if(mbMinor == true) {
      mDC->SetFont(*mMinorFont);
      for(i=0; i<mNumMinor; i++) {
         int pos = mMinorLabels[i].pos;
         if( mbTicksAtExtremes || ((pos!=0)&&(pos!=iMaxPos)))
         {
            if (mOrientation == wxHORIZONTAL)
            {
               if (mFlip)
                  AColor::Line(*mDC, mLeft + pos, mTop,
                                mLeft + pos, mTop + 2);
               else
                  AColor::Line(*mDC, mLeft + pos, mBottom - 2,
                                mLeft + pos, mBottom);
            }
            else
            {
               if (mFlip)
                  AColor::Line(*mDC, mLeft, mTop + pos,
                                mLeft + 2, mTop + pos);
               else
                  AColor::Line(*mDC, mRight - 2, mTop + pos,
                                mRight, mTop + pos);
            }
         }
         mMinorLabels[i].Draw(*mDC, mTwoTone, mTickColour);
      }
   }

   mDC->SetFont(*mMinorMinorFont);

   for(i=0; i<mNumMinorMinor; i++) {
      if (!mMinorMinorLabels[i].text.empty())
      {
         int pos = mMinorMinorLabels[i].pos;

         if( mbTicksAtExtremes || ((pos!=0)&&(pos!=iMaxPos)))
         {
            if (mOrientation == wxHORIZONTAL)
            {
               if (mFlip)
                  AColor::Line(*mDC, mLeft + pos, mTop,
                                mLeft + pos, mTop + 2);
               else
                  AColor::Line(*mDC, mLeft + pos, mBottom - 2,
                                mLeft + pos, mBottom);
            }
            else
            {
               if (mFlip)
                  AColor::Line(*mDC, mLeft, mTop + pos,
                                mLeft + 2, mTop + pos);
               else
                  AColor::Line(*mDC, mRight - 2, mTop + pos,
                                mRight, mTop + pos);
            }
         }
         mMinorMinorLabels[i].Draw(*mDC, mTwoTone, mTickColour);
      }
   }
}

// ********** Draw grid ***************************
void Ruler::DrawGrid(wxDC& dc, int length, bool minor, bool major, int xOffset, int yOffset)
{
   mGridLineLength = length;
   mMajorGrid = major;
   mMinorGrid = minor;
   mDC = &dc;

   Update();

   int gridPos;
   wxPen gridPen;

   if(mbMinor && (mMinorGrid && (mGridLineLength != 0 ))) {
      gridPen.SetColour(178, 178, 178); // very light grey
      mDC->SetPen(gridPen);
      for(int i=0; i<mNumMinor; i++) {
         gridPos = mMinorLabels[i].pos;
         if(mOrientation == wxHORIZONTAL) {
            if((gridPos != 0) && (gridPos != mGridLineLength))
               AColor::Line(*mDC, gridPos+xOffset, yOffset, gridPos+xOffset, mGridLineLength-1+yOffset);
         }
         else {
            if((gridPos != 0) && (gridPos != mGridLineLength))
               AColor::Line(*mDC, xOffset, gridPos+yOffset, mGridLineLength-1+xOffset, gridPos+yOffset);
         }
      }
   }

   if(mMajorGrid && (mGridLineLength != 0 )) {
      gridPen.SetColour(127, 127, 127); // light grey
      mDC->SetPen(gridPen);
      for(int i=0; i<mNumMajor; i++) {
         gridPos = mMajorLabels[i].pos;
         if(mOrientation == wxHORIZONTAL) {
            if((gridPos != 0) && (gridPos != mGridLineLength))
               AColor::Line(*mDC, gridPos+xOffset, yOffset, gridPos+xOffset, mGridLineLength-1+yOffset);
         }
         else {
            if((gridPos != 0) && (gridPos != mGridLineLength))
               AColor::Line(*mDC, xOffset, gridPos+yOffset, mGridLineLength-1+xOffset, gridPos+yOffset);
         }
      }

      int zeroPosition = GetZeroPosition();
      if(zeroPosition > 0) {
         // Draw 'zero' grid line in black
         mDC->SetPen(*wxBLACK_PEN);
         if(mOrientation == wxHORIZONTAL) {
            if(zeroPosition != mGridLineLength)
               AColor::Line(*mDC, zeroPosition+xOffset, yOffset, zeroPosition+xOffset, mGridLineLength-1+yOffset);
         }
         else {
            if(zeroPosition != mGridLineLength)
               AColor::Line(*mDC, xOffset, zeroPosition+yOffset, mGridLineLength-1+xOffset, zeroPosition+yOffset);
         }
      }
   }
}

int Ruler::FindZero(Label * label, const int len)
{
   int i = 0;
   double d = 1.0;   // arbitrary

   do {
      d = label[i].value;
      i++;
   } while( (i < len) && (d != 0.0) );

   if(d == 0.0)
      return (label[i - 1].pos) ;
   else
      return -1;
}

int Ruler::GetZeroPosition()
{
   int zero;
   if((zero = FindZero(mMajorLabels.get(), mNumMajor)) < 0)
      zero = FindZero(mMinorLabels.get(), mNumMinor);
   // PRL: don't consult minor minor??
   return zero;
}

void Ruler::GetMaxSize(wxCoord *width, wxCoord *height)
{
   if (!mValid) {
      wxScreenDC sdc;
      mDC = &sdc;
      Update(NULL);
   }

   if (width)
      *width = mRect.GetWidth(); //mMaxWidth;

   if (height)
      *height = mRect.GetHeight(); //mMaxHeight;
}


void Ruler::SetCustomMode(bool value) { mCustom = value; }

void Ruler::SetCustomMajorLabels(wxArrayString *label, size_t numLabel, int start, int step)
{
   mNumMajor = numLabel;
   mMajorLabels.reinit(numLabel);

   for(size_t i = 0; i<numLabel; i++) {
      mMajorLabels[i].text = (*label)[i];
      mMajorLabels[i].pos  = start + i*step;
   }
   //Remember: DELETE majorlabels....
}

void Ruler::SetCustomMinorLabels(wxArrayString *label, size_t numLabel, int start, int step)
{
   mNumMinor = numLabel;
   mMinorLabels.reinit(numLabel);

   for(size_t i = 0; i<numLabel; i++) {
      mMinorLabels[i].text = (*label)[i];
      mMinorLabels[i].pos  = start + i*step;
   }
   //Remember: DELETE majorlabels....
}

void Ruler::Label::Draw(wxDC&dc, bool twoTone, wxColour c) const
{
   if (!text.empty()) {
      bool altColor = twoTone && value < 0.0;

#ifdef EXPERIMENTAL_THEMING
      dc.SetTextForeground(altColor ? theTheme.Colour( clrTextNegativeNumbers) : c);
#else
      dc.SetTextForeground(altColor ? *wxBLUE : *wxBLACK);
#endif

      dc.DrawText(text, lx, ly);
   }
}

void Ruler::SetUseZoomInfo(int leftOffset, const ZoomInfo *zoomInfo)
{
   mLeftOffset = leftOffset;
   mUseZoomInfo = zoomInfo;
}

//
// RulerPanel
//

BEGIN_EVENT_TABLE(RulerPanel, wxPanelWrapper)
   EVT_ERASE_BACKGROUND(RulerPanel::OnErase)
   EVT_PAINT(RulerPanel::OnPaint)
   EVT_SIZE(RulerPanel::OnSize)
END_EVENT_TABLE()

IMPLEMENT_CLASS(RulerPanel, wxPanelWrapper)

RulerPanel::RulerPanel(wxWindow* parent, wxWindowID id,
                       wxOrientation orientation,
                       const wxSize &bounds,
                       const Range &range,
                       Ruler::RulerFormat format,
                       const wxString &units,
                       const Options &options,
                       const wxPoint& pos /*= wxDefaultPosition*/,
                       const wxSize& size /*= wxDefaultSize*/):
   wxPanelWrapper(parent, id, pos, size)
{
   ruler.SetBounds( 0, 0, bounds.x, bounds.y );
   ruler.SetOrientation(orientation);
   ruler.SetRange( range.first, range.second );
   ruler.SetLog( options.log );
   ruler.SetFormat(format);
   ruler.SetUnits( units );
   ruler.SetFlip( options.flip );
   ruler.SetLabelEdges( options.labelEdges );
   ruler.mbTicksAtExtremes = options.ticksAtExtremes;
   if (orientation == wxVERTICAL) {
      wxCoord w;
      ruler.GetMaxSize(&w, NULL);
      SetMinSize(wxSize(w, 150));  // height needed for wxGTK
   }
   else if (orientation == wxHORIZONTAL) {
      wxCoord h;
      ruler.GetMaxSize(NULL, &h);
      SetMinSize(wxSize(wxDefaultCoord, h));
   }
   if (options.hasTickColour)
      ruler.SetTickColour( options.tickColour );
}

RulerPanel::~RulerPanel()
{
}

void RulerPanel::OnErase(wxEraseEvent & WXUNUSED(evt))
{
   // Ignore it to prevent flashing
}

void RulerPanel::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   wxPaintDC dc(this);

#if defined(__WXMSW__)
   dc.Clear();
#endif

   ruler.Draw(dc);
}

void RulerPanel::OnSize(wxSizeEvent & WXUNUSED(evt))
{
   Refresh();
}

// LL:  We're overloading DoSetSize so that we can update the ruler bounds immediately
//      instead of waiting for a wxEVT_SIZE to come through.  This is needed by (at least)
//      FreqWindow since it needs to have an updated ruler before RulerPanel gets the
//      size event.
void RulerPanel::DoSetSize(int x, int y,
                           int width, int height,
                           int sizeFlags)
{
   wxPanelWrapper::DoSetSize(x, y, width, height, sizeFlags);

   int w, h;
   GetClientSize(&w, &h);

   ruler.SetBounds(0, 0, w-1, h-1);
}
