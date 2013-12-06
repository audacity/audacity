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

\todo Check whether Ruler is costing too much time in malloc/free of
array of Ruler::Label.

*//******************************************************************/

#include "../Audacity.h"

#include <math.h>

#include <wx/dcscreen.h>
#include <wx/dcmemory.h>
#include <wx/dcbuffer.h>
#include <wx/settings.h>

#include "../Internat.h"
#include "../Project.h"
#include "Ruler.h"
#include "../toolbars/ControlToolBar.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../Experimental.h"
#include "../TimeTrack.h"

#define max(a,b)  ( (a<b)?b:a )

#define SELECT_TOLERANCE_PIXEL 10

#define PLAY_REGION_TRIANGLE_SIZE 6
#define PLAY_REGION_RECT_WIDTH 1
#define PLAY_REGION_RECT_HEIGHT 3
#define PLAY_REGION_GLOBAL_OFFSET_Y 7

//
// Ruler
//

Ruler::Ruler()
{
   mMin = 0.0;
   mMax = 100.0;
   mOrientation = wxHORIZONTAL;
   mSpacing = 6;
   mHasSetSpacing = false;
   mFormat = RealFormat;
   mFlip = false;
   mLog = false;
   mLabelEdges = false;
   mUnits = wxT("");

   mLeft = -1;
   mTop = -1;
   mRight = -1;
   mBottom = -1;
   mbTicksOnly = true;
   mbTicksAtExtremes = false;
   mTickColour = wxColour(153,153,153);
   mPen.SetColour(mTickColour);

   // Note: the font size is now adjusted automatically whenever
   // Invalidate is called on a horizontal Ruler, unless the user
   // calls SetFonts manually.  So the defaults here are not used
   // often.

   int fontSize = 10;
#ifdef __WXMSW__
   fontSize = 8;
#endif

   mMinorMinorFont = new wxFont(fontSize-1, wxSWISS, wxNORMAL, wxNORMAL);
   mMinorFont = new wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
   mMajorFont = new wxFont(fontSize, wxSWISS, wxNORMAL, wxBOLD);
   mUserFonts = false;

   #ifdef __WXMAC__
   mMinorMinorFont->SetNoAntiAliasing(true);
   mMinorFont->SetNoAntiAliasing(true);
   mMajorFont->SetNoAntiAliasing(true);
   #endif

   mMajorLabels = 0;
   mMinorLabels = 0;
   mMinorMinorLabels = 0;
   mLengthOld = 0;
   mLength = 0;
   mBits = NULL;
   mUserBits = NULL;
   mUserBitLen = 0;
   
   mValid = false;

   mCustom = false;
   mbMinor = true;

   mGridLineLength = 0;
   mMajorGrid = false;
   mMinorGrid = false;
}

Ruler::~Ruler()
{
   Invalidate();  // frees up our arrays
   if( mUserBits )
      delete [] mUserBits;//JKC
   delete mMinorFont;
   delete mMajorFont;
   delete mMinorMinorFont;

   if (mMajorLabels)
      delete[] mMajorLabels;
   if (mMinorLabels)
      delete[] mMinorLabels;
   if (mMinorMinorLabels)
      delete[] mMinorMinorLabels;
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

void Ruler::SetUnits(wxString units)
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
   // For a horizontal ruler,
   // min is the value in the center of pixel "left",
   // max is the value in the center of pixel "right".

   if (mMin != min || mMax != max) {
      mMin = min;
      mMax = max;
      
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

   #ifdef __WXMAC__
   mMinorMinorFont->SetNoAntiAliasing(true);
   mMinorFont->SetNoAntiAliasing(true);
   mMajorFont->SetNoAntiAliasing(true);
   #endif
   
   // Won't override these fonts
   mUserFonts = true;

   Invalidate();
}

void Ruler::OfflimitsPixels(int start, int end)
{
   int i;

   if (!mUserBits) {
      if (mOrientation == wxHORIZONTAL)
         mLength = mRight-mLeft;
      else
         mLength = mBottom-mTop;      
      mUserBits = new int[mLength+1];
      for(i=0; i<=mLength; i++)
         mUserBits[i] = 0;
      mUserBitLen  = mLength+1;
   }

   if (end < start) {
      i = end;
      end = start;
      start = i;
   }

   if (start < 0)
      start = 0;
   if (end > mLength)
      end = mLength;

   for(i=start; i<=end; i++)
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

   if (mBits) {
      delete [] mBits;
      mBits = NULL;
   }
   if (mUserBits && mLength+1 != mUserBitLen) {
      delete[] mUserBits;
      mUserBits = NULL;
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

   // As a heuristic, we want at least 16 pixels
   // between each minor tick
   double units = 16 * fabs(UPP);

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
   if (d < 0.0 && d+mMinor > 0.0)
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
            t2.Printf(format.c_str(), fmod(d, 60.0));
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
            t2.Printf(format.c_str(), fmod((float)d, (float)60.0));

            s += t1 + t2;
         }
      }
      else {
      }
   }
   
   if (mUnits != wxT(""))
      s = (s + mUnits);

   return s;
}

void Ruler::Tick(int pos, double d, bool major, bool minor)
{
   wxString l;
   wxCoord strW, strH, strD, strL;
   int strPos, strLen, strLeft, strTop;

   // FIXME: We don't draw a tick if of end of our label arrays
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

   label->pos = pos;
   label->lx = mLeft - 1000; // don't display
   label->ly = mTop - 1000;  // don't display
   label->text = wxT("");

   mDC->SetFont(major? *mMajorFont: minor? *mMinorFont : *mMinorMinorFont);
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

         strTop =-strH-mLead;
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

void Ruler::Update(TimeTrack* timetrack)// Envelope *speedEnv, long minSpeed, long maxSpeed )
{
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

     if (mOrientation == wxHORIZONTAL)
       desiredPixelHeight = mBottom-mTop-5; // height less ticks and 1px gap
     else
         desiredPixelHeight = 12;   // why 12?  10 -> 12 seems to be max/min

     if (desiredPixelHeight < 10)//8)
       desiredPixelHeight = 10;//8;
     if (desiredPixelHeight > 12)
       desiredPixelHeight = 12;

      // Keep making the font bigger until it's too big, then subtract one.
      mDC->SetFont(wxFont(fontSize, wxSWISS, wxNORMAL, wxBOLD));
      mDC->GetTextExtent(exampleText, &strW, &strH, &strD, &strL);
      while( (strH-strD-strL) <= desiredPixelHeight && fontSize < 40) {
         fontSize++;
         mDC->SetFont(wxFont(fontSize, wxSWISS, wxNORMAL, wxBOLD));
         mDC->GetTextExtent(exampleText, &strW, &strH, &strD, & strL);
      }
      fontSize--;
      mDC->SetFont(wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL));
      mDC->GetTextExtent(exampleText, &strW, &strH, &strD, &strL);
      mLead = strL;

     if (mMajorFont)
        delete mMajorFont;
     mMajorFont = new wxFont(fontSize, wxSWISS, wxNORMAL, wxBOLD);

     if (mMinorFont)
        delete mMinorFont;
     mMinorFont = new wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);

     if (mMinorMinorFont)
        delete mMinorMinorFont;
     mMinorMinorFont = new wxFont(fontSize-1, wxSWISS, wxNORMAL, wxNORMAL);
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

   if(!mCustom) {
      mNumMajor = 0;
      mNumMinor = 0;
      mNumMinorMinor = 0;
      if (mLength!=mLengthOld) {
         if (mMajorLabels)
            delete[] mMajorLabels;
         mMajorLabels = new Label[mLength+1];
         if (mMinorLabels)
            delete[] mMinorLabels;
         mMinorLabels = new Label[mLength+1];
         if (mMinorMinorLabels)
            delete[] mMinorMinorLabels;
         mMinorMinorLabels = new Label[mLength+1];
         mLengthOld = mLength;
      }
   }

   if (mBits)
      delete[] mBits;
   mBits = new int[mLength+1];
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

      double UPP = (mMax-mMin)/mLength;  // Units per pixel
      FindLinearTickSizes(UPP);
      
      // Left and Right Edges
      if (mLabelEdges) {
         Tick(0, mMin, true, false);
         Tick(mLength, mMax, true, false);
      }
      
      // Zero (if it's in the middle somewhere)
      if (mMin * mMax < 0.0) {
         int mid = (int)(mLength*(mMin/(mMin-mMax)) + 0.5);
         Tick(mid, 0.0, true, false);
      }
      
      double sg = UPP > 0.0? 1.0: -1.0;
      
      // Major ticks
      double d, warpedD;
      d = mMin - UPP/2;
      if(timetrack)
         warpedD = timetrack->ComputeWarpedLength(0.0, d);
      else
         warpedD = d;
      // using ints for majorint doesn't work, as 
      // majorint will overflow and be negative at high zoom.
      double majorInt = floor(sg * warpedD / mMajor);
      i = -1;
      while(i <= mLength) {
         i++;
         if(timetrack)
            warpedD += timetrack->ComputeWarpedLength(d, d + UPP);
         else
            warpedD += UPP;
         d += UPP;
         
         if (floor(sg * warpedD / mMajor) > majorInt) {
            majorInt = floor(sg * warpedD / mMajor);
            Tick(i, sg * majorInt * mMajor, true, false);
         }
      }
      
      // Minor ticks
      d = mMin - UPP/2;
      if(timetrack)
         warpedD = timetrack->ComputeWarpedLength(0.0, d);
      else
         warpedD = d;
      // using ints for majorint doesn't work, as 
      // majorint will overflow and be negative at high zoom.
      // MB: I assume the same applies to minorInt
      double minorInt = floor(sg * warpedD / mMinor);
      i = -1;
      while(i <= mLength) {
         i++;
         if(timetrack)
            warpedD += timetrack->ComputeWarpedLength(d, d + UPP);
         else
            warpedD += UPP;
         d += UPP;
         
         if (floor(sg * warpedD / mMinor) > minorInt) {
            minorInt = floor(sg * warpedD / mMinor);
            Tick(i, sg * minorInt * mMinor, false, true);
         }
      }
      
      // Left and Right Edges
      if (mLabelEdges) {
         Tick(0, mMin, true, false);
         Tick(mLength, mMax, true, false);
      }
      
   }
   else {
      // log case
      mDigits=2;	//TODO: implement dynamic digit computation
      double loLog = log10(mMin);
      double hiLog = log10(mMax);
      double scale = mLength/(hiLog - loLog);
      int loDecade = (int) floor(loLog);
      
      int pos;
      double val;
      double startDecade = pow(10., (double)loDecade);
      
      // Major ticks are the decades
      double decade = startDecade;
      double delta=hiLog-loLog, steps=fabs(delta);
      double step = delta>=0 ? 10 : 0.1;
      double rMin=wxMin(mMin, mMax), rMax=wxMax(mMin, mMax);
      for(i=0; i<=steps; i++) 
      {  // if(i!=0) 
         {  val = decade;
            if(val > rMin && val < rMax) {
               pos = (int)(((log10(val) - loLog)*scale)+0.5);
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
               pos = (int)(((log10(val) - loLog)*scale)+0.5);
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
      for(i=0; i<=steps; i++) {
         for(int f=start; f!=int(end); f+=mstep) {
            if (int(f/10)!=f/10.0f) {
               val = decade * f/10;
               if(val >= rMin && val < rMax) {
                  pos = (int)(((log10(val) - loLog)*scale)+0.5);
               Tick(pos, val, false, false);
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

void Ruler::Draw(wxDC& dc, TimeTrack* timetrack)
{
   mDC = &dc;
   if( mLength <=0 )
      return;

   if (!mValid)
      Update(timetrack);

#ifdef EXPERIMENTAL_THEMING
   mDC->SetPen(mPen);
   mDC->SetTextForeground(mTickColour);
#else
   mDC->SetPen(*wxBLACK_PEN);
   mDC->SetTextForeground(*wxBLACK);
#endif

   // Draws a long line the length of the ruler.
   if( !mbTicksOnly )
   {
      if (mOrientation == wxHORIZONTAL) {
         if (mFlip)
            mDC->DrawLine(mLeft, mTop, mRight+1, mTop);
         else
            mDC->DrawLine(mLeft, mBottom, mRight+1, mBottom);
      }
      else {
         if (mFlip)
            mDC->DrawLine(mLeft, mTop, mLeft, mBottom+1);
         else
         {
            // These calculations tappear to be wrong, and to never have been used (so not tested) prior to MixerBoard.
            //    mDC->DrawLine(mRect.x-mRect.width, mTop, mRect.x-mRect.width, mBottom+1);
            const int nLineX = mRight - 1;
            mDC->DrawLine(nLineX, mTop, nLineX, mBottom+1);
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
               mDC->DrawLine(mLeft + pos, mTop,
                             mLeft + pos, mTop + 4);
            else
               mDC->DrawLine(mLeft + pos, mBottom - 4,
                             mLeft + pos, mBottom);
         }
         else {
            if (mFlip)
               mDC->DrawLine(mLeft, mTop + pos,
                             mLeft + 4, mTop + pos);
            else
               mDC->DrawLine(mRight - 4, mTop + pos,
                             mRight, mTop + pos);
         }
      }

      if (mMajorLabels[i].text != wxT(""))
         mDC->DrawText(mMajorLabels[i].text,
                       mMajorLabels[i].lx,
                       mMajorLabels[i].ly);
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
                  mDC->DrawLine(mLeft + pos, mTop,
                                mLeft + pos, mTop + 2);
               else
                  mDC->DrawLine(mLeft + pos, mBottom - 2,
                                mLeft + pos, mBottom);
            }
            else 
            {
               if (mFlip)
                  mDC->DrawLine(mLeft, mTop + pos,
                                mLeft + 2, mTop + pos);
               else
                  mDC->DrawLine(mRight - 2, mTop + pos,
                                mRight, mTop + pos);
            }
         }
         if (mMinorLabels[i].text != wxT(""))
            mDC->DrawText(mMinorLabels[i].text,
                          mMinorLabels[i].lx,
                          mMinorLabels[i].ly);
      }
   }

   mDC->SetFont(*mMinorMinorFont);

   for(i=0; i<mNumMinorMinor; i++) {
      if (mMinorMinorLabels[i].text != wxT(""))
      {
         int pos = mMinorMinorLabels[i].pos;

         if( mbTicksAtExtremes || ((pos!=0)&&(pos!=iMaxPos)))
         {
            if (mOrientation == wxHORIZONTAL) 
            {
               if (mFlip)
                  mDC->DrawLine(mLeft + pos, mTop,
                                mLeft + pos, mTop + 2);
               else
                  mDC->DrawLine(mLeft + pos, mBottom - 2,
                                mLeft + pos, mBottom);
            }
            else 
            {
               if (mFlip)
                  mDC->DrawLine(mLeft, mTop + pos,
                                mLeft + 2, mTop + pos);
               else
                  mDC->DrawLine(mRight - 2, mTop + pos,
                                mRight, mTop + pos);
            }
         }
         mDC->DrawText(mMinorMinorLabels[i].text,
                       mMinorMinorLabels[i].lx,
                       mMinorMinorLabels[i].ly);
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
               mDC->DrawLine(gridPos+xOffset, yOffset, gridPos+xOffset, mGridLineLength+yOffset);
         }
         else {
            if((gridPos != 0) && (gridPos != mGridLineLength))
               mDC->DrawLine(xOffset, gridPos+yOffset, mGridLineLength+xOffset, gridPos+yOffset);
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
               mDC->DrawLine(gridPos+xOffset, yOffset, gridPos+xOffset, mGridLineLength+yOffset);
         }
         else {
            if((gridPos != 0) && (gridPos != mGridLineLength))
               mDC->DrawLine(xOffset, gridPos+yOffset, mGridLineLength+xOffset, gridPos+yOffset);
         }
      }
      
      int zeroPosition = GetZeroPosition();
      if(zeroPosition > 0) {
         // Draw 'zero' grid line in black
         mDC->SetPen(*wxBLACK_PEN);
         if(mOrientation == wxHORIZONTAL) {
            if(zeroPosition != mGridLineLength)
               mDC->DrawLine(zeroPosition+xOffset, yOffset, zeroPosition+xOffset, mGridLineLength+yOffset);
         }
         else {
            if(zeroPosition != mGridLineLength)
               mDC->DrawLine(xOffset, zeroPosition+yOffset, mGridLineLength+xOffset, zeroPosition+yOffset);
         }
      }
   }
}

int Ruler::FindZero(Label * label, const int len)
{
   int i = 0;
   double d = 1.0;   // arbitrary
   wxString s;

   do {
      s = label[i].text;
      if(!s.IsEmpty())
         s.ToDouble(&d);
      else
         d = 1.0; // arbitrary, looking for some text here
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
   if((zero = FindZero(mMajorLabels, mNumMajor)) < 0)
      zero = FindZero(mMinorLabels, mNumMinor);
   return zero;
}

void Ruler::GetMaxSize(wxCoord *width, wxCoord *height)
{

   if (!mValid) {
      wxMemoryDC tmpDC;
      wxBitmap tmpBM(1, 1);
      tmpDC.SelectObject(tmpBM);
      mDC = &tmpDC;
      Update( NULL);
   }

   if (width)
      *width = mRect.GetWidth(); //mMaxWidth;

   if (height)
      *height = mRect.GetHeight(); //mMaxHeight;
}


void Ruler::SetCustomMode(bool value) { mCustom = value; }

void Ruler::SetCustomMajorLabels(wxArrayString *label, int numLabel, int start, int step)
{
   int i;

   mNumMajor = numLabel;
   mMajorLabels = new Label[numLabel];

   for(i=0; i<numLabel; i++) {
      mMajorLabels[i].text = label->Item(i);
      mMajorLabels[i].pos  = start + i*step;
   }
   //Remember: delete majorlabels....
}

void Ruler::SetCustomMinorLabels(wxArrayString *label, int numLabel, int start, int step)
{
   int i;

   mNumMinor = numLabel;
   mMinorLabels = new Label[numLabel];

   for(i=0; i<numLabel; i++) {
      mMinorLabels[i].text = label->Item(i);
      mMinorLabels[i].pos  = start + i*step;
   }
   //Remember: delete majorlabels....
}

//
// RulerPanel
//

BEGIN_EVENT_TABLE(RulerPanel, wxPanel)
   EVT_ERASE_BACKGROUND(RulerPanel::OnErase)
   EVT_PAINT(RulerPanel::OnPaint)
   EVT_SIZE(RulerPanel::OnSize)
END_EVENT_TABLE()

IMPLEMENT_CLASS(RulerPanel, wxPanel)

RulerPanel::RulerPanel(wxWindow* parent, wxWindowID id,
                       const wxPoint& pos /*= wxDefaultPosition*/,
                       const wxSize& size /*= wxDefaultSize*/):
   wxPanel(parent, id, pos, size)
{
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

#if defined(__WXGTK__)
   dc.SetBackground(wxBrush(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE)));
#endif

   dc.Clear();
   ruler.Draw(dc);
}

void RulerPanel::OnSize(wxSizeEvent & WXUNUSED(evt))
{
   Refresh(false);
}

// LL:  We're overloading DoSetSize so that we can update the ruler bounds immediately
//      instead of waiting for a wxEVT_SIZE to come through.  This is needed by (at least)
//      FreqWindow since it needs to have an updated ruler before RulerPanel gets the
//      size event.
void RulerPanel::DoSetSize(int x, int y,
                           int width, int height,
                           int sizeFlags)
{
   wxPanel::DoSetSize(x, y, width, height, sizeFlags);

   int w, h;
   GetClientSize(&w, &h);

   ruler.SetBounds(0, 0, w-1, h-1);
}

/**********************************************************************

  Implementation of AdornedRulerPanel.
  Either we find a way to make this more generic, Or it will move
  out of the widgets subdirectory into its own source file.

**********************************************************************/

#include "../ViewInfo.h"
#include "../AColor.h"

BEGIN_EVENT_TABLE(AdornedRulerPanel, wxPanel)
   EVT_ERASE_BACKGROUND(AdornedRulerPanel::OnErase)
   EVT_PAINT(AdornedRulerPanel::OnPaint)
   EVT_SIZE(AdornedRulerPanel::OnSize)
   EVT_MOUSE_EVENTS(AdornedRulerPanel::OnMouseEvents)
   EVT_MOUSE_CAPTURE_LOST(AdornedRulerPanel::OnCaptureLost)
END_EVENT_TABLE()

AdornedRulerPanel::AdornedRulerPanel(wxWindow* parent,
                                     wxWindowID id,
                                     const wxPoint& pos,
                                     const wxSize& size,
                                     ViewInfo *viewinfo):
   wxPanel( parent, id, pos, size )
{
   SetLabel( _("Vertical Ruler") );
   SetName( _("Vertical Ruler") );

   mLeftOffset = 0;
   mCurPos = -1;
   mIndPos = -1;
   mIndType = -1;
   mPlayRegionStart = -1;
   mPlayRegionEnd = -1;
   mMouseEventState = mesNone;

   mBuffer = new wxBitmap( 1, 1 );
   mViewInfo = viewinfo;

   mOuter = GetClientRect();

   mInner = mOuter;
   mInner.x += 1;          // +1 for left bevel
   mInner.y += 1;          // +1 for top bevel
   mInner.width -= 2;      // -2 for left and right bevels
   mInner.height -= 3;     // -3 for top and bottom bevels and bottom line

   ruler.SetBounds( mInner.GetLeft(),
                    mInner.GetTop(),
                    mInner.GetRight(),
                    mInner.GetBottom() );
   ruler.SetLabelEdges( false );
   ruler.SetFormat( Ruler::TimeFormat );
}

AdornedRulerPanel::~AdornedRulerPanel()
{
   delete mBuffer;
}

void AdornedRulerPanel::OnErase(wxEraseEvent & WXUNUSED(evt))
{
   // Ignore it to prevent flashing
}

void AdornedRulerPanel::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
#if defined(__WXMAC__)
   wxPaintDC dc(this);
#else
   wxBufferedPaintDC dc(this);
#endif

   DoDrawBorder(&dc);

   if (mViewInfo->sel0 < mViewInfo->sel1)
   {
      DoDrawSelection(&dc);
   }

   if (mIndType >= 0)
   {
      DoDrawIndicator(&dc);
   }

   DoDrawMarks(&dc, true);

   if (mViewInfo->sel0 == mViewInfo->sel1)
   {
      DoDrawCursor(&dc);
   }
   
   DoDrawPlayRegion(&dc);
}

void AdornedRulerPanel::OnSize(wxSizeEvent & WXUNUSED(evt))
{
   mOuter = GetClientRect();

   mInner = mOuter;
   mInner.x += 1;          // +1 for left bevel
   mInner.y += 1;          // +1 for top bevel
   mInner.width -= 2;      // -2 for left and right bevels
   mInner.height -= 3;     // -3 for top and bottom bevels and bottom line

   ruler.SetBounds( mInner.GetLeft(),
                    mInner.GetTop(),
                    mInner.GetRight(),
                    mInner.GetBottom() );

   if( mBuffer )
   {
      delete mBuffer;
   }

   mBuffer = new wxBitmap( mOuter.GetWidth(), mOuter.GetHeight() );

   Refresh( false );
}

double AdornedRulerPanel::Pos2Time(int p)
{
   return (p-mLeftOffset) / mViewInfo->zoom + mViewInfo->h;
}

int AdornedRulerPanel::Time2Pos(double t)
{
   return mLeftOffset + (int)((t-mViewInfo->h) * mViewInfo->zoom + 0.5);
}

bool AdornedRulerPanel::IsWithinMarker(int mousePosX, double markerTime)
{
   if (markerTime < 0)
      return false;
      
   int pixelPos = Time2Pos(markerTime);
   int boundLeft = pixelPos - SELECT_TOLERANCE_PIXEL;
   int boundRight = pixelPos + SELECT_TOLERANCE_PIXEL;
   
   return mousePosX >= boundLeft && mousePosX < boundRight;
}

void AdornedRulerPanel::OnMouseEvents(wxMouseEvent &evt)
{
   bool isWithinStart = IsWithinMarker(evt.GetX(), mPlayRegionStart);
   bool isWithinEnd = IsWithinMarker(evt.GetX(), mPlayRegionEnd);

   mLastMouseX = evt.GetX();

   if (isWithinStart || isWithinEnd)
      SetCursor(wxCursor(wxCURSOR_SIZEWE));
   else
      SetCursor(wxCursor(wxCURSOR_HAND));
      
   double mouseTime = Pos2Time(evt.GetX());
   if (mouseTime < 0.0) {
      mouseTime = 0.0;
   }
   
   if (evt.LeftDown())
   {
      mButtonDownMousePos = evt.GetX();

      if (isWithinStart && isWithinEnd)
      {
         // Both could be selected, check which marker is nearer
         if (fabs(mouseTime-mPlayRegionStart) < fabs(mouseTime-mPlayRegionEnd))
            mMouseEventState = mesDraggingPlayRegionStart;
         else
            mMouseEventState = mesDraggingPlayRegionEnd;
      } else if (isWithinStart)
         mMouseEventState = mesDraggingPlayRegionStart;
      else if (isWithinEnd)
         mMouseEventState = mesDraggingPlayRegionEnd;
      else {
         // First, we enter "click" mode to avoid selecting a small range
         // accidentially
         mMouseEventState = mesSelectingPlayRegionClick;
         mPlayRegionStart = mouseTime;
         mPlayRegionEnd = mouseTime;
         Refresh();
      }
         
      CaptureMouse();
   }
   
   switch (mMouseEventState)
   {
   case mesNone:
      // do nothing
      break;
   case mesDraggingPlayRegionStart:
      mPlayRegionStart = mouseTime;
      Refresh();
      break;
   case mesDraggingPlayRegionEnd:
      mPlayRegionEnd = mouseTime;
      Refresh();
      break;
   case mesSelectingPlayRegionClick:
      if (abs(evt.GetX() - mButtonDownMousePos) > SELECT_TOLERANCE_PIXEL)
      {
         // User moved mouse at least SELECT_TOLERANCE_PIXEL, so change
         // from "click" mode to "range" mode to allow selecting a range
         mMouseEventState = mesSelectingPlayRegionRange;
         mPlayRegionEnd = mouseTime;
         Refresh();
      }
      break;
   case mesSelectingPlayRegionRange:
      mPlayRegionEnd = mouseTime;
      Refresh();
      break;
   }
   
   if (evt.LeftUp())
   {
      if (HasCapture())
         ReleaseMouse();

      if (mPlayRegionStart < 0 && mPlayRegionEnd >= 0)
      {
         // This makes no sense, remove play region
         mPlayRegionStart = -1;
         mPlayRegionEnd = -1;
      }
      
      if (mPlayRegionEnd < mPlayRegionStart)
      {
         // Swap values to make sure mPlayRegionStart > mPlayRegionEnd
         double tmp = mPlayRegionStart;
         mPlayRegionStart = mPlayRegionEnd;
         mPlayRegionEnd = tmp;
      }
      
      bool startPlaying = mPlayRegionStart >= 0 && 
         (mMouseEventState == mesSelectingPlayRegionClick ||
          mMouseEventState == mesSelectingPlayRegionRange);
          
      mMouseEventState = mesNone;
      
      if (startPlaying)
      {
         ControlToolBar* ctb = mProject->GetControlToolBar();
         ctb->StopPlaying();
         ctb->PlayDefault();
      }
   }
}

void AdornedRulerPanel::OnCaptureLost(wxMouseCaptureLostEvent & WXUNUSED(evt))
{
   wxMouseEvent e(wxEVT_LEFT_UP);
   e.m_x = mLastMouseX;
   OnMouseEvents(e);
}

void AdornedRulerPanel::DoDrawPlayRegion(wxDC * dc)
{
   double start, end;
   GetPlayRegion(&start, &end);

   if (start >= 0)
   {
      int x1 = Time2Pos(start) + 1;
      int x2 = Time2Pos(end);
      int y = mInner.height/2;

      bool isLocked = mProject->IsPlayRegionLocked();
      AColor::PlayRegionColor(dc, isLocked);
   
      wxPoint tri[3];
      wxRect r;

      tri[0].x = x1;
      tri[0].y = y + PLAY_REGION_GLOBAL_OFFSET_Y;
      tri[1].x = x1 + PLAY_REGION_TRIANGLE_SIZE;
      tri[1].y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
      tri[2].x = x1 + PLAY_REGION_TRIANGLE_SIZE;
      tri[2].y = y + PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
      dc->DrawPolygon(3, tri);

      r.x = x1;
      r.y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
      r.width = PLAY_REGION_RECT_WIDTH;
      r.height = PLAY_REGION_TRIANGLE_SIZE*2 + 1;
      dc->DrawRectangle(r);
   
      if (end != start)
      {
         tri[0].x = x2;
         tri[0].y = y + PLAY_REGION_GLOBAL_OFFSET_Y;
         tri[1].x = x2 - PLAY_REGION_TRIANGLE_SIZE;
         tri[1].y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
         tri[2].x = x2 - PLAY_REGION_TRIANGLE_SIZE;
         tri[2].y = y + PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
         dc->DrawPolygon(3, tri);

         r.x = x2 - PLAY_REGION_RECT_WIDTH + 1;
         r.y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
         r.width = PLAY_REGION_RECT_WIDTH;
         r.height = PLAY_REGION_TRIANGLE_SIZE*2 + 1;
         dc->DrawRectangle(r);
   
         r.x = x1 + PLAY_REGION_TRIANGLE_SIZE;
         r.y = y - PLAY_REGION_RECT_HEIGHT/2 + PLAY_REGION_GLOBAL_OFFSET_Y;
         r.width = x2-x1 - PLAY_REGION_TRIANGLE_SIZE*2;
         r.height = PLAY_REGION_RECT_HEIGHT;
         dc->DrawRectangle(r);
      }
   }
}

void AdornedRulerPanel::DoDrawBorder(wxDC * dc)
{
   // Draw AdornedRulerPanel border
   AColor::MediumTrackInfo( dc, false );
   dc->DrawRectangle( mInner );

   wxRect r = mOuter;
   r.width -= 1;                 // -1 for bevel
   r.height -= 2;                // -2 for bevel and for bottom line
   AColor::BevelTrackInfo( *dc, true, r );

   dc->SetPen( *wxBLACK_PEN );
   dc->DrawLine( mOuter.x,
                 mOuter.y + mOuter.height - 1,
                 mOuter.x + mOuter.width,
                 mOuter.y + mOuter.height - 1 );
}

void AdornedRulerPanel::DoDrawMarks(wxDC * dc, bool /*text */ )
{
   double min = mViewInfo->h - mLeftOffset / mViewInfo->zoom;
   double max = min + mInner.width / mViewInfo->zoom;
   
   ruler.SetTickColour( theTheme.Colour( clrTrackPanelText ) );
   ruler.SetRange( min, max );
   ruler.Draw( *dc );
}

void AdornedRulerPanel::DrawSelection()
{
   Refresh(false);
}

void AdornedRulerPanel::DoDrawSelection(wxDC * dc)
{
   // Draw selection
   double zoom = mViewInfo->zoom;
   double sel0 = mViewInfo->sel0 - mViewInfo->h + mLeftOffset / zoom;
   double sel1 = mViewInfo->sel1 - mViewInfo->h + mLeftOffset / zoom;

   if( sel0 < 0.0 )
      sel0 = 0.0;

   if( sel1 > ( mInner.width / zoom ) )
      sel1 = mInner.width / zoom;

   int p0 = int ( sel0 * zoom + 1.5 );
   int p1 = int ( sel1 * zoom + 2.5 );

   dc->SetBrush( wxBrush( theTheme.Colour( clrRulerBackground )) );
   dc->SetPen(   wxPen(   theTheme.Colour( clrRulerBackground )) );

   wxRect r;
   r.x = p0;
   r.y = 1;
   r.width = p1 - p0 - 1;
   r.height = mInner.height;
   dc->DrawRectangle( r );
}

void AdornedRulerPanel::DrawCursor(double pos)
{
   mCurPos = pos;

   Refresh(false);
}

void AdornedRulerPanel::DoDrawCursor(wxDC * dc)
{
   int x = mLeftOffset + int ( ( mCurPos - mViewInfo->h ) * mViewInfo->zoom );

   // Draw cursor in ruler
   dc->DrawLine( x, 1, x, mInner.height );
}

//
//This draws the little triangular indicator on the 
//AdornedRulerPanel.
//
void AdornedRulerPanel::ClearIndicator()
{
   mIndType = -1;

   Refresh(false);
}

void AdornedRulerPanel::DrawIndicator( double pos, bool rec )
{
   mIndPos = pos;

   if( mIndPos < 0 )
   {
      ClearIndicator();
      return;
   }

   mIndType = ( rec ? 0 : 1 );

   Refresh(false);
}

void AdornedRulerPanel::DoDrawIndicator(wxDC * dc)
{
   if( mIndType < 0 )
   {
      return;
   }

   int indsize = 6;
   int x = mLeftOffset + int ( ( mIndPos - mViewInfo->h ) * mViewInfo->zoom );

   wxPoint tri[ 3 ];
   tri[ 0 ].x = x - indsize;
   tri[ 0 ].y = 1;
   tri[ 1 ].x = x + indsize;
   tri[ 1 ].y = 1;
   tri[ 2 ].x = x;
   tri[ 2 ].y = ( indsize * 3 ) / 2 + 1;

   AColor::IndicatorColor( dc, ( mIndType ? true : false ) );
   dc->DrawPolygon( 3, tri );
}

void AdornedRulerPanel::SetPlayRegion(double playRegionStart,
                                      double playRegionEnd)
{
   // This is called by AudacityProject to make the play region follow
   // the current selection. But while the user is selecting a play region
   // with the mouse directly in the ruler, changes from outside are blocked.
   if (mMouseEventState != mesNone)
      return;
      
   mPlayRegionStart = playRegionStart;
   mPlayRegionEnd = playRegionEnd;
   Refresh();
}

void AdornedRulerPanel::ClearPlayRegion()
{
   mPlayRegionStart = -1;
   mPlayRegionEnd = -1;
   Refresh();
}

void AdornedRulerPanel::GetPlayRegion(double* playRegionStart,
                                      double* playRegionEnd)
{
   if (mPlayRegionStart >= 0 && mPlayRegionEnd >= 0 &&
       mPlayRegionEnd < mPlayRegionStart)
   {
      // swap values to make sure end > start
      *playRegionStart = mPlayRegionEnd;
      *playRegionEnd = mPlayRegionStart;
   } else
   {
      *playRegionStart = mPlayRegionStart;
      *playRegionEnd = mPlayRegionEnd;
   }
}

void AdornedRulerPanel::GetMaxSize(wxCoord *width, wxCoord *height)
{
   ruler.GetMaxSize(width, height);
}

