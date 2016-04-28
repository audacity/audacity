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
#include "Ruler.h"

#include <math.h>

#include <wx/dcscreen.h>
#include <wx/dcmemory.h>
#include <wx/dcbuffer.h>
#include <wx/settings.h>
#include <wx/menu.h>
#include <wx/menuitem.h>
#include <wx/tooltip.h>

#include "../AColor.h"
#include "../AudioIO.h"
#include "../Internat.h"
#include "../Project.h"
#include "../toolbars/ControlToolBar.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../Experimental.h"
#include "../TimeTrack.h"
#include "../TrackPanel.h"
#include "../TrackPanelCellIterator.h"
#include "../TrackPanelOverlay.h"
#include "../Menus.h"
#include "../NumberScale.h"
#include "../Prefs.h"
#include "../Snap.h"
#include "../tracks/ui/Scrubbing.h"

using std::min;
using std::max;

#define SELECT_TOLERANCE_PIXEL 4

#define PLAY_REGION_TRIANGLE_SIZE 6
#define PLAY_REGION_RECT_WIDTH 1
#define PLAY_REGION_RECT_HEIGHT 3
#define PLAY_REGION_GLOBAL_OFFSET_Y 7

wxColour Ruler::mTickColour{ 153, 153, 153 };

//
// Ruler
//

Ruler::Ruler()
   : mpNumberScale(0)
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
   mUnits = wxT("");

   mLeft = -1;
   mTop = -1;
   mRight = -1;
   mBottom = -1;
   mbTicksOnly = true;
   mbTicksAtExtremes = false;
   mPen.SetColour(mTickColour);

   // Note: the font size is now adjusted automatically whenever
   // Invalidate is called on a horizontal Ruler, unless the user
   // calls SetFonts manually.  So the defaults here are not used
   // often.

   int fontSize = 10;
#ifdef __WXMSW__
   fontSize = 8;
#endif

   mMinorMinorFont = new wxFont(fontSize - 1, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   mMinorFont = new wxFont(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   mMajorFont = new wxFont(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD);

   mUserFonts = false;

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

   mTwoTone = false;

   mUseZoomInfo = NULL;
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

   delete mpNumberScale;
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
         delete mpNumberScale;
         Invalidate();
      }
   }
   else {
      if (!mpNumberScale || *mpNumberScale != *pScale) {
         delete mpNumberScale;
         mpNumberScale = new NumberScale(*pScale);
         Invalidate();
      }
   }
}

void Ruler::OfflimitsPixels(int start, int end)
{
   int i;

   if (!mUserBits) {
      if (mOrientation == wxHORIZONTAL)
         mLength = mRight-mLeft;
      else
         mLength = mBottom-mTop;
      if( mLength < 0 )
         return;
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

   label->value = d;
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

      if (mOrientation == wxHORIZONTAL)
         desiredPixelHeight = mBottom - mTop - 5; // height less ticks and 1px gap
      else
         desiredPixelHeight = 12;   // why 12?  10 -> 12 seems to be max/min

      if (desiredPixelHeight < 10)//8)
         desiredPixelHeight = 10;//8;
      if (desiredPixelHeight > 12)
         desiredPixelHeight = 12;

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

      if (mMajorFont)
         delete mMajorFont;
      mMajorFont = new wxFont(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD);

      if (mMinorFont)
         delete mMinorFont;
      mMinorFont = new wxFont(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);

      if (mMinorMinorFont)
         delete mMinorMinorFont;
      mMinorMinorFont = new wxFont(fontSize - 1, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
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
            mid = int(zoomInfo->TimeToPosition(0.0, mLeftOffset));
         else
            mid = (int)(mLength*(mMin / (mMin - mMax)) + 0.5);
         const int iMaxPos = (mOrientation == wxHORIZONTAL) ? mRight : mBottom - 5;
         if (mid >= 0 && mid < iMaxPos)
            Tick(mid, 0.0, true, false);
      }

      double sg = UPP > 0.0? 1.0: -1.0;

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
            }
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

      NumberScale numberScale(mpNumberScale
         ? *mpNumberScale
         : NumberScale(nstLogarithmic, mMin, mMax, 1.0f)
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
            for (int f = start; f != int(end); f += mstep) {
               if (int(f / 10) != f / 10.0f) {
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
            mDC->DrawLine(mLeft, mTop, mRight+1, mTop);
         else
            mDC->DrawLine(mLeft, mBottom, mRight+1, mBottom);
      }
      else {
         if (mFlip)
            mDC->DrawLine(mLeft, mTop, mLeft, mBottom+1);
         else
         {
            // These calculations appear to be wrong, and to never have been used (so not tested) prior to MixerBoard.
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

      mMajorLabels[i].Draw(*mDC, mTwoTone);
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
         mMinorLabels[i].Draw(*mDC, mTwoTone);
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
         mMinorMinorLabels[i].Draw(*mDC, mTwoTone);
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
   if((zero = FindZero(mMajorLabels, mNumMajor)) < 0)
      zero = FindZero(mMinorLabels, mNumMinor);
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

void Ruler::SetCustomMajorLabels(wxArrayString *label, int numLabel, int start, int step)
{
   int i;

   mNumMajor = numLabel;
   mMajorLabels = new Label[numLabel];

   for(i=0; i<numLabel; i++) {
      mMajorLabels[i].text = label->Item(i);
      mMajorLabels[i].pos  = start + i*step;
   }
   //Remember: DELETE majorlabels....
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
   //Remember: DELETE majorlabels....
}

void Ruler::Label::Draw(wxDC&dc, bool twoTone) const
{
   if (text != wxT("")) {
      bool altColor = twoTone && value < 0.0;

#ifdef EXPERIMENTAL_THEMING
      // TODO:  handle color distinction
      dc.SetTextForeground(mTickColour);
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
   wxPanel::DoSetSize(x, y, width, height, sizeFlags);

   int w, h;
   GetClientSize(&w, &h);

   ruler.SetBounds(0, 0, w-1, h-1);
}

/**********************************************************************

QuickPlayIndicatorOverlay.
Graphical helper for AdornedRulerPanel.

**********************************************************************/

class QuickPlayIndicatorOverlay final : public TrackPanelOverlay
{
public:
   QuickPlayIndicatorOverlay(AudacityProject *project)
      : mProject(project)
      , mOldQPIndicatorPos(-1)
      , mNewQPIndicatorPos(-1)
      , mOldQPIndicatorSnapped(false)
      , mNewQPIndicatorSnapped(false)
   {
   }

   virtual ~QuickPlayIndicatorOverlay()
   {
   }

   void Update(int x, bool snapped = false, bool previewScrub = false)
   {
      mNewQPIndicatorPos = x;
      mNewQPIndicatorSnapped = snapped;
      mNewPreviewingScrub = previewScrub;

      // Not strictly needed, but this reduces the lag in updating
      // track panel causing momentary mismatch between the triangle
      // in the ruler and the white line.

      mProject->GetTrackPanel()->DrawOverlays(false);
   }

private:
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw
      (wxDC &dc, TrackPanelCellIterator begin, TrackPanelCellIterator end) override;

   AudacityProject *mProject;
   int mOldQPIndicatorPos;
   int mNewQPIndicatorPos;
   bool mOldQPIndicatorSnapped;
   bool mNewQPIndicatorSnapped;
   bool mOldPreviewingScrub{}, mNewPreviewingScrub{};
};

std::pair<wxRect, bool> QuickPlayIndicatorOverlay::DoGetRectangle(wxSize size)
{
   wxRect rect(mOldQPIndicatorPos, 0, 1, size.GetHeight());
   return std::make_pair(
      rect,
      (mOldQPIndicatorPos != mNewQPIndicatorPos ||
       mOldQPIndicatorSnapped != mNewQPIndicatorSnapped ||
       mOldPreviewingScrub != mNewPreviewingScrub)
   );
}

void QuickPlayIndicatorOverlay::Draw
(wxDC &dc, TrackPanelCellIterator begin, TrackPanelCellIterator end)
{
   mOldQPIndicatorPos = mNewQPIndicatorPos;
   mOldQPIndicatorSnapped = mNewQPIndicatorSnapped;
   mOldPreviewingScrub = mNewPreviewingScrub;

   if (mOldQPIndicatorPos >= 0) {
      mOldPreviewingScrub
      ? AColor::IndicatorColor(&dc, true) // Draw green line for preview.
      : mOldQPIndicatorSnapped
        ? AColor::SnapGuidePen(&dc)
        : AColor::Light(&dc, false)
      ;

      // Draw indicator in all visible tracks
      for (; begin != end; ++begin)
      {
         TrackPanelCellIterator::value_type data(*begin);
         Track *const pTrack = dynamic_cast<Track*>(data.first);
         if (!pTrack)
            continue;
         const wxRect &rect = data.second;

         // Draw the NEW indicator in its NEW location
         AColor::Line(dc,
            mOldQPIndicatorPos,
            rect.GetTop(),
            mOldQPIndicatorPos,
            rect.GetBottom());
      }
   }
}

/**********************************************************************

  Implementation of AdornedRulerPanel.
  Either we find a way to make this more generic, Or it will move
  out of the widgets subdirectory into its own source file.

**********************************************************************/

#include "../ViewInfo.h"
#include "../AColor.h"

enum {
   OnToggleQuickPlayID = 7000,
   OnSyncQuickPlaySelID,
   OnTimelineToolTipID,
   OnAutoScrollID,
   OnLockPlayRegionID,

   OnShowHideScrubbingID,
};

BEGIN_EVENT_TABLE(AdornedRulerPanel, wxPanel)
   EVT_PAINT(AdornedRulerPanel::OnPaint)
   EVT_SIZE(AdornedRulerPanel::OnSize)
   EVT_MOUSE_EVENTS(AdornedRulerPanel::OnMouseEvents)
   EVT_MOUSE_CAPTURE_LOST(AdornedRulerPanel::OnCaptureLost)

   // Context menu commands
   EVT_MENU(OnToggleQuickPlayID, AdornedRulerPanel::OnToggleQuickPlay)
   EVT_MENU(OnSyncQuickPlaySelID, AdornedRulerPanel::OnSyncSelToQuickPlay)
   EVT_MENU(OnTimelineToolTipID, AdornedRulerPanel::OnTimelineToolTips)
   EVT_MENU(OnAutoScrollID, AdornedRulerPanel::OnAutoScroll)
   EVT_MENU(OnLockPlayRegionID, AdornedRulerPanel::OnLockPlayRegion)

   // Main menu commands
   EVT_MENU(OnShowHideScrubbingID, AdornedRulerPanel::OnShowHideScrubbing)

END_EVENT_TABLE()

AdornedRulerPanel::AdornedRulerPanel(AudacityProject* parent,
                                     wxWindowID id,
                                     const wxPoint& pos,
                                     const wxSize& size,
                                     ViewInfo *viewinfo)
:  wxPanel(parent, id, pos, size)
, mViewInfo(viewinfo)
{
   SetLabel( _("Timeline") );
   SetName(GetLabel());
   SetBackgroundStyle(wxBG_STYLE_PAINT);

   mBack = new wxBitmap(1, 1);
   mBackDC.SelectObject(*mBack);

   mCursorDefault = wxCursor(wxCURSOR_DEFAULT);
   mCursorHand = wxCursor(wxCURSOR_HAND);
   mCursorSizeWE = wxCursor(wxCURSOR_SIZEWE);

   mLeftOffset = 0;
   mIndTime = -1;
   mIndType = -1;
   mQuickPlayInd = false;
   mLastQuickPlayX = -1;
   mPlayRegionStart = -1;
   mPlayRegionLock = false;
   mPlayRegionEnd = -1;
   mOldPlayRegionStart = -1;
   mOldPlayRegionEnd = -1;
   mLeftDownClick = -1;
   mMouseEventState = mesNone;
   mIsDragging = false;

   mOuter = GetClientRect();

   mRuler.SetUseZoomInfo(mLeftOffset, mViewInfo);
   mRuler.SetLabelEdges( false );
   mRuler.SetFormat( Ruler::TimeFormat );

   mTracks = parent->GetTracks();

   mSnapManager = NULL;
   mIsSnapped = false;

   mIsRecording = false;

   mTimelineToolTip = !!gPrefs->Read(wxT("/QuickPlay/ToolTips"), 1L);
   mPlayRegionDragsSelection = (gPrefs->Read(wxT("/QuickPlay/DragSelection"), 0L) == 1)? true : false; 
   mQuickPlayEnabled = !!gPrefs->Read(wxT("/QuickPlay/QuickPlayEnabled"), 1L);

   UpdatePrefs();

#if wxUSE_TOOLTIPS
   wxToolTip::Enable(true);
#endif

   wxTheApp->Connect(EVT_AUDIOIO_CAPTURE,
                     wxCommandEventHandler(AdornedRulerPanel::OnCapture),
                     NULL,
                     this);
}

AdornedRulerPanel::~AdornedRulerPanel()
{
   if(HasCapture())
      ReleaseMouse();

   // Done with the snap manager
   if (mSnapManager) {
      delete mSnapManager;
   }

   wxTheApp->Disconnect(EVT_AUDIOIO_CAPTURE,
                        wxCommandEventHandler(AdornedRulerPanel::OnCapture),
                        NULL,
                        this);

   if (mBack)
   {
      mBackDC.SelectObject(wxNullBitmap);
      delete mBack;
   }
}

namespace {
   static const wxChar *scrubEnabledPrefName = wxT("/QuickPlay/ScrubbingEnabled");

   bool ReadScrubEnabledPref()
   {
      bool result {};
      gPrefs->Read(scrubEnabledPrefName, &result, true);
      return result;
   }

   void WriteScrubEnabledPref(bool value)
   {
      gPrefs->Write(scrubEnabledPrefName, value);
   }
}

void AdornedRulerPanel::UpdatePrefs()
{
#ifdef EXPERIMENTAL_SCROLLING_LIMITS
#ifdef EXPERIMENTAL_TWO_TONE_TIME_RULER
   {
      bool scrollBeyondZero = false;
      gPrefs->Read(wxT("/GUI/ScrollBeyondZero"), &scrollBeyondZero, false);
      mRuler.SetTwoTone(scrollBeyondZero);
   }
#endif
#endif

   mShowScrubbing = ReadScrubEnabledPref();
   // Affected by the last
   UpdateRects();

   RegenerateTooltips();
}

void AdornedRulerPanel::InvalidateRuler()
{
   mRuler.Invalidate();
}

void AdornedRulerPanel::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   if (mTimelineToolTip) {
      if (mIsRecording) {
         this->SetToolTip(_("Timeline actions disabled during recording"));
      }
      else if (!mQuickPlayEnabled) {
         this->SetToolTip(_("Quick-Play disabled"));
      }
      else {
         this->SetToolTip(_("Quick-Play enabled"));
      }
   }
   else {
      this->SetToolTip(NULL);
   }
#endif
}

void AdornedRulerPanel::HideQuickPlayIndicator()
{
   mQuickPlayInd = false;
   DrawQuickPlayIndicator(NULL);
   Refresh(false);
}

void AdornedRulerPanel::OnCapture(wxCommandEvent & evt)
{
   evt.Skip();

   if (evt.GetInt() != 0)
   {
      // Set cursor immediately  because OnMouseEvents is not called
      // if recording is initiated by a modal window (Timer Record).
      SetCursor(mCursorDefault);
      mIsRecording = true;
   }
   else {
      SetCursor(mCursorHand);
      mIsRecording = false;
   }
   RegenerateTooltips();
}

enum : int {
   IndicatorSmallWidth = 9,
   IndicatorMediumWidth = 13,
   IndicatorOffset = 1,

   TopMargin = 1,
   BottomMargin = 2, // for bottom bevel and bottom line
   LeftMargin = 1,
   RightMargin = 1,
};

inline int IndicatorHeightForWidth(int width)
{
   return ((width / 2) * 3) / 2;
}

inline int IndicatorWidthForHeight(int height)
{
   // Not an exact inverse of the above, with rounding, but good enough
   return std::max(static_cast<int>(IndicatorSmallWidth),
      (((height) * 2) / 3) * 2
   );
}

int AdornedRulerPanel::IndicatorBigWidth()
{
   return IndicatorWidthForHeight(IndicatorBigHeight());
}

enum {
   ScrubHeight = 14,
   RulerHeight = 28
};

int AdornedRulerPanel::IndicatorBigHeight()
{
   return std::max(int(ScrubHeight - TopMargin),
                   int(IndicatorMediumWidth));
}

void AdornedRulerPanel::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   wxPaintDC dc(this);

   if (mBack)
   {
      mBackDC.SelectObject(wxNullBitmap);
      delete mBack;
   }
      
   wxSize sz = GetClientSize();
   mBack = new wxBitmap();
   mBack->Create(sz.x, sz.y, dc);
   mBackDC.SelectObject(*mBack);

   DoDrawBorder(&mBackDC);

   if (!mViewInfo->selectedRegion.isPoint())
   {
      DoDrawSelection(&mBackDC);
   }

   DoDrawMarks(&mBackDC, true);

   if (mIndType >= 0)
   {
      const bool scrub = mProject->GetScrubber().HasStartedScrubbing();
      auto width = scrub ? IndicatorBigWidth() : IndicatorMediumWidth;
      DoDrawIndicator(&mBackDC, mIndTime, mIndType != 0, width, scrub);
   }

   if (mViewInfo->selectedRegion.isPoint())
   {
      DoDrawCursor(&mBackDC);
   }

   DoDrawPlayRegion(&mBackDC);

   dc.Blit(0, 0, mBack->GetWidth(), mBack->GetHeight(), &mBackDC, 0, 0);

   if (mQuickPlayInd)
   {
      DrawQuickPlayIndicator(&dc);
   }
}

void AdornedRulerPanel::OnSize(wxSizeEvent & WXUNUSED(evt))
{
   mOuter = GetClientRect();
   if (mOuter.GetWidth() == 0 || mOuter.GetHeight() == 0)
   {
      return;
   }

   UpdateRects();

   Refresh();
}

void AdornedRulerPanel::UpdateRects()
{
   mInner = mOuter;
   mInner.x += LeftMargin;
   mInner.width -= (LeftMargin + RightMargin);

   wxRect *top = &mInner;

   if (mShowScrubbing) {
      mScrubZone = mInner;
      auto scrubHeight = std::min(mScrubZone.height, int(ScrubHeight));
      mScrubZone.height = scrubHeight;
      mInner.height -= scrubHeight;
      mInner.y += scrubHeight;
      top = &mScrubZone;
   }

   top->y += TopMargin;
   top->height -= TopMargin;

   mInner.height -= BottomMargin;

   if (!mShowScrubbing)
      mScrubZone = mInner;

   mRuler.SetBounds(mInner.GetLeft(),
                    mInner.GetTop(),
                    mInner.GetRight(),
                    mInner.GetBottom());

}

double AdornedRulerPanel::Pos2Time(int p, bool ignoreFisheye)
{
   return mViewInfo->PositionToTime(p, mLeftOffset
      , ignoreFisheye
   );
}

int AdornedRulerPanel::Time2Pos(double t, bool ignoreFisheye)
{
   return mViewInfo->TimeToPosition(t, mLeftOffset
      , ignoreFisheye
   );
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
   // Disable mouse actions on Timeline while recording.
   if (mIsRecording)
      return;

   const bool inScrubZone =
      // only if scrubbing is allowed now
      mProject->GetScrubber().CanScrub() &&
      mShowScrubbing &&
      mScrubZone.Contains(evt.GetPosition());

   const bool changeInScrubZone = (inScrubZone != mPrevInScrubZone);
   mPrevInScrubZone = inScrubZone;

   double t0 = mTracks->GetStartTime();
   double t1 = mTracks->GetEndTime();
   double sel0 = mProject->GetSel0();
   double sel1 = mProject->GetSel1();

   wxCoord xx = evt.GetX();
   wxCoord mousePosX = xx;
   UpdateQuickPlayPos(mousePosX);

   // If not looping, restrict selection to end of project
   if (!inScrubZone && !evt.ShiftDown()) {
      mQuickPlayPos = std::min(t1, mQuickPlayPos);
   }

   // If position was adjusted right, we are over menu
   const bool overMenu = (xx < mousePosX);

   auto &scrubber = mProject->GetScrubber();

   // Handle status bar messages
   UpdateStatusBar (
      overMenu || evt.Leaving()
      ? StatusChoice::Leaving
      : evt.Entering() || changeInScrubZone
         ? inScrubZone
            ? StatusChoice::EnteringScrubZone
            : StatusChoice::EnteringQP
         : StatusChoice::NoChange
   );

   if (overMenu && evt.Button(wxMOUSE_BTN_ANY)) {
      if(evt.ButtonDown())
         DoMainMenu();
      return;
   }

   // Handle popup menus
   if (evt.RightDown() && !(evt.LeftIsDown())) {
      if(inScrubZone)
         ShowScrubMenu(evt.GetPosition());
      else
         ShowMenu(evt.GetPosition());

      // dismiss and clear Quick-Play indicator
      HideQuickPlayIndicator();

      if (HasCapture())
         ReleaseMouse();
      return;
   }

   if (scrubber.HasStartedScrubbing()) {
      // If already clicked for scrub, preempt the usual event handling,
      // no matter what the y coordinate.

      // Do this hack so scrubber can detect mouse drags anywhere
      evt.ResumePropagation(wxEVENT_PROPAGATE_MAX);

      if (scrubber.IsScrubbing())
         evt.Skip();
      else if (evt.LeftDClick())
         // On the second button down, switch the pending scrub to scrolling
         scrubber.MarkScrubStart(evt.m_x, true, false);
      else
         evt.Skip();

      mQuickPlayInd = true;
      wxClientDC dc(this);
      DrawQuickPlayIndicator(&dc);

      return;
   }

   // Store the initial play region state
   if(mMouseEventState == mesNone) {
      mOldPlayRegionStart = mPlayRegionStart;
      mOldPlayRegionEnd = mPlayRegionEnd;
      mPlayRegionLock = mProject->IsPlayRegionLocked();
   }

   bool isWithinStart = IsWithinMarker(mousePosX, mOldPlayRegionStart);
   bool isWithinEnd = IsWithinMarker(mousePosX, mOldPlayRegionEnd);
   bool isWithinClick = (mLeftDownClick >= 0) && IsWithinMarker(mousePosX, mLeftDownClick);
   bool canDragSel = !mPlayRegionLock && mPlayRegionDragsSelection;

   // Handle entering and leaving of the bar, or movement from
   // one portion (quick play or scrub) to the other
   if (evt.Leaving() || (changeInScrubZone && inScrubZone)) {
      if (evt.Leaving()) {
         // Erase the line
         HideQuickPlayIndicator();
      }

      SetCursor(mCursorDefault);
      mIsWE = false;

      if (mSnapManager) {
         delete mSnapManager;
         mSnapManager = NULL;
      }

      if(evt.Leaving())
         return;
      // else, may detect a scrub click below
   }
   else if (evt.Entering() || (changeInScrubZone && !inScrubZone)) {
      SetCursor(mCursorHand);
      HideQuickPlayIndicator();
      return;
   }

   if (inScrubZone) {
      if (evt.LeftDown())
         scrubber.MarkScrubStart(evt.m_x, false, false);
      UpdateStatusBar(StatusChoice::EnteringScrubZone);
      wxClientDC dc(this);
      DrawQuickPlayIndicator(&dc);
      return;
   }

   if (!mQuickPlayEnabled)
      return;

  if (isWithinStart || isWithinEnd) {
      if (!mIsWE) {
         SetCursor(mCursorSizeWE);
         mIsWE = true;
      }
   }
   else {
      if (mIsWE) {
         SetCursor(mCursorHand);
         mIsWE = false;
      }
   }

   HandleSnapping();

   mQuickPlayInd = true;
   wxClientDC dc(this);
   DrawQuickPlayIndicator(&dc);

   if (evt.LeftDown())
   {
      // Temporarily unlock locked play region
      if (mPlayRegionLock && evt.LeftDown()) {
         //mPlayRegionLock = true;
         mProject->OnUnlockPlayRegion();
      }

      mLeftDownClick = mQuickPlayPos;
      isWithinClick = IsWithinMarker(mousePosX, mLeftDownClick);

      if (isWithinStart || isWithinEnd) {
         // If Quick-Play is playing from a point, we need to treat it as a click
         // not as dragging.
         if (mOldPlayRegionStart == mOldPlayRegionEnd)
            mMouseEventState = mesSelectingPlayRegionClick;
         // otherwise check which marker is nearer
         else {
            // Don't compare times, compare positions.
            //if (fabs(mQuickPlayPos - mPlayRegionStart) < fabs(mQuickPlayPos - mPlayRegionEnd))
            if (abs(Time2Pos(mQuickPlayPos) - Time2Pos(mPlayRegionStart)) <
               abs(Time2Pos(mQuickPlayPos) - Time2Pos(mPlayRegionEnd)))
               mMouseEventState = mesDraggingPlayRegionStart;
            else
               mMouseEventState = mesDraggingPlayRegionEnd;
         }
      }
      else {
         // Clicked but not yet dragging
         mMouseEventState = mesSelectingPlayRegionClick;
      }

      // Check if we are dragging BEFORE CaptureMouse.
      if (mMouseEventState != mesNone)
         SetCursor(mCursorSizeWE);
      CaptureMouse();
   }

   if (evt.LeftIsDown()) {
      switch (mMouseEventState)
      {
      case mesNone:
         // If close to either end of play region, snap to closest
         if (isWithinStart || isWithinEnd) {
            DrawQuickPlayIndicator(NULL);

            if (fabs(mQuickPlayPos - mOldPlayRegionStart) < fabs(mQuickPlayPos - mOldPlayRegionEnd))
               mQuickPlayPos = mOldPlayRegionStart;
            else
               mQuickPlayPos = mOldPlayRegionEnd;
         }
         break;
      case mesDraggingPlayRegionStart:
         DrawQuickPlayIndicator(NULL);

         // Don't start dragging until beyond tollerance initial playback start
         if (!mIsDragging && isWithinStart)
            mQuickPlayPos = mOldPlayRegionStart;
         else
            mIsDragging = true;
         // avoid accidental tiny selection
         if (isWithinEnd)
            mQuickPlayPos = mOldPlayRegionEnd;
         mPlayRegionStart = mQuickPlayPos;
         if (canDragSel) {
            DragSelection();
         }
         break;
      case mesDraggingPlayRegionEnd:
         if (!mIsDragging && isWithinEnd) {
            DrawQuickPlayIndicator(NULL);

            mQuickPlayPos = mOldPlayRegionEnd;
         }
         else
            mIsDragging = true;
         if (isWithinStart) {
            DrawQuickPlayIndicator(NULL);

            mQuickPlayPos = mOldPlayRegionStart;
         }
         mPlayRegionEnd = mQuickPlayPos;
         if (canDragSel) {
            DragSelection();
         }
         break;
      case mesSelectingPlayRegionClick:

         // Don't start dragging until mouse is beyond tollerance of initial click.
         if (isWithinClick || mLeftDownClick == -1) {
            DrawQuickPlayIndicator(NULL);

            mQuickPlayPos = mLeftDownClick;
            mPlayRegionStart = mLeftDownClick;
            mPlayRegionEnd = mLeftDownClick;
         }
         else {
            mMouseEventState = mesSelectingPlayRegionRange;
         }
         break;
      case mesSelectingPlayRegionRange:
         if (isWithinClick) {
            DrawQuickPlayIndicator(NULL);

            mQuickPlayPos = mLeftDownClick;
         }

         if (mQuickPlayPos < mLeftDownClick) {
            mPlayRegionStart = mQuickPlayPos;
            mPlayRegionEnd = mLeftDownClick;
         }
         else {
            mPlayRegionEnd = mQuickPlayPos;
            mPlayRegionStart = mLeftDownClick;
         }
         if (canDragSel) {
            DragSelection();
         }
         break;
      }
      Refresh();
      Update();
   }

   if (evt.LeftUp())
   {
      HideQuickPlayIndicator();

      if (HasCapture())
         ReleaseMouse();

      if (mPlayRegionEnd < mPlayRegionStart) {
         // Swap values to ensure mPlayRegionStart < mPlayRegionEnd
         double tmp = mPlayRegionStart;
         mPlayRegionStart = mPlayRegionEnd;
         mPlayRegionEnd = tmp;
      }

      // We want some audio in the selection, but we allow a dragged
      // region to include selected white-space and space before audio start.
      if (evt.ShiftDown() && (mPlayRegionStart == mPlayRegionEnd)) {
         // Looping the selection or project.
         // Disable if track selection is in white-space beyond end of tracks and
         // play position is outside of track contents.
         if (((sel1 < t0) || (sel0 > t1)) &&
            ((mPlayRegionStart < t0) || (mPlayRegionStart > t1))) {
            ClearPlayRegion();
         }
      }
      // Disable if beyond end.
      else if (mPlayRegionStart >= t1) {
         ClearPlayRegion();
      }
      // Disable if empty selection before start.
      // (allow Quick-Play region to include 'pre-roll' white space)
      else if (((mPlayRegionEnd - mPlayRegionStart) > 0.0) && (mPlayRegionEnd < t0)) {
         ClearPlayRegion();
      }

      // Start / Restart playback on left click.
      bool startPlaying = (mPlayRegionStart >= 0);

      if (startPlaying) {
         ControlToolBar* ctb = mProject->GetControlToolBar();
         ctb->StopPlaying();

         bool loopEnabled = true;
         double start, end;

         if ((mPlayRegionEnd - mPlayRegionStart == 0.0) && evt.ShiftDown()) {
            // Loop play a point will loop either a selection or the project.
            if ((mPlayRegionStart > sel0) && (mPlayRegionStart < sel1)) {
               // we are in a selection, so use the selection
               start = sel0;
               end = sel1;
            } // not in a selection, so use the project
            else {
               start = t0;
               end = t1;
            }
         }
         else {
            start = mPlayRegionStart;
            end = mPlayRegionEnd;
         }
         // Looping a tiny selection may freeze, so just play it once.
         loopEnabled = ((end - start) > 0.001)? true : false;

         AudioIOStartStreamOptions options(mProject->GetDefaultPlayOptions());
         options.playLooped = (loopEnabled && evt.ShiftDown());

         if (!evt.ControlDown())
            options.pStartTime = &mPlayRegionStart;
         else
            options.timeTrack = NULL;

         ControlToolBar::PlayAppearance appearance =
            evt.ControlDown() ? ControlToolBar::PlayAppearance::CutPreview
               : options.playLooped ? ControlToolBar::PlayAppearance::Looped
               : ControlToolBar::PlayAppearance::Straight;
         ctb->PlayPlayRegion((SelectedRegion(start, end)),
                             options, PlayMode::normalPlay,
                             appearance,
                             false,
                             true);

         mPlayRegionStart = start;
         mPlayRegionEnd = end;
         Refresh();
      }

      mMouseEventState = mesNone;
      mIsDragging = false;
      mLeftDownClick = -1;

      if (mPlayRegionLock) {
         // Restore Locked Play region
         SetPlayRegion(mOldPlayRegionStart, mOldPlayRegionEnd);
         mProject->OnLockPlayRegion();
         // and release local lock
         mPlayRegionLock = false;
      }
   }
}

void AdornedRulerPanel::UpdateStatusBar(StatusChoice choice)
{
   if (choice == StatusChoice::NoChange)
      return;

   const auto &scrubber = mProject->GetScrubber();
   const bool scrubbing = scrubber.HasStartedScrubbing();
   if (scrubbing && choice != StatusChoice::Leaving)
      // Don't distinguish zones
      choice = StatusChoice::EnteringScrubZone;
   wxString message{};

   switch (choice) {
      case StatusChoice::EnteringQP:
      {
         // message = Insert timeline status bar message here
      }
         break;

      case StatusChoice::EnteringScrubZone:
      {
         if (scrubbing) {
            if(!scrubber.IsAlwaysSeeking())
               message = _("Click or drag to seek");
         }
         else
            message = _("Click to scrub, Double-Click to scroll, Drag to seek");
      }
         break;

      case StatusChoice::Leaving:
      default:
         break;
   }

   // Display a message, or empty message
   mProject->TP_DisplayStatusMessage(message);
}

void AdornedRulerPanel::DoMainMenu()
{
   wxMenu menu;

   menu.AppendCheckItem(OnShowHideScrubbingID, _("Scrub Bar"));
   menu.Check(OnShowHideScrubbingID, mShowScrubbing);

   // Position the popup similarly to the track control panel menus
   wxPoint pos {
      kLeftInset + kTrackInfoBtnSize + 1,
      GetSize().GetHeight() + 1
   };
   PopupMenu(&menu, pos);
}

void AdornedRulerPanel::OnShowHideScrubbing(wxCommandEvent&)
{
   mShowScrubbing = !mShowScrubbing;
   WriteScrubEnabledPref(mShowScrubbing);
   gPrefs->Flush();
   wxSize size { GetSize().GetWidth(), GetRulerHeight(mShowScrubbing) };
   SetSize(size);
   SetMinSize(size);
   PostSizeEventToParent();
}

void AdornedRulerPanel::OnCaptureLost(wxMouseCaptureLostEvent & WXUNUSED(evt))
{
   DrawQuickPlayIndicator(NULL);

   wxMouseEvent e(wxEVT_LEFT_UP);
   e.m_x = mLastMouseX;
   OnMouseEvents(e);
}

void AdornedRulerPanel::UpdateQuickPlayPos(wxCoord &mousePosX)
{
   // Keep Quick-Play within usable track area.
   TrackPanel *tp = mProject->GetTrackPanel();
   int width;
   tp->GetTracksUsableArea(&width, NULL);
   mousePosX = std::max(mousePosX, tp->GetLeftOffset());
   mousePosX = std::min(mousePosX, tp->GetLeftOffset() + width - 1);

   mLastMouseX = mousePosX;
   mQuickPlayPos = Pos2Time(mousePosX);
}

// Pop-up menus

void AdornedRulerPanel::ShowMenu(const wxPoint & pos)
{
   wxMenu rulerMenu;

   if (mQuickPlayEnabled)
      rulerMenu.Append(OnToggleQuickPlayID, _("Disable Quick-Play"));
   else
      rulerMenu.Append(OnToggleQuickPlayID, _("Enable Quick-Play"));

   wxMenuItem *dragitem;
   if (mPlayRegionDragsSelection && !mProject->IsPlayRegionLocked())
      dragitem = rulerMenu.Append(OnSyncQuickPlaySelID, _("Disable dragging selection"));
   else
      dragitem = rulerMenu.Append(OnSyncQuickPlaySelID, _("Enable dragging selection"));
   dragitem->Enable(mQuickPlayEnabled && !mProject->IsPlayRegionLocked());

#if wxUSE_TOOLTIPS
   if (mTimelineToolTip)
      rulerMenu.Append(OnTimelineToolTipID, _("Disable Timeline Tooltips"));
   else
      rulerMenu.Append(OnTimelineToolTipID, _("Enable Timeline Tooltips"));
#endif

   if (mViewInfo->bUpdateTrackIndicator)
      rulerMenu.Append(OnAutoScrollID, _("Do not scroll while playing"));
   else
      rulerMenu.Append(OnAutoScrollID, _("Update display while playing"));

   wxMenuItem *prlitem;
   if (!mProject->IsPlayRegionLocked())
      prlitem = rulerMenu.Append(OnLockPlayRegionID, _("Lock Play Region"));
   else
      prlitem = rulerMenu.Append(OnLockPlayRegionID, _("Unlock Play Region"));
   prlitem->Enable(mProject->IsPlayRegionLocked() || (mPlayRegionStart != mPlayRegionEnd));

   PopupMenu(&rulerMenu, pos);
}

void AdornedRulerPanel::ShowScrubMenu(const wxPoint & pos)
{
   auto &scrubber = mProject->GetScrubber();
   PushEventHandler(&scrubber);
   auto cleanup = finally([this]{ PopEventHandler(); });

   wxMenu rulerMenu;
   mProject->GetScrubber().PopulateMenu(rulerMenu);
   PopupMenu(&rulerMenu, pos);
}

void AdornedRulerPanel::OnToggleQuickPlay(wxCommandEvent&)
{
   mQuickPlayEnabled = (mQuickPlayEnabled)? false : true;
   gPrefs->Write(wxT("/QuickPlay/QuickPlayEnabled"), mQuickPlayEnabled);
   gPrefs->Flush();
   RegenerateTooltips();
}

void AdornedRulerPanel::OnSyncSelToQuickPlay(wxCommandEvent&)
{
   mPlayRegionDragsSelection = (mPlayRegionDragsSelection)? false : true;
   gPrefs->Write(wxT("/QuickPlay/DragSelection"), mPlayRegionDragsSelection);
   gPrefs->Flush();
}

void AdornedRulerPanel::DragSelection()
{
   if (mPlayRegionStart < mPlayRegionEnd) {
      mProject->SetSel0(mPlayRegionStart);
      mProject->SetSel1(mPlayRegionEnd);
   }
   else {
      mProject->SetSel0(mPlayRegionEnd);
      mProject->SetSel1(mPlayRegionStart);
   }
   mProject->GetTrackPanel()->DisplaySelection();
   mProject->GetTrackPanel()->Refresh(false);
}

void AdornedRulerPanel::HandleSnapping()
{
   if (!mSnapManager) {
      mSnapManager = new SnapManager(mTracks, mViewInfo);
   }

   bool snappedPoint, snappedTime;
   mIsSnapped = mSnapManager->Snap(NULL, mQuickPlayPos, false,
                                   &mQuickPlayPos, &snappedPoint, &snappedTime);
}

void AdornedRulerPanel::OnTimelineToolTips(wxCommandEvent&)
{
   mTimelineToolTip = (mTimelineToolTip)? false : true;
   gPrefs->Write(wxT("/QuickPlay/ToolTips"), mTimelineToolTip);
   gPrefs->Flush();
#if wxUSE_TOOLTIPS
   RegenerateTooltips();
#endif
}

void AdornedRulerPanel::OnAutoScroll(wxCommandEvent&)
{
   if (mViewInfo->bUpdateTrackIndicator)
      gPrefs->Write(wxT("/GUI/AutoScroll"), false);
   else
      gPrefs->Write(wxT("/GUI/AutoScroll"), true);
   mProject->UpdatePrefs();
   gPrefs->Flush();
}


void AdornedRulerPanel::OnLockPlayRegion(wxCommandEvent&)
{
   if (mProject->IsPlayRegionLocked())
      mProject->OnUnlockPlayRegion();
   else
      mProject->OnLockPlayRegion();
}


// Draws the horizontal <===>
void AdornedRulerPanel::DoDrawPlayRegion(wxDC * dc)
{
   double start, end;
   GetPlayRegion(&start, &end);

   if (start >= 0)
   {
      const int x1 = Time2Pos(start) + 1;
      const int x2 = Time2Pos(end);
      int y = mInner.y - TopMargin + mInner.height/2;

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
         r.width = std::max(0, x2-x1 - PLAY_REGION_TRIANGLE_SIZE*2);
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

   if (mShowScrubbing) {
      // Let's distinguish the scrubbing area by using the same gray as for
      // selected track control panel.
      AColor::Medium(&mBackDC, true);
      mBackDC.DrawRectangle(mScrubZone);
   }

   wxRect r = mOuter;
   r.width -= RightMargin;
   r.height -= BottomMargin;
   AColor::BevelTrackInfo( *dc, true, r );

   dc->SetPen( *wxBLACK_PEN );
   dc->DrawLine( mOuter.x,
                 mOuter.y + mOuter.height - 1,
                 mOuter.x + mOuter.width,
                 mOuter.y + mOuter.height - 1 );
}

void AdornedRulerPanel::DoDrawMarks(wxDC * dc, bool /*text */ )
{
   const double min = Pos2Time(0);
   const double hiddenMin = Pos2Time(0, true);
   const double max = Pos2Time(mInner.width);
   const double hiddenMax = Pos2Time(mInner.width, true);

   mRuler.SetTickColour( theTheme.Colour( clrTrackPanelText ) );
   mRuler.SetRange( min, max, hiddenMin, hiddenMax );
   mRuler.Draw( *dc );
}

void AdornedRulerPanel::DrawSelection()
{
   Refresh();
}

void AdornedRulerPanel::DoDrawSelection(wxDC * dc)
{
   // Draw selection
   const int p0 = 1 + max(0, Time2Pos(mViewInfo->selectedRegion.t0()));
   const int p1 = 2 + min(mInner.width, Time2Pos(mViewInfo->selectedRegion.t1()));

   dc->SetBrush( wxBrush( theTheme.Colour( clrRulerBackground )) );
   dc->SetPen(   wxPen(   theTheme.Colour( clrRulerBackground )) );

   wxRect r;
   r.x = p0;
   r.y = mInner.y;
   r.width = p1 - p0 - 1;
   r.height = mInner.height;
   dc->DrawRectangle( r );
}

int AdornedRulerPanel::GetRulerHeight()
{
   return GetRulerHeight(ReadScrubEnabledPref());
}

int AdornedRulerPanel::GetRulerHeight(bool showScrubBar)
{
   return RulerHeight + (showScrubBar ? ScrubHeight : 0);
}

void AdornedRulerPanel::SetLeftOffset(int offset)
{
   mLeftOffset = offset;
   mRuler.SetUseZoomInfo(offset, mViewInfo);
}

void AdornedRulerPanel::DoDrawCursor(wxDC * dc)
{
   const int x = Time2Pos(mViewInfo->selectedRegion.t0());

   // Draw cursor in ruler
   dc->DrawLine( x, mInner.y, x, mInner.y + mInner.height );
}

//
//This draws the little triangular indicator on the
//AdornedRulerPanel.
//
void AdornedRulerPanel::ClearIndicator()
{
   mIndType = -1;

   Refresh();
}

void AdornedRulerPanel::DrawIndicator( double time, bool rec )
{
   mIndTime = time;

   if (mIndTime < 0)
   {
      ClearIndicator();
      return;
   }

   mIndType = ( rec ? 0 : 1 );

   Refresh();
}

// Draws the play/recording position indicator.
void AdornedRulerPanel::DoDrawIndicator
   (wxDC * dc, double time, bool playing, int width, bool scrub)
{

   const int x = Time2Pos(time);
   AColor::IndicatorColor( dc, playing );

   wxPoint tri[ 3 ];
   if (scrub) {
      auto height = IndicatorHeightForWidth(width);
      const int IndicatorHalfWidth = width / 2;

      // Double headed, left-right
      tri[ 0 ].x = x - IndicatorOffset;
      tri[ 0 ].y = mScrubZone.y;
      tri[ 1 ].x = x - IndicatorOffset;
      tri[ 1 ].y = mScrubZone.y + height;
      tri[ 2 ].x = x - IndicatorHalfWidth;
      tri[ 2 ].y = mScrubZone.y + height / 2;
      dc->DrawPolygon( 3, tri );
      tri[ 0 ].x = tri[ 1 ].x = x + IndicatorOffset;
      tri[ 2 ].x = x + IndicatorHalfWidth;
      dc->DrawPolygon( 3, tri );
   }
   else {
      // Down pointing triangle
      auto height = IndicatorHeightForWidth(width);
      const int IndicatorHalfWidth = width / 2;
      tri[ 0 ].x = x - IndicatorHalfWidth;
      tri[ 0 ].y = mScrubZone.y;
      tri[ 1 ].x = x + IndicatorHalfWidth;
      tri[ 1 ].y = mScrubZone.y;
      tri[ 2 ].x = x;
      tri[ 2 ].y = mScrubZone.y + height;
      dc->DrawPolygon( 3, tri );
   }
}

void AdornedRulerPanel::DoEraseIndicator(wxDC *dc, int x)
{
   if (x >= 0) {
      // These dimensions are always sufficient, even if a little
      // excessive for the small triangle:
      const int width = IndicatorBigWidth();
      const auto height = IndicatorHeightForWidth(width);

      const int indsize = width / 2;

      // Restore the background, but make it a little oversized to make
      // it happy OSX.
      dc->Blit(x - indsize - 1,
               mScrubZone.y - 1,
               indsize * 2 + 1 + 2,
               mScrubZone.y + height + 2,
               &mBackDC,
               x - indsize - 1,
               0);
   }
}

QuickPlayIndicatorOverlay *AdornedRulerPanel::GetOverlay()
{
   if (!mOverlay) {
      TrackPanel *tp = mProject->GetTrackPanel();
      mOverlay = std::make_unique<QuickPlayIndicatorOverlay>(mProject);
      tp->AddOverlay(mOverlay.get());
   }
   return mOverlay.get();
}

// Draws the vertical line and green triangle indicating the Quick Play cursor position.
void AdornedRulerPanel::DrawQuickPlayIndicator(wxDC * dc)
{
   double latestEnd = std::max(mTracks->GetEndTime(), mProject->GetSel1());
   if (dc == NULL || (mQuickPlayPos >= latestEnd)) {
      mLastQuickPlayX = -1;
      GetOverlay()->Update(-1);
      return;
   }

   const int x = Time2Pos(mQuickPlayPos);
   bool previewScrub =
      mPrevInScrubZone &&
      !mProject->GetScrubber().IsScrubbing();
   GetOverlay()->Update(x, mIsSnapped, previewScrub);

   DoEraseIndicator(dc, mLastQuickPlayX);
   mLastQuickPlayX = x;

   auto scrub = mPrevInScrubZone || mProject->GetScrubber().HasStartedScrubbing();
   auto width = scrub ? IndicatorBigWidth() : IndicatorSmallWidth;
   DoDrawIndicator(dc, mQuickPlayPos, true, width, scrub);
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
   ControlToolBar* ctb = mProject->GetControlToolBar();
   ctb->StopPlaying();

   mPlayRegionStart = -1;
   mPlayRegionEnd = -1;
   mQuickPlayInd = false;

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
   mRuler.GetMaxSize(width, height);
}
