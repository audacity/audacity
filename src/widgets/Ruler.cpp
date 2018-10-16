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

\class AdornedRulerPanel
\brief This is an Audacity Specific ruler panel which additionally
  has border, selection markers, play marker.
  
  Once TrackPanel uses wxSizers, we will derive it from some
  wxWindow and the GetSize and SetSize functions
  will then be wxWidgets functions instead.

*//***************************************************************//**

\class Ruler::Label
\brief An array of these created by the Ruler is used to determine
what and where text annotations to the numbers on the Ruler get drawn.

\todo Check whether Ruler is costing too much time in allocation/free of
array of Ruler::Label.

*//******************************************************************/

#include "../Audacity.h"
#include "Ruler.h"

#include <math.h>

#include <wx/app.h>
#include <wx/dcscreen.h>
#include <wx/dcmemory.h>
#include <wx/dcbuffer.h>
#include <wx/settings.h>
#include <wx/menu.h>
#include <wx/menuitem.h>
#include <wx/tooltip.h>

#include "AButton.h"
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
#include "../Menus.h"
#include "../NumberScale.h"
#include "../Prefs.h"
#include "../Snap.h"
#include "../UIHandle.h"
#include "../tracks/ui/Scrubbing.h"
#include "../prefs/PlaybackPrefs.h"
#include "../prefs/TracksPrefs.h"
#include "../prefs/TracksBehaviorsPrefs.h"
#include "../widgets/Grabber.h"
#include "../commands/CommandContext.h"

//#define SCRUB_ABOVE

using std::min;
using std::max;

#define SELECT_TOLERANCE_PIXEL 4

#define PLAY_REGION_TRIANGLE_SIZE 6
#define PLAY_REGION_RECT_WIDTH 1
#define PLAY_REGION_RECT_HEIGHT 3
#define PLAY_REGION_GLOBAL_OFFSET_Y 7

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
   mUnits = wxT("");

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

   if (mUnits != wxT(""))
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
               if( !major && mMinorLabels[mNumMinor-1].text.IsEmpty() ){
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
         mMinorLabels[i].Draw(*mDC, mTwoTone, mTickColour);
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
      mMajorLabels[i].text = label->Item(i);
      mMajorLabels[i].pos  = start + i*step;
   }
   //Remember: DELETE majorlabels....
}

void Ruler::SetCustomMinorLabels(wxArrayString *label, size_t numLabel, int start, int step)
{
   mNumMinor = numLabel;
   mMinorLabels.reinit(numLabel);

   for(size_t i = 0; i<numLabel; i++) {
      mMinorLabels[i].text = label->Item(i);
      mMinorLabels[i].pos  = start + i*step;
   }
   //Remember: DELETE majorlabels....
}

void Ruler::Label::Draw(wxDC&dc, bool twoTone, wxColour c) const
{
   if (text != wxT("")) {
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


/*********************************************************************/
enum : int {
   IndicatorSmallWidth = 9,
   IndicatorMediumWidth = 13,
   IndicatorOffset = 1,

   TopMargin = 1,
   BottomMargin = 2, // for bottom bevel and bottom line
   LeftMargin = 1, 

   RightMargin = 1,
};

enum {
   ScrubHeight = 14,
   ProperRulerHeight = 29
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

inline int IndicatorBigHeight()
{
   return std::max((int)(ScrubHeight - TopMargin),
                   (int)(IndicatorMediumWidth));
}

inline int IndicatorBigWidth()
{
   return IndicatorWidthForHeight(IndicatorBigHeight());
}

/**********************************************************************

QuickPlayRulerOverlay.
Graphical helper for AdornedRulerPanel.

**********************************************************************/

class QuickPlayIndicatorOverlay;

// This is an overlay drawn on the ruler.  It draws the little triangle or
// the double-headed arrow.
class AdornedRulerPanel::QuickPlayRulerOverlay final : public Overlay
{
public:
   QuickPlayRulerOverlay(QuickPlayIndicatorOverlay &partner);
   virtual ~QuickPlayRulerOverlay();

   // Available to this and to partner

   int mNewQPIndicatorPos { -1 };
   bool mNewQPIndicatorSnapped {};
   bool mNewPreviewingScrub {};

   bool mNewScrub {};
   bool mNewSeek {};

   void Update();

private:
   AdornedRulerPanel *GetRuler() const;

   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

   QuickPlayIndicatorOverlay &mPartner;

   // Used by this only
   int mOldQPIndicatorPos { -1 };
   bool mOldScrub {};
   bool mOldSeek {};
};

/**********************************************************************

 QuickPlayIndicatorOverlay.
 Graphical helper for AdornedRulerPanel.

 **********************************************************************/

// This is an overlay drawn on a different window, the track panel.
// It draws the pale guide line that follows mouse movement.
class AdornedRulerPanel::QuickPlayIndicatorOverlay final : public Overlay
{
   friend QuickPlayRulerOverlay;

public:
   QuickPlayIndicatorOverlay(AudacityProject *project);

   virtual ~QuickPlayIndicatorOverlay();

private:
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

   AudacityProject *mProject;

   std::unique_ptr<QuickPlayRulerOverlay> mPartner
      { std::make_unique<QuickPlayRulerOverlay>(*this) };

   int mOldQPIndicatorPos { -1 };
   bool mOldQPIndicatorSnapped {};
   bool mOldPreviewingScrub {};
};

/**********************************************************************

 Implementation of QuickPlayRulerOverlay.

 **********************************************************************/

AdornedRulerPanel::QuickPlayRulerOverlay::QuickPlayRulerOverlay(
   QuickPlayIndicatorOverlay &partner)
: mPartner(partner)
{
   GetRuler()->AddOverlay(this);
}

AdornedRulerPanel::QuickPlayRulerOverlay::~QuickPlayRulerOverlay()
{
   auto ruler = GetRuler();
   if (ruler)
      ruler->RemoveOverlay(this);
}

AdornedRulerPanel *AdornedRulerPanel::QuickPlayRulerOverlay::GetRuler() const
{
   return mPartner.mProject->GetRulerPanel();
}

void AdornedRulerPanel::QuickPlayRulerOverlay::Update()
{
   const auto project = mPartner.mProject;
   auto ruler = GetRuler();

   // Hide during transport, or if mouse is not in the ruler, unless scrubbing
   if ((!ruler->LastCell() || project->IsAudioActive())
       && (!project->GetScrubber().IsScrubbing() || project->GetScrubber().IsSpeedPlaying()))
      mNewQPIndicatorPos = -1;
   else {
      double latestEnd =
         std::max(ruler->mTracks->GetEndTime(), project->GetSel1());
      if (ruler->mQuickPlayPos >= latestEnd)
         mNewQPIndicatorPos = -1;
      else {
         // This will determine the x coordinate of the line and of the
         // ruler indicator
         mNewQPIndicatorPos = ruler->Time2Pos(ruler->mQuickPlayPos);

         // These determine which shape is drawn on the ruler, and whether
         // in the scrub or the qp zone
         const auto &scrubber = mPartner.mProject->GetScrubber();
         mNewScrub =
            ruler->mMouseEventState == AdornedRulerPanel::mesNone &&
            (ruler->LastCell() == ruler->mScrubbingCell ||
             (scrubber.HasMark()));
         mNewSeek = mNewScrub &&
            (scrubber.Seeks() || scrubber.TemporarilySeeks());

         // These two will determine the color of the line stroked over
         // the track panel, green for scrub or yellow for snapped or white
         mNewPreviewingScrub =
            ruler->LastCell() == ruler->mScrubbingCell &&
            !project->GetScrubber().IsScrubbing();
         mNewQPIndicatorSnapped = ruler->mIsSnapped;
      }
   }
}

std::pair<wxRect, bool>
AdornedRulerPanel::QuickPlayRulerOverlay::DoGetRectangle(wxSize /*size*/)
{
   Update();

   const auto x = mOldQPIndicatorPos;
   if (x >= 0) {
      // These dimensions are always sufficient, even if a little
      // excessive for the small triangle:
      const int width = IndicatorBigWidth() * 3 / 2;
      //const auto height = IndicatorHeightForWidth(width);

      const int indsize = width / 2;

      auto xx = x - indsize;
      auto yy = 0;
      return {
         { xx, yy,
            indsize * 2 + 1,
            GetRuler()->GetSize().GetHeight() },
         (x != mNewQPIndicatorPos
          || (mOldScrub != mNewScrub)
          || (mOldSeek != mNewSeek) )
      };
   }
   else
      return { {}, mNewQPIndicatorPos >= 0 };
}

void AdornedRulerPanel::QuickPlayRulerOverlay::Draw(
   OverlayPanel & /*panel*/, wxDC &dc)
{
   mOldQPIndicatorPos = mNewQPIndicatorPos;
   mOldScrub = mNewScrub;
   mOldSeek = mNewSeek;
   if (mOldQPIndicatorPos >= 0) {
      auto ruler = GetRuler();
      auto width = mOldScrub ? IndicatorBigWidth() : IndicatorSmallWidth;
      ruler->DoDrawIndicator(
         &dc, mOldQPIndicatorPos, true, width, mOldScrub, mOldSeek);
   }
}

/**********************************************************************

 Implementation of QuickPlayIndicatorOverlay.

 **********************************************************************/

AdornedRulerPanel::QuickPlayIndicatorOverlay::QuickPlayIndicatorOverlay(
   AudacityProject *project)
   : mProject(project)
{
   auto tp = mProject->GetTrackPanel();
   tp->AddOverlay(this);
}

AdornedRulerPanel::QuickPlayIndicatorOverlay::~QuickPlayIndicatorOverlay()
{
   auto tp = mProject->GetTrackPanel();
   if (tp)
      tp->RemoveOverlay(this);
}

std::pair<wxRect, bool>
AdornedRulerPanel::QuickPlayIndicatorOverlay::DoGetRectangle(wxSize size)
{
   mPartner->Update();

   wxRect rect(mOldQPIndicatorPos, 0, 1, size.GetHeight());
   return std::make_pair(
      rect,
      (mOldQPIndicatorPos != mPartner->mNewQPIndicatorPos ||
       mOldQPIndicatorSnapped != mPartner->mNewQPIndicatorSnapped ||
       mOldPreviewingScrub != mPartner->mNewPreviewingScrub)
   );
}

void AdornedRulerPanel::QuickPlayIndicatorOverlay::Draw(
   OverlayPanel &panel, wxDC &dc)
{
   mOldQPIndicatorPos = mPartner->mNewQPIndicatorPos;
   mOldQPIndicatorSnapped = mPartner->mNewQPIndicatorSnapped;
   mOldPreviewingScrub = mPartner->mNewPreviewingScrub;

   if (mOldQPIndicatorPos >= 0) {
      mOldPreviewingScrub
      ? AColor::IndicatorColor(&dc, true) // Draw green line for preview.
      : mOldQPIndicatorSnapped
        ? AColor::SnapGuidePen(&dc)
        : AColor::Light(&dc, false)
      ;

      // Draw indicator in all visible tracks
      for ( const auto &data : static_cast<TrackPanel&>(panel).Cells() )
      {
         Track *const pTrack = dynamic_cast<Track*>(data.first.get());
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
#include "../RefreshCode.h"
#include "../TrackPanelMouseEvent.h"

enum {
   OnToggleQuickPlayID = 7000,
   OnSyncQuickPlaySelID,
   OnTimelineToolTipID,
   OnAutoScrollID,
   OnLockPlayRegionID,
   OnScrubRulerID,

   OnTogglePinnedStateID,
};

BEGIN_EVENT_TABLE(AdornedRulerPanel, CellularPanel)
   EVT_PAINT(AdornedRulerPanel::OnPaint)
   EVT_SIZE(AdornedRulerPanel::OnSize)

   // Context menu commands
   EVT_MENU(OnToggleQuickPlayID, AdornedRulerPanel::OnToggleQuickPlay)
   EVT_MENU(OnSyncQuickPlaySelID, AdornedRulerPanel::OnSyncSelToQuickPlay)
   EVT_MENU(OnTimelineToolTipID, AdornedRulerPanel::OnTimelineToolTips)
   EVT_MENU(OnAutoScrollID, AdornedRulerPanel::OnAutoScroll)
   EVT_MENU(OnLockPlayRegionID, AdornedRulerPanel::OnLockPlayRegion)
   EVT_MENU(OnScrubRulerID, AdornedRulerPanel::OnToggleScrubRulerFromMenu)

   EVT_COMMAND( OnTogglePinnedStateID,
               wxEVT_COMMAND_BUTTON_CLICKED,
               AdornedRulerPanel::OnTogglePinnedState )

END_EVENT_TABLE()

class AdornedRulerPanel::CommonCell : public TrackPanelCell
{
public:
   explicit
   CommonCell( AdornedRulerPanel *parent, MenuChoice menuChoice )
   : mParent{ parent }
   , mMenuChoice{ menuChoice }
   {}
   
   HitTestPreview DefaultPreview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
      override
   {
      (void)pProject;// Compiler food
      (void)state;// Compiler food
      // May come here when recording is in progress, so hit tests are turned
      // off.
      wxString tooltip;
      if (mParent->mTimelineToolTip)
         tooltip = _("Timeline actions disabled during recording");

      static wxCursor cursor{ wxCURSOR_DEFAULT };
      return {
         {},
         &cursor,
         tooltip,
      };
   }

   unsigned DoContextMenu
      (const wxRect &rect,
       wxWindow *pParent, wxPoint *pPosition) final override
   {
      (void)pParent;// Compiler food
      (void)rect;// Compiler food
      mParent->ShowContextMenu(mMenuChoice, pPosition);
      return 0;
   }

protected:
   AdornedRulerPanel *mParent;
   const MenuChoice mMenuChoice;
};

class AdornedRulerPanel::CommonRulerHandle : public UIHandle
{
public:
   explicit
   CommonRulerHandle(
      AdornedRulerPanel *pParent, wxCoord xx, MenuChoice menuChoice )
      : mParent(pParent)
      , mX( xx )
      , mChoice( menuChoice )
   {}

   bool Clicked() const { return mClicked != Button::None; }

   static UIHandle::Result NeedChangeHighlight
   (const CommonRulerHandle &oldState, const CommonRulerHandle &newState)
   {
      if (oldState.mX != newState.mX)
         return RefreshCode::DrawOverlays;
      return 0;
   }

protected:
   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *) override
   {
      mClicked = event.event.LeftIsDown() ? Button::Left : Button::Right;
      return RefreshCode::DrawOverlays;
   }

   Result Drag
      (const TrackPanelMouseEvent &, AudacityProject *) override
   {
      return RefreshCode::DrawOverlays;
   }

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *,
       wxWindow *) override
   {
      if ( mParent && mClicked == Button::Right ) {
         const auto pos = event.event.GetPosition();
         mParent->ShowContextMenu( mChoice, &pos );
      }
      return RefreshCode::DrawOverlays;
   }

   Result Cancel(AudacityProject *pProject) override
   {
      (void)pProject;// Compiler food
      return RefreshCode::DrawOverlays;
   }
   
   void Enter(bool) override
   {
      mChangeHighlight = RefreshCode::DrawOverlays;
   }

   wxWeakRef<AdornedRulerPanel> mParent;

   wxCoord mX;
   
   MenuChoice mChoice;

   enum class Button { None, Left, Right };
   Button mClicked{ Button::None };
};

class AdornedRulerPanel::QPHandle final : public CommonRulerHandle
{
public:
   explicit
   QPHandle( AdornedRulerPanel *pParent, wxCoord xx )
   : CommonRulerHandle( pParent, xx, MenuChoice::QuickPlay )
   {
   }
   
   std::unique_ptr<SnapManager> mSnapManager;

private:
   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
   override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   SelectedRegion mOldSelection;
};

namespace
{

wxCoord GetPlayHeadX( const AudacityProject *pProject )
{
   const TrackPanel *tp = pProject->GetTrackPanel();
   int width;
   tp->GetTracksUsableArea(&width, NULL);
   return tp->GetLeftOffset()
      + width * TracksPrefs::GetPinnedHeadPositionPreference();
}

double GetPlayHeadFraction( const AudacityProject *pProject, wxCoord xx )
{
   const TrackPanel *tp = pProject->GetTrackPanel();
   int width;
   tp->GetTracksUsableArea(&width, NULL);
   auto fraction = (xx - tp->GetLeftOffset()) / double(width);
   return std::max(0.0, std::min(1.0, fraction));
}

// Handle for dragging the pinned play head, which so far does not need
// to be a friend of the AdornedRulerPanel class, so we don't make it nested.
class PlayheadHandle : public UIHandle
{
public:
   explicit
   PlayheadHandle( wxCoord xx )
      : mX( xx )
   {}

   static UIHandle::Result NeedChangeHighlight
   (const PlayheadHandle &oldState, const PlayheadHandle &newState)
   {
      if (oldState.mX != newState.mX)
         return RefreshCode::DrawOverlays;
      return 0;
   }
   
   static std::shared_ptr<PlayheadHandle>
   HitTest( const AudacityProject *pProject, wxCoord xx )
   {
      if( ControlToolBar::IsTransportingPinned() &&
          pProject->IsAudioActive() )
      {
         const auto targetX = GetPlayHeadX( pProject );
         if ( abs( xx - targetX ) <= SELECT_TOLERANCE_PIXEL )
            return std::make_shared<PlayheadHandle>( xx );
      }
      return {};
   }
   
protected:
   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {
      (void)pProject;// Compiler food
      if (event.event.LeftDClick()) {
         // Restore default position on double click
         TracksPrefs::SetPinnedHeadPositionPreference( 0.5, true );
      
         return RefreshCode::DrawOverlays |
            // Do not start a drag
            RefreshCode::Cancelled;
      }

      mOrigPreference = TracksPrefs::GetPinnedHeadPositionPreference();
      return 0;
   }

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {
      auto value = GetPlayHeadFraction(pProject, event.event.m_x );
      TracksPrefs::SetPinnedHeadPositionPreference( value );
      return RefreshCode::DrawOverlays;
   }

   HitTestPreview Preview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
      override
   {
      (void)pProject;// Compiler food
      (void)state;// Compiler food

      static wxCursor cursor{ wxCURSOR_SIZEWE };
      return {
         _( "Click and drag to adjust, double-click to reset" ),
         &cursor,
         _( "Record/Play head" )
      };
   }

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *) override
   {
      auto value = GetPlayHeadFraction(pProject, event.event.m_x );
      TracksPrefs::SetPinnedHeadPositionPreference( value, true );
      return RefreshCode::DrawOverlays;
   }

   Result Cancel(AudacityProject *pProject) override
   {
      (void)pProject;// Compiler food
      TracksPrefs::SetPinnedHeadPositionPreference( mOrigPreference );
      return RefreshCode::DrawOverlays;
   }
   
   void Enter(bool) override
   {
      mChangeHighlight = RefreshCode::DrawOverlays;
   }

   wxCoord mX;
   double mOrigPreference {};
};

}

class AdornedRulerPanel::QPCell final : public CommonCell
{
public:
   explicit
   QPCell( AdornedRulerPanel *parent )
   : AdornedRulerPanel::CommonCell{ parent, MenuChoice::QuickPlay }
   {}
   
   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;
   
   // Return shared_ptr to self, stored in parent
   std::shared_ptr<TrackPanelCell> ContextMenuDelegate() override
      { return mParent->mQPCell; }

   bool Hit() const { return !mHolder.expired(); }
   bool Clicked() const {
      if (auto ptr = mHolder.lock())
         return ptr->Clicked();
      return false;
   }
   
   std::weak_ptr<QPHandle> mHolder;
   std::weak_ptr<PlayheadHandle> mPlayheadHolder;
};

std::vector<UIHandlePtr> AdornedRulerPanel::QPCell::HitTest
(const TrackPanelMouseState &state,
 const AudacityProject *pProject)
{
   // Creation of overlays on demand here -- constructor of AdornedRulerPanel
   // is too early to do it
   mParent->CreateOverlays();
   
   std::vector<UIHandlePtr> results;
   auto xx = state.state.m_x;

#ifdef EXPERIMENTAL_DRAGGABLE_PLAY_HEAD
   {
      // Allow click and drag on the play head even while recording
      // Make this handle more prominent then the quick play handle
      auto result = PlayheadHandle::HitTest( pProject, xx );
      if (result) {
         result = AssignUIHandlePtr( mPlayheadHolder, result );
         results.push_back( result );
      }
   }
#endif
   
   // Disable mouse actions on Timeline while recording.
   if (!mParent->mIsRecording) {

   mParent->UpdateQuickPlayPos( xx, state.state.ShiftDown() );

   auto result = std::make_shared<QPHandle>( mParent, xx );
   result = AssignUIHandlePtr( mHolder, result );
   results.push_back( result );

   }

   return results;
}

class AdornedRulerPanel::ScrubbingHandle final : public CommonRulerHandle
{
public:
   explicit
   ScrubbingHandle( AdornedRulerPanel *pParent, wxCoord xx )
   : CommonRulerHandle( pParent, xx, MenuChoice::Scrub )
   {
   }

private:
   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {
      auto result = CommonRulerHandle::Click(event, pProject);
      if (!( result & RefreshCode::Cancelled )) {
         if (mClicked == Button::Left) {
            auto &scrubber = pProject->GetScrubber();
            // only if scrubbing is allowed now
            bool canScrub =
               scrubber.CanScrub() &&
               mParent &&
               mParent->mShowScrubbing;

            if (!canScrub)
               return RefreshCode::Cancelled;
            if (!scrubber.HasMark()) {
               // Asynchronous scrub poller gets activated here
               scrubber.MarkScrubStart(
                  event.event.m_x, Scrubber::ShouldScrubPinned(), false);
            }
         }
      }
      return result;
   }

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {
      auto result = CommonRulerHandle::Drag(event, pProject);
      if (!( result & RefreshCode::Cancelled )) {
         // Nothing needed here.  The scrubber works by polling mouse state
         // after the start has been marked.
      }
      return result;
   }

   HitTestPreview Preview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
   override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override {
      auto result = CommonRulerHandle::Release(event, pProject, pParent);
      if (!( result & RefreshCode::Cancelled )) {
         // Nothing needed here either.  The scrub poller may have decided to
         // seek because a drag happened before button up, or it may decide
         // to start a scrub, as it watches mouse movement after the button up.
      }
      return result;
   }

   Result Cancel(AudacityProject *pProject) override
   {
      auto result = CommonRulerHandle::Cancel(pProject);

      if (mClicked == Button::Left) {
         auto &scrubber = pProject->GetScrubber();
         scrubber.Cancel();
         
         auto ctb = pProject->GetControlToolBar();
         wxCommandEvent evt;
         ctb->OnStop(evt);
      }

      return result;
   }
};

class AdornedRulerPanel::ScrubbingCell final : public CommonCell
{
public:
   explicit
   ScrubbingCell( AdornedRulerPanel *parent )
   : AdornedRulerPanel::CommonCell{ parent, MenuChoice::Scrub }
   {}
   
   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;
   
   // Return shared_ptr to self, stored in parent
   std::shared_ptr<TrackPanelCell> ContextMenuDelegate() override
      { return mParent->mScrubbingCell; }
   
   bool Hit() const { return !mHolder.expired(); }
   bool Clicked() const {
      if (auto ptr = mHolder.lock())
         return ptr->Clicked();
      return false;
   }
   
private:
   std::weak_ptr<ScrubbingHandle> mHolder;
};

std::vector<UIHandlePtr> AdornedRulerPanel::ScrubbingCell::HitTest
(const TrackPanelMouseState &state,
 const AudacityProject *pProject)
{
   (void)pProject;// Compiler food
   // Creation of overlays on demand here -- constructor of AdornedRulerPanel
   // is too early to do it
   mParent->CreateOverlays();
   
   std::vector<UIHandlePtr> results;
   
   // Disable mouse actions on Timeline while recording.
   if (!mParent->mIsRecording) {

   auto xx = state.state.m_x;
   mParent->UpdateQuickPlayPos( xx, state.state.ShiftDown() );
   auto result = std::make_shared<ScrubbingHandle>( mParent, xx );
   result = AssignUIHandlePtr( mHolder, result );
   results.push_back( result );

   }
   
   return results;
}

AdornedRulerPanel::AdornedRulerPanel(AudacityProject* project,
                                     wxWindow *parent,
                                     wxWindowID id,
                                     const wxPoint& pos,
                                     const wxSize& size,
                                     ViewInfo *viewinfo)
:  CellularPanel(parent, id, pos, size, viewinfo)
, mProject(project)
{
   mQPCell = std::make_shared<QPCell>( this );
   mScrubbingCell = std::make_shared<ScrubbingCell>( this );
   
   for (auto &button : mButtons)
      button = nullptr;

   SetLabel( _("Timeline") );
   SetName(GetLabel());
   SetBackgroundStyle(wxBG_STYLE_PAINT);

   mLeftOffset = 0;
   mIndTime = -1;

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

   mTracks = project->GetTracks();

   mIsSnapped = false;

   mIsRecording = false;

   mTimelineToolTip = !!gPrefs->Read(wxT("/QuickPlay/ToolTips"), 1L);
   mPlayRegionDragsSelection = (gPrefs->Read(wxT("/QuickPlay/DragSelection"), 0L) == 1)? true : false; 
   mQuickPlayEnabled = !!gPrefs->Read(wxT("/QuickPlay/QuickPlayEnabled"), 1L);

#if wxUSE_TOOLTIPS
   wxToolTip::Enable(true);
#endif

   wxTheApp->Bind(EVT_AUDIOIO_CAPTURE,
                     &AdornedRulerPanel::OnRecordStartStop,
                     this);
}

AdornedRulerPanel::~AdornedRulerPanel()
{
}

#if 1
namespace {
   static const wxChar *scrubEnabledPrefName = wxT("/QuickPlay/ScrubbingEnabled");

   bool ReadScrubEnabledPref()
   {
      bool result {};
// DA: Scrub is disabled by default.
#ifdef EXPERIMENTAL_DA
      gPrefs->Read(scrubEnabledPrefName, &result, false);
#else
      gPrefs->Read(scrubEnabledPrefName, &result, true);
#endif
      return result;
   }

   void WriteScrubEnabledPref(bool value)
   {
      gPrefs->Write(scrubEnabledPrefName, value);
   }
}
#endif

void AdornedRulerPanel::UpdatePrefs()
{
   // Update button texts for language change
   UpdateButtonStates();

#ifdef EXPERIMENTAL_SCROLLING_LIMITS
#ifdef EXPERIMENTAL_TWO_TONE_TIME_RULER
   {
      bool scrollBeyondZero = false;
      gPrefs->Read(TracksBehaviorsPrefs::ScrollingPreferenceKey(), &scrollBeyondZero,
                   TracksBehaviorsPrefs::ScrollingPreferenceDefault());
      mRuler.SetTwoTone(scrollBeyondZero);
   }
#endif
#endif

   mShowScrubbing = ReadScrubEnabledPref();
   // Affected by the last
   UpdateRects();
   SetPanelSize();

   RegenerateTooltips();
}

void AdornedRulerPanel::ReCreateButtons()
{
   // TODO: Should we do this to destroy the grabber??
   // Get rid of any children we may have
   // DestroyChildren();

   SetBackgroundColour(theTheme.Colour( clrMedium ));

   for (auto & button : mButtons) {
      if (button)
         button->Destroy();
      button = nullptr;
   }

   size_t iButton = 0;
   // Make the short row of time ruler pushbottons.
   // Don't bother with sizers.  Their sizes and positions are fixed.
   // Add a grabber converted to a spacer.
   // This makes it visually clearer that the button is a button.

   wxPoint position( 1, 0 );

   Grabber * pGrabber = safenew Grabber(this, this->GetId());
   pGrabber->SetAsSpacer( true );
   //pGrabber->SetSize( 10, 27 ); // default is 10,27
   pGrabber->SetPosition( position );

   position.x = 12;

   auto size = theTheme.ImageSize( bmpRecoloredUpSmall );
   size.y = std::min(size.y, GetRulerHeight(false));

   auto buttonMaker = [&]
   (wxWindowID id, teBmps bitmap, bool toggle)
   {
      const auto button =
      ToolBar::MakeButton(
         this,
         bmpRecoloredUpSmall, bmpRecoloredDownSmall, 
         bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall, 
         bitmap, bitmap, bitmap,
         id, position, toggle, size
      );

      position.x += size.GetWidth();
      mButtons[iButton++] = button;
      return button;
   };
   auto button = buttonMaker(OnTogglePinnedStateID, bmpPlayPointerPinned, true);
   ToolBar::MakeAlternateImages(
	   *button, 3,
	   bmpRecoloredUpSmall, bmpRecoloredDownSmall,
	   bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall,
	   //bmpUnpinnedPlayHead, bmpUnpinnedPlayHead, bmpUnpinnedPlayHead,
	   bmpRecordPointer, bmpRecordPointer, bmpRecordPointer,
	   size);
   ToolBar::MakeAlternateImages(
	   *button, 2,
	   bmpRecoloredUpSmall, bmpRecoloredDownSmall,
	   bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall,
	   //bmpUnpinnedPlayHead, bmpUnpinnedPlayHead, bmpUnpinnedPlayHead,
	   bmpRecordPointerPinned, bmpRecordPointerPinned, bmpRecordPointerPinned,
	   size);
   ToolBar::MakeAlternateImages(
      *button, 1,
      bmpRecoloredUpSmall, bmpRecoloredDownSmall, 
      bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall, 
      //bmpUnpinnedPlayHead, bmpUnpinnedPlayHead, bmpUnpinnedPlayHead,
      bmpPlayPointer, bmpPlayPointer, bmpPlayPointer,
      size);

   UpdateButtonStates();
}

void AdornedRulerPanel::InvalidateRuler()
{
   mRuler.Invalidate();
}

namespace {
   const wxString StartScrubbingMessage(const Scrubber &/*scrubber*/)
   {
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
#if 0
      if(scrubber.Seeks())
         return _("Click or drag to begin Seek");
      else
         return _("Click or drag to begin Scrub");
#else
      return _("Click & move to Scrub. Click & drag to Seek.");
#endif
   }

   const wxString ContinueScrubbingMessage(
      const Scrubber &scrubber, bool clicked)
   {
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
#if 0
      if(scrubber.Seeks())
         return _("Move to Seek");
      else
         return _("Move to Scrub");
#else
      if( clicked ) {
         // Since mouse is down, mention dragging first.
         // IsScrubbing is true if Scrubbing OR seeking.
         if( scrubber.IsScrubbing() )
            // User is dragging already, explain.
            return _("Drag to Seek. Release to stop seeking.");
         else
            // User has clicked but not yet moved or released.
            return _("Drag to Seek. Release and move to Scrub.");
      }
      // Since mouse is up, mention moving first.
      return _("Move to Scrub. Drag to Seek.");
#endif
   }

   const wxString ScrubbingMessage(const Scrubber &scrubber, bool clicked)
   {
      if (scrubber.HasMark())
         return ContinueScrubbingMessage(scrubber, clicked);
      else
         return StartScrubbingMessage(scrubber);
   }
}

void AdornedRulerPanel::RegenerateTooltips()
{
   CallAfter( [this]{ HandleCursorForPresentMouseState(); } );
}

void AdornedRulerPanel::OnRecordStartStop(wxCommandEvent & evt)
{
   evt.Skip();

   if (evt.GetInt() != 0)
   {
      mIsRecording = true;
      this->CellularPanel::CancelDragging( false );
      this->CellularPanel::ClearTargets();

      UpdateButtonStates();
   }
   else {
      mIsRecording = false;
      UpdateButtonStates();
   }
   
   RegenerateTooltips();
}

void AdornedRulerPanel::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   if (mNeedButtonUpdate) {
      // Visit this block once only in the lifetime of this panel
      mNeedButtonUpdate = false;
      // Do this first time setting of button status texts
      // when we are sure the CommandManager is initialized.
      ReCreateButtons();
      // Sends a resize event, which will cause a second paint.
      UpdatePrefs();
   }

   wxPaintDC dc(this);

   auto &backDC = GetBackingDCForRepaint();

   DoDrawBackground(&backDC);

   if (!mViewInfo->selectedRegion.isPoint())
   {
      DoDrawSelection(&backDC);
   }

   DoDrawMarks(&backDC, true);

   DoDrawPlayRegion(&backDC);

   DoDrawEdge(&backDC);

   DisplayBitmap(dc);

   // Stroke extras direct to the client area,
   // maybe outside of the damaged area
   // As with TrackPanel, do not make a NEW wxClientDC or else Mac flashes badly!
   dc.DestroyClippingRegion();
   DrawOverlays(true, &dc);
}

void AdornedRulerPanel::OnSize(wxSizeEvent &evt)
{
   mOuter = GetClientRect();
   if (mOuter.GetWidth() == 0 || mOuter.GetHeight() == 0)
   {
      return;
   }

   UpdateRects();

   OverlayPanel::OnSize(evt);
}

void AdornedRulerPanel::UpdateRects()
{
   mInner = mOuter;
   mInner.x += LeftMargin;
   mInner.width -= (LeftMargin + RightMargin);

   auto top = &mInner;
   auto bottom = &mInner;

   if (mShowScrubbing) {
      mScrubZone = mInner;
      auto scrubHeight = std::min(mScrubZone.height, (int)(ScrubHeight));

      int topHeight;
#ifdef SCRUB_ABOVE
      top = &mScrubZone, topHeight = scrubHeight;
#else
      auto qpHeight = mScrubZone.height - scrubHeight;
      bottom = &mScrubZone, topHeight = qpHeight;
      // Increase scrub zone height so that hit testing finds it and
      // not QP region, when on bottom 'edge'.
      mScrubZone.height+=BottomMargin;
#endif

      top->height = topHeight;
      bottom->height -= topHeight;
      bottom->y += topHeight;
   }

   top->y += TopMargin;
   top->height -= TopMargin;

   bottom->height -= BottomMargin;

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

auto AdornedRulerPanel::QPHandle::Click
(const TrackPanelMouseEvent &event, AudacityProject *pProject) -> Result
{
   auto result = CommonRulerHandle::Click(event, pProject);
   if (!( result & RefreshCode::Cancelled )) {
      if (mClicked == Button::Left) {
         if (!(mParent && mParent->mQuickPlayEnabled))
            return RefreshCode::Cancelled;

         auto &scrubber = pProject->GetScrubber();
         if(scrubber.HasMark()) {
            // We can't stop scrubbing yet (see comments in Bug 1391),
            // but we can pause it.
            GetMenuCommandHandler(*pProject).OnPause(*pProject);
         }

         // Store the initial play region state
         mParent->mOldPlayRegionStart = mParent->mPlayRegionStart;
         mParent->mOldPlayRegionEnd =   mParent->mPlayRegionEnd;
         mParent->mPlayRegionLock =     mParent->mProject->IsPlayRegionLocked();

         // Save old selection, in case drag of selection is cancelled
         mOldSelection = pProject->GetViewInfo().selectedRegion;

         mParent->HandleQPClick( event.event, mX );
         mParent->HandleQPDrag( event.event, mX );
      }
   }
   
   return result;
}

void AdornedRulerPanel::HandleQPClick(wxMouseEvent &evt, wxCoord mousePosX)
{
   // Temporarily unlock locked play region
   if (mPlayRegionLock && evt.LeftDown()) {
      //mPlayRegionLock = true;
      GetMenuCommandHandler(*mProject).OnUnlockPlayRegion(*mProject);
   }

   mLeftDownClickUnsnapped = mQuickPlayPosUnsnapped;
   mLeftDownClick = mQuickPlayPos;
   bool isWithinStart = IsWithinMarker(mousePosX, mOldPlayRegionStart);
   bool isWithinEnd = IsWithinMarker(mousePosX, mOldPlayRegionEnd);

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
}

auto AdornedRulerPanel::QPHandle::Drag
(const TrackPanelMouseEvent &event, AudacityProject *pProject) -> Result
{
   auto result = CommonRulerHandle::Drag(event, pProject);
   if (!( result & RefreshCode::Cancelled )) {
      if (mClicked == Button::Left) {
         if ( mParent ) {
            mX = event.event.m_x;
            mParent->UpdateQuickPlayPos( mX, event.event.ShiftDown() );
            mParent->HandleQPDrag( event.event, mX );
         }
      }
   }
   return result;
}

void AdornedRulerPanel::HandleQPDrag(wxMouseEvent &/*event*/, wxCoord mousePosX)
{
   bool isWithinClick =
      (mLeftDownClickUnsnapped >= 0) &&
      IsWithinMarker(mousePosX, mLeftDownClickUnsnapped);
   bool isWithinStart = IsWithinMarker(mousePosX, mOldPlayRegionStart);
   bool isWithinEnd = IsWithinMarker(mousePosX, mOldPlayRegionEnd);
   bool canDragSel = !mPlayRegionLock && mPlayRegionDragsSelection;

   switch (mMouseEventState)
   {
      case mesNone:
         // If close to either end of play region, snap to closest
         if (isWithinStart || isWithinEnd) {
            if (fabs(mQuickPlayPos - mOldPlayRegionStart) < fabs(mQuickPlayPos - mOldPlayRegionEnd))
               mQuickPlayPos = mOldPlayRegionStart;
            else
               mQuickPlayPos = mOldPlayRegionEnd;
         }
         break;
      case mesDraggingPlayRegionStart:
         // Don't start dragging until beyond tolerance initial playback start
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
            mQuickPlayPos = mOldPlayRegionEnd;
         }
         else
            mIsDragging = true;
         if (isWithinStart) {
            mQuickPlayPos = mOldPlayRegionStart;
         }
         mPlayRegionEnd = mQuickPlayPos;
         if (canDragSel) {
            DragSelection();
         }
         break;
      case mesSelectingPlayRegionClick:

         // Don't start dragging until mouse is beyond tolerance of initial click.
         if (isWithinClick || mLeftDownClick == -1) {
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

auto AdornedRulerPanel::ScrubbingHandle::Preview
(const TrackPanelMouseState &state, const AudacityProject *pProject)
-> HitTestPreview
{
   (void)state;// Compiler food
   const auto &scrubber = pProject->GetScrubber();
   auto message = ScrubbingMessage(scrubber, mClicked == Button::Left);

   return {
      message,
      {},
      // Tooltip is same as status message, or blank
      ((mParent && mParent->mTimelineToolTip) ? message : wxString{}),
   };
}

auto AdornedRulerPanel::QPHandle::Preview
(const TrackPanelMouseState &state, const AudacityProject *pProject)
-> HitTestPreview
{
   wxString tooltip;
   if (mParent && mParent->mTimelineToolTip) {
      if (!mParent->mQuickPlayEnabled)
         tooltip = _("Quick-Play disabled");
      else
         tooltip = _("Quick-Play enabled");
   }

   wxString message;
   const auto &scrubber = pProject->GetScrubber();
   const bool scrubbing = scrubber.HasMark();
   if (scrubbing)
      // Don't distinguish zones
      message = ScrubbingMessage(scrubber, false);
   else
      // message = Insert timeline status bar message here
      ;

   static wxCursor cursorHand{ wxCURSOR_HAND };
   static wxCursor cursorSizeWE{ wxCURSOR_SIZEWE };
   
   bool showArrows = false;
   if (mParent && mParent->mQuickPlayEnabled)
      showArrows =
         (mClicked == Button::Left)
         || mParent->IsWithinMarker(
               state.state.m_x, mParent->mOldPlayRegionStart)
         || mParent->IsWithinMarker(
               state.state.m_x, mParent->mOldPlayRegionEnd);
   
   return {
      message,
      showArrows ? &cursorSizeWE : &cursorHand,
      tooltip,
   };
}

auto AdornedRulerPanel::QPHandle::Release
(const TrackPanelMouseEvent &event, AudacityProject *pProject,
 wxWindow *pParent) -> Result
{
   // Keep a shared pointer to self.  Otherwise *this might get deleted
   // in HandleQPRelease on Windows!  Because there is an event-loop yield
   // stopping playback, which caused OnCaptureLost to be called, which caused
   // clearing of CellularPanel targets!
   auto saveMe = mParent->mQPCell->mHolder.lock();

   auto result = CommonRulerHandle::Release(event, pProject, pParent);
   if (!( result & RefreshCode::Cancelled )) {
      if (mClicked == Button::Left) {
         if ( mParent )
            mParent->HandleQPRelease( event.event );
            // Update the hot zones for cursor changes
            mParent->mOldPlayRegionStart = mParent->mPlayRegionStart;
            mParent->mOldPlayRegionEnd = mParent->mPlayRegionEnd;
      }
   }
   return result;
}

void AdornedRulerPanel::HandleQPRelease(wxMouseEvent &evt)
{
   if (mPlayRegionEnd < mPlayRegionStart) {
      // Swap values to ensure mPlayRegionStart < mPlayRegionEnd
      double tmp = mPlayRegionStart;
      mPlayRegionStart = mPlayRegionEnd;
      mPlayRegionEnd = tmp;
   }

   const double t0 = mTracks->GetStartTime();
   const double t1 = mTracks->GetEndTime();
   const double sel0 = mProject->GetSel0();
   const double sel1 = mProject->GetSel1();

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

   mMouseEventState = mesNone;
   mIsDragging = false;
   mLeftDownClick = -1;

   auto cleanup = finally( [&] {
      if (mPlayRegionLock) {
         // Restore Locked Play region
         SetPlayRegion(mOldPlayRegionStart, mOldPlayRegionEnd);
         GetMenuCommandHandler(*mProject).OnLockPlayRegion(*mProject);
         // and release local lock
         mPlayRegionLock = false;
      }
   } );

   StartQPPlay(evt.ShiftDown(), evt.ControlDown());
}

auto AdornedRulerPanel::QPHandle::Cancel
(AudacityProject *pProject) -> Result
{
   auto result = CommonRulerHandle::Cancel(pProject);

   if (mClicked == Button::Left) {
      if( mParent ) {
         pProject->GetViewInfo().selectedRegion = mOldSelection;
         mParent->mMouseEventState = mesNone;
         mParent->SetPlayRegion(
            mParent->mOldPlayRegionStart, mParent->mOldPlayRegionEnd);
         if (mParent->mPlayRegionLock) {
            // Restore Locked Play region
            GetMenuCommandHandler(*pProject).OnLockPlayRegion(*pProject);
            // and release local lock
            mParent->mPlayRegionLock = false;
         }
      }
   }

   return result;
}

void AdornedRulerPanel::StartQPPlay(bool looped, bool cutPreview)
{
   const double t0 = mTracks->GetStartTime();
   const double t1 = mTracks->GetEndTime();
   const double sel0 = mProject->GetSel0();
   const double sel1 = mProject->GetSel1();

   // Start / Restart playback on left click.
   bool startPlaying = (mPlayRegionStart >= 0);

   if (startPlaying) {
      ControlToolBar* ctb = mProject->GetControlToolBar();
      ctb->StopPlaying();

      bool loopEnabled = true;
      double start, end;

      if ((mPlayRegionEnd - mPlayRegionStart == 0.0) && looped) {
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
      options.playLooped = (loopEnabled && looped);

      auto oldStart = mPlayRegionStart;
      if (!cutPreview)
         options.pStartTime = &oldStart;
      else
         options.timeTrack = NULL;

      ControlToolBar::PlayAppearance appearance =
         cutPreview ? ControlToolBar::PlayAppearance::CutPreview
         : options.playLooped ? ControlToolBar::PlayAppearance::Looped
         : ControlToolBar::PlayAppearance::Straight;

      mPlayRegionStart = start;
      mPlayRegionEnd = end;
      Refresh();

      ctb->PlayPlayRegion((SelectedRegion(start, end)),
                          options, PlayMode::normalPlay,
                          appearance,
                          false,
                          true);

   }
}

// This version toggles ruler state indirectly via the scrubber
// to ensure that all the places where the state is shown update.
// For example buttons and menus must update.
void AdornedRulerPanel::OnToggleScrubRulerFromMenu(wxCommandEvent&)
{
   auto &scrubber = mProject->GetScrubber();
   scrubber.OnToggleScrubRuler(*mProject);
}

void AdornedRulerPanel::OnToggleScrubRuler(/*wxCommandEvent&*/)
{
   mShowScrubbing = !mShowScrubbing;
   WriteScrubEnabledPref(mShowScrubbing);
   gPrefs->Flush();
   SetPanelSize();
}

void AdornedRulerPanel::SetPanelSize()
{
   wxSize size { GetSize().GetWidth(), GetRulerHeight(mShowScrubbing) };
   SetSize(size);
   SetMinSize(size);
   GetParent()->PostSizeEventToParent();
}

void AdornedRulerPanel::DrawBothOverlays()
{
   mProject->GetTrackPanel()->DrawOverlays( false );
   DrawOverlays( false );
}

void AdornedRulerPanel::UpdateButtonStates()
{
   auto common = [this]
   (AButton &button, const wxString &commandName, const wxString &label) {
      TranslatedInternalString command{ commandName, label };
      ToolBar::SetButtonToolTip( button, &command, 1u );
      button.SetLabel(button.GetToolTipText());

      button.UpdateStatus();
   };

   {
      // The button always reflects the pinned head preference, even though
      // there is also a Playback preference that may overrule it for scrubbing
      bool state = TracksPrefs::GetPinnedHeadPreference();
      auto pinButton = static_cast<AButton*>(FindWindow(OnTogglePinnedStateID));
      if( !state )
         pinButton->PopUp();
      else
         pinButton->PushDown();
      pinButton->SetAlternateIdx(
         (gAudioIO->IsCapturing() ? 2 : 0) + (state ? 0 : 1));
      // Bug 1584: Toltip now shows what clicking will do.
      const auto label = state
      ? _("Click to unpin")
      : _("Click to pin");
      common(*pinButton, wxT("PinnedHead"), label);
   }
}

void AdornedRulerPanel::OnTogglePinnedState(wxCommandEvent & /*event*/)
{
   GetMenuCommandHandler(*mProject).OnTogglePinnedHead(*mProject);
   UpdateButtonStates();
}

void AdornedRulerPanel::UpdateQuickPlayPos(wxCoord &mousePosX, bool shiftDown)
{
   // Keep Quick-Play within usable track area.
   TrackPanel *tp = mProject->GetTrackPanel();
   int width;
   tp->GetTracksUsableArea(&width, NULL);
   mousePosX = std::max(mousePosX, tp->GetLeftOffset());
   mousePosX = std::min(mousePosX, tp->GetLeftOffset() + width - 1);

   mQuickPlayPosUnsnapped = mQuickPlayPos = Pos2Time(mousePosX);

   HandleSnapping();

   // If not looping, restrict selection to end of project
   if ((LastCell() == mQPCell || mQPCell->Clicked()) && !shiftDown) {
      const double t1 = mTracks->GetEndTime();
      mQuickPlayPos = std::min(t1, mQuickPlayPos);
   }
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

   wxMenuItem *ruleritem;
   if (mShowScrubbing)
      ruleritem = rulerMenu.Append(OnScrubRulerID, _("Disable Scrub Ruler"));
   else
      ruleritem = rulerMenu.Append(OnScrubRulerID, _("Enable Scrub Ruler"));

   PopupMenu(&rulerMenu, pos);
}

void AdornedRulerPanel::ShowScrubMenu(const wxPoint & pos)
{
   auto &scrubber = mProject->GetScrubber();
   PushEventHandler(&scrubber);
   auto cleanup = finally([this]{ PopEventHandler(); });

   wxMenu rulerMenu;
   mProject->GetScrubber().PopulatePopupMenu(rulerMenu);
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
   mViewInfo->selectedRegion.setT0(mPlayRegionStart, false);
   mViewInfo->selectedRegion.setT1(mPlayRegionEnd, true);
}

void AdornedRulerPanel::HandleSnapping()
{
   auto handle = mQPCell->mHolder.lock();
   if (handle) {
      auto &pSnapManager = handle->mSnapManager;
      if (! pSnapManager)
         pSnapManager = std::make_unique<SnapManager>(mTracks, mViewInfo);
      
      auto results = pSnapManager->Snap(NULL, mQuickPlayPos, false);
      mQuickPlayPos = results.outTime;
      mIsSnapped = results.Snapped();
   }
}

void AdornedRulerPanel::OnTimelineToolTips(wxCommandEvent&)
{
   mTimelineToolTip = (mTimelineToolTip)? false : true;
   gPrefs->Write(wxT("/QuickPlay/ToolTips"), mTimelineToolTip);
   gPrefs->Flush();
   RegenerateTooltips();
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
      GetMenuCommandHandler(*mProject).OnUnlockPlayRegion(*mProject);
   else
      GetMenuCommandHandler(*mProject).OnLockPlayRegion(*mProject);
}


// Draws the horizontal <===>
void AdornedRulerPanel::DoDrawPlayRegion(wxDC * dc)
{
   double start, end;
   GetPlayRegion(&start, &end);

   if (start >= 0)
   {
      const int x1 = Time2Pos(start);
      const int x2 = Time2Pos(end)-2;
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

void AdornedRulerPanel::ShowContextMenu( MenuChoice choice, const wxPoint *pPosition)
{
   wxPoint position;
   if(pPosition)
      position = *pPosition;
   else
   {
      auto rect = GetRect();
      position = { rect.GetLeft() + 1, rect.GetBottom() + 1 };
   }

   switch (choice) {
      case MenuChoice::QuickPlay:
         ShowMenu(position); break;
      case MenuChoice::Scrub:
         ShowScrubMenu(position); break;
      default:
         return;
   }
}

void AdornedRulerPanel::DoDrawBackground(wxDC * dc)
{
   // Draw AdornedRulerPanel border
   AColor::UseThemeColour( dc, clrTrackInfo );
   dc->DrawRectangle( mInner );

   if (mShowScrubbing) {
      // Let's distinguish the scrubbing area by using a themable
      // colour and a line to set it off.  
      AColor::UseThemeColour(dc, clrScrubRuler, clrTrackPanelText );
      wxRect ScrubRect = mScrubZone;
      ScrubRect.Inflate( 1,0 );
      dc->DrawRectangle(ScrubRect);
   }
}

void AdornedRulerPanel::DoDrawEdge(wxDC *dc)
{
   wxRect r = mOuter;
   r.width -= RightMargin;
   r.height -= BottomMargin;
   AColor::BevelTrackInfo( *dc, true, r );

   // Black stroke at bottom
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
   const int p0 = max(1, Time2Pos(mViewInfo->selectedRegion.t0()));
   const int p1 = min(mInner.width, Time2Pos(mViewInfo->selectedRegion.t1()));

   dc->SetBrush( wxBrush( theTheme.Colour( clrRulerBackground )) );
   dc->SetPen(   wxPen(   theTheme.Colour( clrRulerBackground )) );

   wxRect r;
   r.x = p0;
   r.y = mInner.y;
   r.width = p1 - p0 - 1;
   r.height = mInner.height;
   dc->DrawRectangle( r );
}

int AdornedRulerPanel::GetRulerHeight(bool showScrubBar)
{
   return ProperRulerHeight + (showScrubBar ? ScrubHeight : 0);
}

void AdornedRulerPanel::SetLeftOffset(int offset)
{
   mLeftOffset = offset;
   mRuler.SetUseZoomInfo(offset, mViewInfo);
}

// Draws the play/recording position indicator.
void AdornedRulerPanel::DoDrawIndicator
   (wxDC * dc, wxCoord xx, bool playing, int width, bool scrub, bool seek)
{
   ADCChanger changer(dc); // Undo pen and brush changes at function exit

   AColor::IndicatorColor( dc, playing );

   wxPoint tri[ 3 ];
   if (seek) {
      auto height = IndicatorHeightForWidth(width);
      // Make four triangles
      const int TriangleWidth = width * 3 / 8;

      // Double-double headed, left-right
      auto yy = mShowScrubbing
      ? mScrubZone.y
      : (mInner.GetBottom() + 1) - 1 /* bevel */ - height;
      tri[ 0 ].x = xx - IndicatorOffset;
      tri[ 0 ].y = yy;
      tri[ 1 ].x = xx - IndicatorOffset;
      tri[ 1 ].y = yy + height;
      tri[ 2 ].x = xx - TriangleWidth;
      tri[ 2 ].y = yy + height / 2;
      dc->DrawPolygon( 3, tri );

      tri[ 0 ].x -= TriangleWidth;
      tri[ 1 ].x -= TriangleWidth;
      tri[ 2 ].x -= TriangleWidth;
      dc->DrawPolygon( 3, tri );

      tri[ 0 ].x = tri[ 1 ].x = xx + IndicatorOffset;
      tri[ 2 ].x = xx + TriangleWidth;
      dc->DrawPolygon( 3, tri );


      tri[ 0 ].x += TriangleWidth;
      tri[ 1 ].x += TriangleWidth;
      tri[ 2 ].x += TriangleWidth;
      dc->DrawPolygon( 3, tri );
   }
   else if (scrub) {
      auto height = IndicatorHeightForWidth(width);
      const int IndicatorHalfWidth = width / 2;

      // Double headed, left-right
      auto yy = mShowScrubbing
         ? mScrubZone.y
         : (mInner.GetBottom() + 1) - 1 /* bevel */ - height;
      tri[ 0 ].x = xx - IndicatorOffset;
      tri[ 0 ].y = yy;
      tri[ 1 ].x = xx - IndicatorOffset;
      tri[ 1 ].y = yy + height;
      tri[ 2 ].x = xx - IndicatorHalfWidth;
      tri[ 2 ].y = yy + height / 2;
      dc->DrawPolygon( 3, tri );
      tri[ 0 ].x = tri[ 1 ].x = xx + IndicatorOffset;
      tri[ 2 ].x = xx + IndicatorHalfWidth;
      dc->DrawPolygon( 3, tri );
   }
   else {
      bool pinned = ControlToolBar::IsTransportingPinned();
      wxBitmap & bmp = theTheme.Bitmap( pinned ? 
         (playing ? bmpPlayPointerPinned : bmpRecordPointerPinned) :
         (playing ? bmpPlayPointer : bmpRecordPointer) 
      );
      const int IndicatorHalfWidth = bmp.GetWidth() / 2;
      dc->DrawBitmap( bmp, xx - IndicatorHalfWidth -1, mInner.y );
#if 0

      // Down pointing triangle
      auto height = IndicatorHeightForWidth(width);
      const int IndicatorHalfWidth = width / 2;
      tri[ 0 ].x = xx - IndicatorHalfWidth;
      tri[ 0 ].y = mInner.y;
      tri[ 1 ].x = xx + IndicatorHalfWidth;
      tri[ 1 ].y = mInner.y;
      tri[ 2 ].x = xx;
      tri[ 2 ].y = mInner.y + height;
      dc->DrawPolygon( 3, tri );
#endif
   }
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

// CellularPanel implementation
auto AdornedRulerPanel::FindCell(int mouseX, int mouseY) -> FoundCell
{
   bool mayScrub = mProject->GetScrubber().CanScrub() &&
      mShowScrubbing;
   if (mayScrub && mScrubZone.Contains(mouseX, mouseY))
      return { mScrubbingCell, mScrubZone };

   if (mInner.Contains(mouseX, mouseY))
      return { mQPCell, mInner };

   return {};
}

wxRect AdornedRulerPanel::FindRect(const TrackPanelCell &cell)
{
   if (&cell == mScrubbingCell.get())
      return mScrubZone;
   if (&cell == mQPCell.get())
      return mInner;
   
   return {};
}


AudacityProject * AdornedRulerPanel::GetProject() const
{
   return mProject;
}


TrackPanelCell *AdornedRulerPanel::GetFocusedCell()
{
   // No switching of focus yet to the other, scrub zone
   return mQPCell.get();
}


void AdornedRulerPanel::SetFocusedCell()
{
}


void AdornedRulerPanel::ProcessUIHandleResult
(TrackPanelCell *pClickedTrack, TrackPanelCell *pLatestCell,
 unsigned refreshResult)
{
   (void)pLatestCell;// Compiler food
   (void)pClickedTrack;// Compiler food
   if (refreshResult & RefreshCode::DrawOverlays)
      DrawBothOverlays();
}

void AdornedRulerPanel::UpdateStatusMessage( const wxString &message )
{
   GetProject()->TP_DisplayStatusMessage(message);
}

bool AdornedRulerPanel::TakesFocus() const
{
   return false;
}

void AdornedRulerPanel::CreateOverlays()
{
   if (!mOverlay)
      mOverlay =
         std::make_unique<QuickPlayIndicatorOverlay>( mProject );
}
