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


#include "Ruler.h"

#include <wx/dcclient.h>
#include <wx/dcscreen.h>

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../Envelope.h"
#include "NumberScale.h"
#include "../Theme.h"
#include "ViewInfo.h"

using std::min;
using std::max;

//wxColour Ruler::mTickColour{ 153, 153, 153 };

//
// Ruler
//

Ruler::Ruler()
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

   mLength = 0;

   mCustom = false;
   mbMinor = true;

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

void Ruler::SetUnits(const TranslatableString &units)
{
   // Specify the name of the units (like "dB") if you
   // want numbers like "1.6" formatted as "1.6 dB".

   if (mUnits != units) {
      mUnits = units;

      Invalidate();
   }
}

void Ruler::SetDbMirrorValue( const double d )
{
   if (mDbMirrorValue != d) {
      mDbMirrorValue = d;

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

namespace {
void FindFontHeights(
   wxCoord &height, wxCoord &lead, wxDC &dc, const wxFont &font )
{
   wxCoord strW, strH, strD, strL;
   static const wxString exampleText = wxT("0.9");   //ignored for height calcs on all platforms
   dc.SetFont( font );
   dc.GetTextExtent(exampleText, &strW, &strH, &strD, &strL);
   height = strH - strD - strL;
   lead = strL;
}

void FindFontHeights(
   wxCoord &height, wxCoord &lead,
   wxDC &dc, int fontSize, wxFontWeight weight = wxFONTWEIGHT_NORMAL )
{
   const wxFont font{ fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, weight };
   FindFontHeights( height, lead, dc, font );
}
}

void Ruler::SetFonts(const wxFont &minorFont, const wxFont &majorFont, const wxFont &minorMinorFont)
{
   // Won't override these fonts

   mpUserFonts = std::make_unique<Fonts>(
      Fonts{ majorFont, minorFont, minorMinorFont, 0 } );

   wxScreenDC dc;
   wxCoord height;
   FindFontHeights( height, mpUserFonts->lead, dc, majorFont );

   mpFonts.reset();
   mpFonts.reset();
   Invalidate();
}

void Ruler::SetNumberScale(const NumberScale &scale)
{
   if ( mNumberScale != scale ) {
      mNumberScale = scale;
      Invalidate();
   }
}

void Ruler::OfflimitsPixels(int start, int end)
{
   int length = mLength;
   if (mOrientation == wxHORIZONTAL)
      length = mRight - mLeft;
   else
      length = mBottom - mTop;
   if( length < 0 )
      return;

   auto size = static_cast<size_t>( length + 1 );
   if ( mUserBits.size() < size ) {
      mLength = length;
      mUserBits.resize( size, false );
   }

   if (end < start)
      std::swap( start, end );

   if (start < 0)
      start = 0;
   if (end > mLength)
      end = mLength;

   for(int i = start; i <= end; i++)
      mUserBits[i] = true;

   Invalidate();
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
   if (mOrientation == wxHORIZONTAL)
      mLength = mRight-mLeft;
   else
      mLength = mBottom-mTop;

   mpCache.reset();
   // Bug 2316 we must preserve off-limit pixels.
   // mUserBits.clear();
}

struct Ruler::TickSizes
{
   bool useMajor = true;

   double       mMajor;
   double       mMinor;

   int          mDigits;

TickSizes( double UPP, int orientation, RulerFormat format, bool log )
{
   //TODO: better dynamic digit computation for the log case
   (void)log;

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
   double units = ((orientation == wxHORIZONTAL) ? 22 : 16) * fabs(UPP);

   mDigits = 0;

   switch(format) {
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

TranslatableString LabelString(
   double d, RulerFormat format, const TranslatableString &units )
   const
{
   // Given a value, turn it into a string according
   // to the current ruler format.  The number of digits of
   // accuracy depends on the resolution of the ruler,
   // i.e. how far zoomed in or out you are.

   wxString s;

   // PRL Todo: are all these cases properly localized?  (Decimal points,
   // hour-minute-second, etc.?)

   // Replace -0 with 0
   if (d < 0.0 && (d+mMinor > 0.0) && ( format != RealLogFormat ))
      d = 0.0;

   switch( format ) {
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
      if (useMajor) {
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

   auto result = Verbatim( s );
   if (!units.empty())
      result += units;

   return result;
}

}; // struct Ruler::TickSizes

auto Ruler::MakeTick(
   Label lab,
   wxDC &dc, wxFont font,
   std::vector<bool> &bits,
   int left, int top, int spacing, int lead,
   bool flip, int orientation )
      -> std::pair< wxRect, Label >
{
   lab.lx = left - 1000; // don't display
   lab.ly = top - 1000;  // don't display

   auto length = bits.size() - 1;
   auto pos = lab.pos;

   dc.SetFont( font );

   wxCoord strW, strH, strD, strL;
   auto str = lab.text;
   // Do not put the text into results until we are sure it does not overlap
   lab.text = {};
   dc.GetTextExtent(str.Translation(), &strW, &strH, &strD, &strL);

   int strPos, strLen, strLeft, strTop;
   if ( orientation == wxHORIZONTAL ) {
      strLen = strW;
      strPos = pos - strW/2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strW >= length)
         strPos = length - strW;
      strLeft = left + strPos;
      if ( flip )
         strTop = top + 4;
      else
         strTop = -strH - lead;
//         strTop = top - lead + 4;// More space was needed...
   }
   else {
      strLen = strH;
      strPos = pos - strH/2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strH >= length)
         strPos = length - strH;
      strTop = top + strPos;
      if ( flip )
         strLeft = left + 5;
      else
         strLeft = -strW - 6;
   }

   // FIXME: we shouldn't even get here if strPos < 0.
   // Ruler code currently does  not handle very small or
   // negative sized windows (i.e. don't draw) properly.
   if( strPos < 0 )
      return { {}, lab };

   // See if any of the pixels we need to draw this
   // label is already covered

   int i;
   for(i=0; i<strLen; i++)
      if ( bits[strPos+i] )
         return { {}, lab };

   // If not, position the label

   lab.lx = strLeft;
   lab.ly = strTop;

   // And mark these pixels, plus some surrounding
   // ones (the spacing between labels), as covered
   int leftMargin = spacing;
   if (strPos < leftMargin)
      leftMargin = strPos;
   strPos -= leftMargin;
   strLen += leftMargin;

   int rightMargin = spacing;
   if (strPos + strLen > length - spacing)
      rightMargin = length - strPos - strLen;
   strLen += rightMargin;

   for(i=0; i<strLen; i++)
      bits[strPos+i] = true;

   // Good to display the text
   lab.text = str;
   return { { strLeft, strTop, strW, strH }, lab };
}

struct Ruler::Updater {
   const Ruler &mRuler;
   const ZoomInfo *zoomInfo;

   explicit Updater( const Ruler &ruler, const ZoomInfo *z )
   : mRuler{ ruler }
   , zoomInfo{ z }
   {}

   const double mDbMirrorValue = mRuler.mDbMirrorValue;
   const int mLength = mRuler.mLength;
   const RulerFormat mFormat = mRuler.mFormat;
   const TranslatableString mUnits = mRuler.mUnits;

   const int mLeft = mRuler.mLeft;
   const int mTop = mRuler.mTop;
   const int mBottom = mRuler.mBottom;
   const int mRight = mRuler.mRight;

   const int mSpacing = mRuler.mSpacing;
   const int mOrientation = mRuler.mOrientation;
   const bool mFlip = mRuler.mFlip;

   const bool mCustom = mRuler.mCustom;
   const Fonts &mFonts = *mRuler.mpFonts;
   const bool mLog = mRuler.mLog;
   const double mHiddenMin = mRuler.mHiddenMin;
   const double mHiddenMax = mRuler.mHiddenMax;
   const bool mLabelEdges = mRuler.mLabelEdges;
   const double mMin = mRuler.mMin;
   const double mMax = mRuler.mMax;
   const int mLeftOffset = mRuler.mLeftOffset;
   const NumberScale mNumberScale = mRuler.mNumberScale;

   struct TickOutputs;
   
   bool Tick( wxDC &dc,
      int pos, double d, const TickSizes &tickSizes, wxFont font,
      TickOutputs outputs
   ) const;

   // Another tick generator for custom ruler case (noauto) .
   bool TickCustom( wxDC &dc, int labelIdx, wxFont font,
      TickOutputs outputs
   ) const;

   static void ChooseFonts(
      std::unique_ptr<Fonts> &pFonts, const Fonts *pUserFonts,
      wxDC &dc, int desiredPixelHeight );

   struct UpdateOutputs;
   
   void Update(
      wxDC &dc, const Envelope* envelope,
      UpdateOutputs &allOutputs
   )// Envelope *speedEnv, long minSpeed, long maxSpeed )
      const;

   void UpdateCustom( wxDC &dc, UpdateOutputs &allOutputs ) const;
   void UpdateLinear(
      wxDC &dc, const Envelope *envelope, UpdateOutputs &allOutputs ) const;
   void UpdateNonlinear( wxDC &dc, UpdateOutputs &allOutputs ) const;
};

struct Ruler::Cache {
   Bits mBits;
   Labels mMajorLabels, mMinorLabels, mMinorMinorLabels;
   wxRect mRect;
};

struct Ruler::Updater::TickOutputs{ Labels &labels; Bits &bits; wxRect &box; };
struct Ruler::Updater::UpdateOutputs {
   Labels &majorLabels, &minorLabels, &minorMinorLabels;
   Bits &bits;
   wxRect &box;
};

bool Ruler::Updater::Tick( wxDC &dc,
   int pos, double d, const TickSizes &tickSizes, wxFont font,
   // in/out:
   TickOutputs outputs ) const
{
   // Bug 521.  dB view for waveforms needs a 2-sided scale.
   if(( mDbMirrorValue > 1.0 ) && ( -d > mDbMirrorValue ))
      d = -2*mDbMirrorValue - d;

   // FIXME: We don't draw a tick if off end of our label arrays
   // But we shouldn't have an array of labels.
   if( outputs.labels.size() >= mLength )
      return false;

   Label lab;
   lab.value = d;
   lab.pos = pos;
   lab.text = tickSizes.LabelString( d, mFormat, mUnits );

   const auto result = MakeTick(
      lab,
      dc, font,
      outputs.bits,
      mLeft, mTop, mSpacing, mFonts.lead,
      mFlip,
      mOrientation );

   auto &rect = result.first;
   outputs.box.Union( rect );
   outputs.labels.emplace_back( result.second );
   return !rect.IsEmpty();
}

bool Ruler::Updater::TickCustom( wxDC &dc, int labelIdx, wxFont font,
   // in/out:
   TickOutputs outputs ) const
{
   // FIXME: We don't draw a tick if of end of our label arrays
   // But we shouldn't have an array of labels.
   if( labelIdx >= outputs.labels.size() )
      return false;

   //This should only used in the mCustom case

   Label lab;
   lab.value = 0.0;

   const auto result = MakeTick(
      lab,

      dc, font,
      outputs.bits,
      mLeft, mTop, mSpacing, mFonts.lead,
      mFlip,
      mOrientation );

   auto &rect = result.first;
   outputs.box.Union( rect );
   outputs.labels[labelIdx] = ( result.second );
   return !rect.IsEmpty();
}

namespace {
double ComputeWarpedLength(const Envelope &env, double t0, double t1)
{
   return env.IntegralOfInverse(t0, t1);
}

double SolveWarpedLength(const Envelope &env, double t0, double length)
{
   return env.SolveIntegralOfInverse(t0, length);
}
}

static constexpr int MinPixelHeight =
#ifdef __WXMSW__
   12;
#else
   10;
#endif

static constexpr int MaxPixelHeight =
#ifdef __WXMSW__
   14;
#elif __WXMAC__
   10;
#else
   12;
#endif


void Ruler::Updater::ChooseFonts(
   std::unique_ptr<Fonts> &pFonts, const Fonts *pUserFonts,
   wxDC &dc, int desiredPixelHeight )
{
   if ( pFonts )
      return;

   if ( pUserFonts ) {
      pFonts = std::make_unique<Fonts>( *pUserFonts );
      return;
   }

   pFonts = std::make_unique<Fonts>( Fonts{ {}, {}, {}, 0 } );
   auto &fonts = *pFonts;

   int fontSize = 4;

   desiredPixelHeight =
      std::max(MinPixelHeight, std::min(MaxPixelHeight, -desiredPixelHeight));

   // Keep making the font bigger until it's too big, then subtract one.
   wxCoord height;
   FindFontHeights( height, fonts.lead, dc, fontSize, wxFONTWEIGHT_BOLD );
   while (height <= desiredPixelHeight && fontSize < 40) {
      fontSize++;
      FindFontHeights( height, fonts.lead, dc, fontSize, wxFONTWEIGHT_BOLD );
   }
   fontSize--;
   FindFontHeights( height, fonts.lead, dc, fontSize );

   fonts.major = wxFont{ fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD };
   fonts.minor = wxFont{ fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL };
   fonts.minorMinor = wxFont{ fontSize - 1, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL };
}

void Ruler::Updater::UpdateCustom( wxDC &dc, UpdateOutputs &allOutputs ) const
{
   TickOutputs majorOutputs{
      allOutputs.majorLabels, allOutputs.bits, allOutputs.box };

   // SET PARAMETER IN MCUSTOM CASE
   // Works only with major labels

   int numLabel = allOutputs.majorLabels.size();

   for( int i = 0; (i<numLabel) && (i<=mLength); ++i )
      TickCustom( dc, i, mFonts.major, majorOutputs );
}

void Ruler::Updater::UpdateLinear(
   wxDC &dc, const Envelope *envelope, UpdateOutputs &allOutputs ) const
{
   TickOutputs majorOutputs{
      allOutputs.majorLabels, allOutputs.bits, allOutputs.box };

   // Use the "hidden" min and max to determine the tick size.
   // That may make a difference with fisheye.
   // Otherwise you may see the tick size for the whole ruler change
   // when the fisheye approaches start or end.
   double UPP = (mHiddenMax-mHiddenMin)/mLength;  // Units per pixel
   TickSizes tickSizes{ UPP, mOrientation, mFormat, false };

   auto TickAtValue =
   [this, &tickSizes, &dc, &majorOutputs]
   ( double value ) -> int {
      // Make a tick only if the value is strictly between the bounds
      if ( value <= std::min( mMin, mMax ) )
         return -1;
      if ( value >= std::max( mMin, mMax ) )
         return -1;

      int mid;
      if (zoomInfo != NULL) {
         // Tick only at zero
         if ( value )
            return -1;
         mid = (int)(zoomInfo->TimeToPosition(0.0, mLeftOffset));
      }
      else
         mid = (int)(mLength*((mMin - value) / (mMin - mMax)) + 0.5);

      const int iMaxPos = (mOrientation == wxHORIZONTAL) ? mRight : mBottom - 5;
      if (mid >= 0 && mid < iMaxPos)
         Tick( dc, mid, value, tickSizes, mFonts.major, majorOutputs );
      else
         return -1;
      
      return mid;
   };

   if ( mDbMirrorValue ) {
      // For dB scale, let the zeroes prevail over the extreme values if
      // not the same, and let midline prevail over all

      // Do the midline
      TickAtValue( -mDbMirrorValue );

      // Do the upper zero
      TickAtValue( 0.0 );

      // Do the other zero
      TickAtValue( -2 * mDbMirrorValue );
   }

   // Extreme values
   if (mLabelEdges) {
      Tick( dc, 0, mMin, tickSizes, mFonts.major, majorOutputs );
      Tick( dc, mLength, mMax, tickSizes, mFonts.major, majorOutputs );
   }

   if ( !mDbMirrorValue ) {
      // Zero (if it's strictly in the middle somewhere)
      TickAtValue( 0.0 );
   }

   double sg = UPP > 0.0? 1.0: -1.0;

   int nDroppedMinorLabels=0;
   // Major and minor ticks
   for (int jj = 0; jj < 2; ++jj) {
      const double denom = jj == 0 ? tickSizes.mMajor : tickSizes.mMinor;
      auto font = jj == 0 ? mFonts.major : mFonts.minor;
      TickOutputs outputs{
         (jj == 0 ? allOutputs.majorLabels : allOutputs.minorLabels),
         allOutputs.bits, allOutputs.box
      };
      int ii = -1, j = 0;
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
      if (envelope)
         warpedD = ComputeWarpedLength(*envelope, 0.0, d);
      else
         warpedD = d;
      // using ints doesn't work, as
      // this will overflow and be negative at high zoom.
      double step = floor(sg * warpedD / denom);
      while (ii <= mLength) {
         ii++;
         if (zoomInfo)
         {
            prevTime = time;
            time = zoomInfo->PositionToTime(++j);
            nextD = (prevTime + time) / 2.0;
            // wxASSERT(time >= prevTime);
         }
         else
            nextD = d + UPP;
         if (envelope)
            warpedD += ComputeWarpedLength(*envelope, d, nextD);
         else
            warpedD = nextD;
         d = nextD;

         if (floor(sg * warpedD / denom) > step) {
            step = floor(sg * warpedD / denom);
            bool major = jj == 0;
            tickSizes.useMajor = major;
            bool ticked = Tick( dc, ii, sg * step * denom, tickSizes,
               font, outputs );
            if( !major && !ticked ){
               nDroppedMinorLabels++;
            }
         }
      }
   }

   tickSizes.useMajor = true;

   // If we've dropped minor labels through overcrowding, then don't show
   // any of them.  We're allowed though to drop ones which correspond to the
   // major numbers.
   if( nDroppedMinorLabels >
         (allOutputs.majorLabels.size() + (mLabelEdges ? 2:0)) ){
      // Old code dropped the labels AND their ticks, like so:
      //    mMinorLabels.clear();
      // Nowadays we just drop the labels.
      for( auto &label : allOutputs.minorLabels )
         label.text = {};
   }

   // Left and Right Edges
   if (mLabelEdges) {
      Tick( dc, 0, mMin, tickSizes, mFonts.major, majorOutputs );
      Tick( dc, mLength, mMax, tickSizes, mFonts.major, majorOutputs );
   }
}

void Ruler::Updater::UpdateNonlinear(
    wxDC &dc, UpdateOutputs &allOutputs ) const
{
   TickOutputs majorOutputs{
      allOutputs.majorLabels, allOutputs.bits, allOutputs.box };

   auto numberScale = ( mNumberScale == NumberScale{} )
      ? NumberScale( nstLogarithmic, mMin, mMax )
      : mNumberScale;

   double UPP = (mHiddenMax-mHiddenMin)/mLength;  // Units per pixel
   TickSizes tickSizes{ UPP, mOrientation, mFormat, true };

   tickSizes.mDigits = 2; //TODO: implement dynamic digit computation

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
   for(int i=0; i<=steps; i++)
   {  // if(i!=0)
      {  val = decade;
         if(val >= rMin && val < rMax) {
            const int pos(0.5 + mLength * numberScale.ValueToPosition(val));
            Tick( dc, pos, val, tickSizes, mFonts.major, majorOutputs );
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
   tickSizes.useMajor = false;
   TickOutputs minorOutputs{
      allOutputs.minorLabels, allOutputs.bits, allOutputs.box };
   for(int i=0; i<=steps; i++) {
      for(int j=start; j!=end; j+=mstep) {
         val = decade * j;
         if(val >= rMin && val < rMax) {
            const int pos(0.5 + mLength * numberScale.ValueToPosition(val));
            Tick( dc, pos, val, tickSizes, mFonts.minor, minorOutputs );
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
   TickOutputs minorMinorOutputs{
      allOutputs.minorMinorLabels, allOutputs.bits, allOutputs.box };
   for (int i = 0; i <= steps; i++) {
      // PRL:  Bug1038.  Don't label 1.6, rounded, as a duplicate tick for "2"
      if (!(mFormat == IntFormat && decade < 10.0)) {
         for (int f = start; f != (int)(end); f += mstep) {
            if ((int)(f / 10) != f / 10.0f) {
               val = decade * f / 10;
               if (val >= rMin && val < rMax) {
                  const int pos(0.5 + mLength * numberScale.ValueToPosition(val));
                  Tick( dc, pos, val, tickSizes,
                     mFonts.minorMinor, minorMinorOutputs );
               }
            }
         }
      }
      decade *= step;
   }
}

void Ruler::Updater::Update(
   wxDC &dc, const Envelope* envelope,
   UpdateOutputs &allOutputs
)// Envelope *speedEnv, long minSpeed, long maxSpeed )
   const
{
   TickOutputs majorOutputs{
      allOutputs.majorLabels, allOutputs.bits, allOutputs.box };

   if ( mCustom )
      UpdateCustom( dc, allOutputs );
   else if ( !mLog )
      UpdateLinear( dc, envelope, allOutputs );
   else
      UpdateNonlinear( dc, allOutputs );

   int displacementx=0, displacementy=0;
   auto &box = allOutputs.box;
   if (!mFlip) {
      if (mOrientation==wxHORIZONTAL) {
         int d = mTop + box.GetHeight() + 5;
         box.Offset(0,d);
         box.Inflate(0,5);
         displacementx=0;
         displacementy=d;
      }
      else {
         int d = mLeft - box.GetLeft() + 5;
         box.Offset(d,0);
         box.Inflate(5,0);
         displacementx=d;
         displacementy=0;
      }
   }
   else {
      if (mOrientation==wxHORIZONTAL) {
         box.Inflate(0,5);
         displacementx=0;
         displacementy=0;
      }
   }
   auto update = [=]( Label &label ){
      label.lx += displacementx;
      label.ly += displacementy;
   };
   for( auto &label : allOutputs.majorLabels )
      update( label );
   for( auto &label : allOutputs.minorLabels )
      update( label );
   for( auto &label : allOutputs.minorMinorLabels )
      update( label );
}

void Ruler::ChooseFonts( wxDC &dc ) const
{
   Updater::ChooseFonts( mpFonts, mpUserFonts.get(), dc,
      mOrientation == wxHORIZONTAL
         ? mBottom - mTop - 5 // height less ticks and 1px gap
         : MaxPixelHeight
   );
}

void Ruler::UpdateCache(
   wxDC &dc, const Envelope* envelope )
   const // Envelope *speedEnv, long minSpeed, long maxSpeed )
{
   if ( mpCache )
      return;

   const ZoomInfo *zoomInfo = NULL;
   if (!mLog && mOrientation == wxHORIZONTAL)
      zoomInfo = mUseZoomInfo;

   // This gets called when something has been changed
   // (i.e. we've been invalidated).  Recompute all
   // tick positions and font size.

   ChooseFonts( dc );
   mpCache = std::make_unique< Cache >();
   auto &cache = *mpCache;

   // If ruler is being resized, we could end up with it being too small.
   // Values of mLength of zero or below cause bad array allocations and
   // division by zero.  So...
   // IF too small THEN bail out and don't draw.
   if( mLength <= 0 )
      return;

   if (mOrientation == wxHORIZONTAL)
      cache.mRect = { 0, 0, mLength, 0 };
   else
      cache.mRect = { 0, 0, 0, mLength };

   // FIXME: Surely we do not need to allocate storage for the labels?
   // We can just recompute them as we need them?  Yes, but only if
   // mCustom is false!!!!

   if(!mCustom) {
      cache.mMajorLabels.clear();
      cache.mMinorLabels.clear();
      cache.mMinorMinorLabels.clear();
   }

   cache.mBits = mUserBits;
   cache.mBits.resize( static_cast<size_t>(mLength + 1), false );

   // Keep Updater const!  We want no hidden state changes affecting its
   // computations.
   const Updater updater{ *this, zoomInfo };
   Updater::UpdateOutputs allOutputs{
      cache.mMajorLabels, cache.mMinorLabels, cache.mMinorMinorLabels,
      cache.mBits, cache.mRect
   };
   updater.Update(dc, envelope, allOutputs);
}

auto Ruler::GetFonts() const -> Fonts
{
   if ( !mpFonts ) {
      wxScreenDC dc;
      ChooseFonts( dc );
   }

   return *mpFonts;
}

void Ruler::Draw(wxDC& dc) const
{
   Draw( dc, NULL);
}

void Ruler::Draw(wxDC& dc, const Envelope* envelope) const
{
   if( mLength <=0 )
      return;

   UpdateCache( dc, envelope );
   auto &cache = *mpCache;

   dc.SetTextForeground( mTickColour );
#ifdef EXPERIMENTAL_THEMING
   dc.SetPen(mPen);
#else
   dc.SetPen(*wxBLACK_PEN);
#endif

   // Draws a long line the length of the ruler.
   if( !mbTicksOnly )
   {
      if (mOrientation == wxHORIZONTAL) {
         if (mFlip)
            AColor::Line(dc, mLeft, mTop, mRight, mTop);
         else
            AColor::Line(dc, mLeft, mBottom, mRight, mBottom);
      }
      else {
         if (mFlip)
            AColor::Line(dc, mLeft, mTop, mLeft, mBottom);
         else
         {
            // These calculations appear to be wrong, and to never have been used (so not tested) prior to MixerBoard.
            //    AColor::Line(dc, mRect.x-mRect.width, mTop, mRect.x-mRect.width, mBottom);
            const int nLineX = mRight - 1;
            AColor::Line(dc, nLineX, mTop, nLineX, mBottom);
         }
      }
   }

   dc.SetFont( mpFonts->major );

   // We may want to not show the ticks at the extremes,
   // though still showing the labels.
   // This gives a better look when the ruler is on a bevelled
   // button, since otherwise the tick is drawn on the bevel.
   int iMaxPos = (mOrientation==wxHORIZONTAL)? mRight : mBottom-5;

   auto drawLabel = [this, iMaxPos, &dc]( const Label &label, int length ){
      int pos = label.pos;

      if( mbTicksAtExtremes || ((pos!=0)&&(pos!=iMaxPos)))
      {
         if (mOrientation == wxHORIZONTAL) {
            if (mFlip)
               AColor::Line(dc, mLeft + pos, mTop,
                             mLeft + pos, mTop + length);
            else
               AColor::Line(dc, mLeft + pos, mBottom - length,
                             mLeft + pos, mBottom);
         }
         else {
            if (mFlip)
               AColor::Line(dc, mLeft, mTop + pos,
                             mLeft + length, mTop + pos);
            else
               AColor::Line(dc, mRight - length, mTop + pos,
                             mRight, mTop + pos);
         }
      }

      label.Draw(dc, mTwoTone, mTickColour);
   };

   for( const auto &label : cache.mMajorLabels )
      drawLabel( label, 4 );

   if( mbMinor ) {
      dc.SetFont( mpFonts->minor );
      for( const auto &label : cache.mMinorLabels )
         drawLabel( label, 2 );
   }

   dc.SetFont( mpFonts->minorMinor );

   for( const auto &label : cache.mMinorMinorLabels )
      if ( !label.text.empty() )
         drawLabel( label, 2 );
}

// ********** Draw grid ***************************
void Ruler::DrawGrid(wxDC& dc,
   const int gridLineLength,
   const bool minorGrid, const bool majorGrid, int xOffset, int yOffset)
   const
{
   UpdateCache( dc, nullptr );
   auto &cache = *mpCache;

   int gridPos;
   wxPen gridPen;

   if(mbMinor && (minorGrid && (gridLineLength != 0 ))) {
      gridPen.SetColour(178, 178, 178); // very light grey
      dc.SetPen(gridPen);
      for( const auto &label : cache.mMinorLabels ) {
         gridPos = label.pos;
         if(mOrientation == wxHORIZONTAL) {
            if((gridPos != 0) && (gridPos != gridLineLength))
               AColor::Line(dc, gridPos+xOffset, yOffset, gridPos+xOffset, gridLineLength-1+yOffset);
         }
         else {
            if((gridPos != 0) && (gridPos != gridLineLength))
               AColor::Line(dc, xOffset, gridPos+yOffset, gridLineLength-1+xOffset, gridPos+yOffset);
         }
      }
   }

   if(majorGrid && (gridLineLength != 0 )) {
      gridPen.SetColour(127, 127, 127); // light grey
      dc.SetPen(gridPen);
      for( const auto &label : cache.mMajorLabels ) {
         gridPos = label.pos;
         if(mOrientation == wxHORIZONTAL) {
            if((gridPos != 0) && (gridPos != gridLineLength))
               AColor::Line(dc, gridPos+xOffset, yOffset, gridPos+xOffset, gridLineLength-1+yOffset);
         }
         else {
            if((gridPos != 0) && (gridPos != gridLineLength))
               AColor::Line(dc, xOffset, gridPos+yOffset, gridLineLength-1+xOffset, gridPos+yOffset);
         }
      }

      int zeroPosition = GetZeroPosition();
      if(zeroPosition > 0) {
         // Draw 'zero' grid line in black
         dc.SetPen(*wxBLACK_PEN);
         if(mOrientation == wxHORIZONTAL) {
            if(zeroPosition != gridLineLength)
               AColor::Line(dc, zeroPosition+xOffset, yOffset, zeroPosition+xOffset, gridLineLength-1+yOffset);
         }
         else {
            if(zeroPosition != gridLineLength)
               AColor::Line(dc, xOffset, zeroPosition+yOffset, gridLineLength-1+xOffset, zeroPosition+yOffset);
         }
      }
   }
}

int Ruler::FindZero( const Labels &labels ) const
{
   auto begin = labels.begin(), end = labels.end(),
      iter = std::find_if( begin, end, []( const Label &label ){
         return label.value == 0.0;
      } );

   if ( iter == end )
      return -1;
   else
      return iter->pos;
}

int Ruler::GetZeroPosition() const
{
   wxASSERT( mpCache );
   auto &cache = *mpCache;
   int zero;
   if( (zero = FindZero( cache.mMajorLabels ) ) < 0)
      zero = FindZero( cache.mMinorLabels );
   // PRL: don't consult minor minor??
   return zero;
}

void Ruler::GetMaxSize(wxCoord *width, wxCoord *height)
{
   if ( !mpCache ) {
      wxScreenDC sdc;
      UpdateCache( sdc, nullptr );
   }

   auto &cache = *mpCache;
   if (width)
      *width = cache.mRect.GetWidth();

   if (height)
      *height = cache.mRect.GetHeight();
}


void Ruler::SetCustomMode(bool value)
{
   if ( mCustom != value ) {
      mCustom = value;
      Invalidate();
   }
}

#if 0
// These two unused functions need reconsideration of their interactions with
// the cache and update
void Ruler::SetCustomMajorLabels(
   const TranslatableStrings &labels, int start, int step)
{
   SetCustomMode( true );
   mpCache = std::make_unique<Cache>();
   auto &cache = *mpCache;
   auto &mMajorLabels = cache.mMajorLabels;

   const auto numLabel = labels.size();
   mMajorLabels.resize( numLabel );

   for(size_t i = 0; i<numLabel; i++) {
      mMajorLabels[i].text = labels[i];
      mMajorLabels[i].pos  = start + i*step;
   }
}

void Ruler::SetCustomMinorLabels(
   const TranslatableStrings &labels, int start, int step)
{
   SetCustomMode( true );
   mpCache = std::make_unique<Cache>();
   auto &cache = *mpCache;
   auto &mMinorLabels = cache.mMinorLabels;

   const auto numLabel = labels.size();
   mMinorLabels.resize( numLabel );

   for(size_t i = 0; i<numLabel; i++) {
      mMinorLabels[i].text = labels[i];
      mMinorLabels[i].pos  = start + i*step;
   }
}
#endif

void Ruler::Label::Draw(wxDC&dc, bool twoTone, wxColour c) const
{
   if (!text.empty()) {
      bool altColor = twoTone && value < 0.0;

#ifdef EXPERIMENTAL_THEMING
      dc.SetTextForeground(altColor ? theTheme.Colour( clrTextNegativeNumbers) : c);
#else
      dc.SetTextForeground(altColor ? *wxBLUE : *wxBLACK);
#endif
      dc.SetBackgroundMode(wxTRANSPARENT);
      dc.DrawText(text.Translation(), lx, ly);
   }
}

void Ruler::SetUseZoomInfo(int leftOffset, const ZoomInfo *zoomInfo)
{
   
   if ( mLeftOffset != leftOffset ||
      // Hm, is this invalidation sufficient?  What if *zoomInfo changes under us?
      mUseZoomInfo != zoomInfo
   ) {
      mLeftOffset = leftOffset;
      mUseZoomInfo = zoomInfo;
      Invalidate();
   }
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
                       const TranslatableString &units,
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
//      FrequencyPlotDialog since it needs to have an updated ruler before RulerPanel gets the
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
