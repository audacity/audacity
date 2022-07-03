/**********************************************************************

  Audacity: A Digital Audio Editor

  RulerUpdater.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.cpp

*******************************************************************//**

\class RulerUpdater
\brief Used to update a Ruler.

  This is a pure virtual class which sets how a ruler will generate
  its values.

  It supports three subclasses: LinearUpdater, LogarithmicUpdater,
  and CustomUpdater

*//******************************************************************/

#include "RulerUpdater.h"

RulerUpdater::TickSizes::TickSizes(double UPP, int orientation, Ruler::RulerFormat format, bool log)
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

      switch (format) {
      case Ruler::LinearDBFormat:
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
         for (;;) {
            if (units < d) {
               mMinor = d;
               mMajor = d * 5.0;
               return;
            }
            d *= 5.0;
            if (units < d) {
               mMinor = d;
               mMajor = d * 5.0;
               return;
            }
            d *= 2.0;
         }
         break;

      case Ruler::IntFormat:
         d = 1.0;
         for (;;) {
            if (units < d) {
               mMinor = d;
               mMajor = d * 5.0;
               return;
            }
            d *= 5.0;
            if (units < d) {
               mMinor = d;
               mMajor = d * 2.0;
               return;
            }
            d *= 2.0;
         }
         break;

      case Ruler::TimeFormat:
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
               mMajor = 6 * 3600.0;
               return;
            }
            if (units < 6 * 3600.0) { // 6 hrs
               mMinor = 6 * 3600.0;
               mMajor = 24 * 3600.0;
               return;
            }
            if (units < 24 * 3600.0) { // 1 day
               mMinor = 24 * 3600.0;
               mMajor = 7 * 24 * 3600.0;
               return;
            }

            mMinor = 24.0 * 7.0 * 3600.0; // 1 week
            mMajor = 24.0 * 7.0 * 3600.0;
         }

         // Otherwise fall through to RealFormat
         // (fractions of a second should be dealt with
         // the same way as for RealFormat)

      case Ruler::RealFormat:
         d = 0.000001;
         // mDigits is number of digits after the decimal point.
         mDigits = 6;
         for (;;) {
            if (units < d) {
               mMinor = d;
               mMajor = d * 5.0;
               return;
            }
            d *= 5.0;
            if (units < d) {
               mMinor = d;
               mMajor = d * 2.0;
               return;
            }
            d *= 2.0;
            mDigits--;
            // More than 10 digit numbers?  Something is badly wrong.
            // Probably units is coming in with too high a value.
            wxASSERT(mDigits >= -10);
            if (mDigits < -10)
               break;
         }
         mMinor = d;
         mMajor = d * 2.0;
         break;

      case Ruler::RealLogFormat:
         d = 0.000001;
         // mDigits is number of digits after the decimal point.
         mDigits = 6;
         for (;;) {
            if (units < d) {
               mMinor = d;
               mMajor = d * 5.0;
               return;
            }
            d *= 5.0;
            if (units < d) {
               mMinor = d;
               mMajor = d * 2.0;
               return;
            }
            d *= 2.0;
            mDigits--;
            // More than 10 digit numbers?  Something is badly wrong.
            // Probably units is coming in with too high a value.
            wxASSERT(mDigits >= -10);
            if (mDigits < -10)
               break;
         }
         mDigits++;
         mMinor = d;
         mMajor = d * 2.0;
         break;
      }
   }

TranslatableString RulerUpdater::TickSizes::LabelString(
      double d, Ruler::RulerFormat format, const TranslatableString& units)
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
      if (d < 0.0 && (d + mMinor > 0.0) && (format != Ruler::RealLogFormat))
         d = 0.0;

      switch (format) {
      case Ruler::IntFormat:
         s.Printf(wxT("%d"), (int)floor(d + 0.5));
         break;
      case Ruler::LinearDBFormat:
         if (mMinor >= 1.0)
            s.Printf(wxT("%d"), (int)floor(d + 0.5));
         else {
            int precision = -log10(mMinor);
            s.Printf(wxT("%.*f"), precision, d);
         }
         break;
      case Ruler::RealFormat:
         if (mMinor >= 1.0)
            s.Printf(wxT("%d"), (int)floor(d + 0.5));
         else {
            s.Printf(wxString::Format(wxT("%%.%df"), mDigits), d);
         }
         break;
      case Ruler::RealLogFormat:
         if (mMinor >= 1.0)
            s.Printf(wxT("%d"), (int)floor(d + 0.5));
         else {
            s.Printf(wxString::Format(wxT("%%.%df"), mDigits), d);
         }
         break;
      case Ruler::TimeFormat:
         if (useMajor) {
            if (d < 0) {
               s = wxT("-");
               d = -d;
            }

#if ALWAYS_HH_MM_SS
            int secs = (int)(d + 0.5);
            if (mMinor >= 1.0) {
               s.Printf(wxT("%d:%02d:%02d"), secs / 3600, (secs / 60) % 60, secs % 60);
            }
            else {
               wxString t1, t2, format;
               t1.Printf(wxT("%d:%02d:"), secs / 3600, (secs / 60) % 60);
               format.Printf(wxT("%%0%d.%dlf"), mDigits + 3, mDigits);
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
                  m.Printf(wxT("%d:%02d:00"), minutes / 60, minutes % 60);
               else
                  m.Printf(wxT("%d:00"), minutes);
               s += m;
            }
            else if (mMinor >= 1.0) {
               int secs = (int)(d + 0.5);
               wxString t;
               if (secs >= 3600)
                  t.Printf(wxT("%d:%02d:%02d"), secs / 3600, (secs / 60) % 60, secs % 60);
               else if (secs >= 60)
                  t.Printf(wxT("%d:%02d"), secs / 60, secs % 60);
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
                  t1.Printf(wxT("%d:%02d:"), secs / 3600, (secs / 60) % 60);
               else if (secs >= 60)
                  t1.Printf(wxT("%d:"), secs / 60);

               if (secs >= 60)
                  format.Printf(wxT("%%0%d.%dlf"), mDigits + 3, mDigits);
               else
                  format.Printf(wxT("%%%d.%dlf"), mDigits + 3, mDigits);
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
                  t1.Printf(wxT("%d:%02d:"), secs / 3600, (secs / 60) % 60);
               else if (secs >= 60)
                  t1.Printf(wxT("%d:"), secs / 60);

               if (secs >= 60)
                  format.Printf(wxT("%%0%d.%dlf"), mDigits + 3, mDigits);
               else
                  format.Printf(wxT("%%%d.%dlf"), mDigits + 3, mDigits);
               // dd will be reduced to just the seconds and fractional part.
               dd = dd - secs + (secs % 60);
               // truncate to appropriate number of digits, so that the print formatting 
               // doesn't round up 59.9999999 to 60.
               double multiplier = pow(10, mDigits);
               dd = ((int)(dd * multiplier)) / multiplier;
               t2.Printf(format, dd);
#endif
               s += t1 + t2;
            }
         }
         else {
         }
      }

      auto result = Verbatim(s);
      if (!units.empty())
         result += units;

      return result;
 }

bool RulerUpdater::Tick(wxDC& dc,
   int pos, double d, const TickSizes& tickSizes, wxFont font,
   // in/out:
   TickOutputs outputs) const
{
   const double mDbMirrorValue = mRuler.mDbMirrorValue;
   const int mLength = mRuler.mLength;
   const Ruler::RulerFormat mFormat = mRuler.mFormat;

   const int mLeft = mRuler.mLeft;
   const int mTop = mRuler.mTop;
   const int mBottom = mRuler.mBottom;
   const int mRight = mRuler.mRight;
   const int mOrientation = mRuler.mOrientation;

   const Ruler::Fonts& mFonts = *mRuler.mpFonts;
   const TranslatableString mUnits = mRuler.mUnits;
   const int mSpacing = mRuler.mSpacing;
   const bool mFlip = mRuler.mFlip;

   // Bug 521.  dB view for waveforms needs a 2-sided scale.
   if ((mDbMirrorValue > 1.0) && (-d > mDbMirrorValue))
      d = -2 * mDbMirrorValue - d;

   // FIXME: We don't draw a tick if off end of our label arrays
   // But we shouldn't have an array of labels.
   if (outputs.labels.size() >= mLength)
      return false;

   Ruler::Label lab;
   lab.value = d;
   lab.pos = pos;
   lab.text = tickSizes.LabelString(d, mFormat, mUnits);

   const auto result = Ruler::MakeTick(
      lab,
      dc, font,
      outputs.bits,
      mLeft, mTop, mSpacing, mFonts.lead,
      mFlip,
      mOrientation);

   auto& rect = result.first;
   outputs.box.Union(rect);
   outputs.labels.emplace_back(result.second);
   return !rect.IsEmpty();
}

bool RulerUpdater::TickCustom(wxDC& dc, int labelIdx, wxFont font,
   // in/out:
   TickOutputs outputs) const
{
   const int mLeft = mRuler.mLeft;
   const int mTop = mRuler.mTop;
   const int mOrientation = mRuler.mOrientation;

   const Ruler::Fonts& mFonts = *mRuler.mpFonts;
   const int mSpacing = mRuler.mSpacing;
   const bool mFlip = mRuler.mFlip;

   // FIXME: We don't draw a tick if of end of our label arrays
   // But we shouldn't have an array of labels.
   if (labelIdx >= outputs.labels.size())
      return false;

   //This should only used in the mCustom case

   Ruler::Label lab;
   lab.value = 0.0;

   const auto result = Ruler::MakeTick(
      lab,

      dc, font,
      outputs.bits,
      mLeft, mTop, mSpacing, mFonts.lead,
      mFlip,
      mOrientation);

   auto& rect = result.first;
   outputs.box.Union(rect);
   outputs.labels[labelIdx] = (result.second);
   return !rect.IsEmpty();
}

RulerUpdater::~RulerUpdater() = default;

// Theoretically this helps
void RulerUpdater::Update(
   wxDC& dc, const Envelope* envelope,
   UpdateOutputs& allOutputs
) const {}

void RulerUpdater::BoxAdjust(
   UpdateOutputs& allOutputs
)
const
{
   const int mLeft = mRuler.mLeft;
   const int mTop = mRuler.mTop;
   const int mBottom = mRuler.mBottom;
   const int mRight = mRuler.mRight;
   const int mOrientation = mRuler.mOrientation;
   const bool mFlip = mRuler.mFlip;

   int displacementx = 0, displacementy = 0;
   auto& box = allOutputs.box;
   if (!mFlip) {
      if (mOrientation == wxHORIZONTAL) {
         int d = mTop + box.GetHeight() + 5;
         box.Offset(0, d);
         box.Inflate(0, 5);
         displacementx = 0;
         displacementy = d;
      }
      else {
         int d = mLeft - box.GetLeft() + 5;
         box.Offset(d, 0);
         box.Inflate(5, 0);
         displacementx = d;
         displacementy = 0;
      }
   }
   else {
      if (mOrientation == wxHORIZONTAL) {
         box.Inflate(0, 5);
         displacementx = 0;
         displacementy = 0;
      }
   }
   auto update = [=](Ruler::Label& label) {
      label.lx += displacementx;
      label.ly += displacementy;
   };
   for (auto& label : allOutputs.majorLabels)
      update(label);
   for (auto& label : allOutputs.minorLabels)
      update(label);
   for (auto& label : allOutputs.minorMinorLabels)
      update(label);
}
