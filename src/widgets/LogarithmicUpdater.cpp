/**********************************************************************

  Audacity: A Digital Audio Editor

  LogarithmicUpdater.cpp

  Dominic Mazzoni

**********************************************************************/

#include "LogarithmicUpdater.h"

void LogarithmicUpdater::Update(
   wxDC& dc, const Envelope* envelope,
   UpdateOutputs& allOutputs, const RulerStruct& context) const
{
   TickOutputs majorOutputs{
      allOutputs.majorLabels, allOutputs.bits, allOutputs.box };

   const int mLength = context.mLength;
   const RulerFormat mFormat = context.mFormat;

   const int mOrientation = context.mOrientation;

   const double mMin = context.mMin;
   const double mMax = context.mMax;
   const double mHiddenMin = context.mHiddenMin;
   const double mHiddenMax = context.mHiddenMax;

   const RulerStruct::Fonts& mFonts = *context.mpFonts;
   const NumberScale mNumberScale = context.mNumberScale;

   auto numberScale = (mNumberScale == NumberScale{})
      ? NumberScale(nstLogarithmic, mMin, mMax)
      : mNumberScale;

   double UPP = (mHiddenMax - mHiddenMin) / mLength;  // Units per pixel
   TickSizes tickSizes{ UPP, mOrientation, mFormat, true };

   tickSizes.mDigits = 2; //TODO: implement dynamic digit computation

   double loLog = log10(mMin);
   double hiLog = log10(mMax);
   int loDecade = (int)floor(loLog);

   double val;
   double startDecade = pow(10., (double)loDecade);

   // Major ticks are the decades
   double decade = startDecade;
   double delta = hiLog - loLog, steps = fabs(delta);
   double step = delta >= 0 ? 10 : 0.1;
   double rMin = std::min(mMin, mMax), rMax = std::max(mMin, mMax);
   for (int i = 0; i <= steps; i++)
   {  // if(i!=0)
      {  val = decade;
      if (val >= rMin && val < rMax) {
         const int pos(0.5 + mLength * numberScale.ValueToPosition(val));
         Tick(dc, pos, val, tickSizes, mFonts.major, majorOutputs, context);
      }
      }
      decade *= step;
   }

   // Minor ticks are multiples of decades
   decade = startDecade;
   float start, end, mstep;
   if (delta > 0)
   {
      start = 2; end = 10; mstep = 1;
   }
   else
   {
      start = 9; end = 1; mstep = -1;
   }
   steps++;
   tickSizes.useMajor = false;
   TickOutputs minorOutputs{
      allOutputs.minorLabels, allOutputs.bits, allOutputs.box };
   for (int i = 0; i <= steps; i++) {
      for (int j = start; j != end; j += mstep) {
         val = decade * j;
         if (val >= rMin && val < rMax) {
            const int pos(0.5 + mLength * numberScale.ValueToPosition(val));
            Tick(dc, pos, val, tickSizes, mFonts.minor, minorOutputs, context);
         }
      }
      decade *= step;
   }

   // MinorMinor ticks are multiples of decades
   decade = startDecade;
   if (delta > 0)
   {
      start = 10; end = 100; mstep = 1;
   }
   else
   {
      start = 100; end = 10; mstep = -1;
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
                  Tick(dc, pos, val, tickSizes,
                     mFonts.minorMinor, minorMinorOutputs, context);
               }
            }
         }
      }
      decade *= step;
   }

   BoxAdjust(allOutputs, context);
}

LogarithmicUpdater::~LogarithmicUpdater() = default;
