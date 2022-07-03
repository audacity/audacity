/**********************************************************************

  Audacity: A Digital Audio Editor

  Updater.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_UPDATER__
#define __AUDACITY_UPDATER__

#include "Ruler.h"
#include "ViewInfo.h" // for children
#include "Envelope.h"

struct Updater {
   const Ruler& mRuler;
   const ZoomInfo* zoomInfo;

   explicit Updater(const Ruler& ruler, const ZoomInfo* z)
      : mRuler{ ruler }
      , zoomInfo{ z }
   {}
   ~Updater() {}

   struct TickOutputs { Ruler::Labels& labels; Ruler::Bits& bits; wxRect& box; };
   struct UpdateOutputs {
      Ruler::Labels& majorLabels, & minorLabels, & minorMinorLabels;
      Ruler::Bits& bits;
      wxRect& box;
   };


   struct TickSizes
   {
      bool useMajor = true;

      double       mMajor;
      double       mMinor;

      int          mDigits;

      TickSizes(double UPP, int orientation, Ruler::RulerFormat format, bool log);

      TranslatableString LabelString(
         double d, Ruler::RulerFormat format, const TranslatableString& units)
         const;
   };

   double ComputeWarpedLength(const Envelope& env, double t0, double t1) const
   {
      return env.IntegralOfInverse(t0, t1);
   }

   double SolveWarpedLength(const Envelope& env, double t0, double length) const
   {
      return env.SolveIntegralOfInverse(t0, length);
   }

   bool Tick(wxDC& dc,
      int pos, double d, const TickSizes& tickSizes, wxFont font,
      TickOutputs outputs
   ) const;

   // Another tick generator for custom ruler case (noauto) .
   bool TickCustom(wxDC& dc, int labelIdx, wxFont font,
      TickOutputs outputs
   ) const;

   void BoxAdjust(
      UpdateOutputs& allOutputs
   )
      const;

   virtual void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs
   )// Envelope *speedEnv, long minSpeed, long maxSpeed )
      const = 0;
};

#endif //define __AUDACITY_UPDATER__
