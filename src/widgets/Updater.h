/**********************************************************************

  Audacity: A Digital Audio Editor

  Updater.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_UPDATER__
#define __AUDACITY_UPDATER__

#include "ViewInfo.h" // for children
#include "Envelope.h"
#include "NumberScale.h" // member variable
#include <wx/font.h>

class wxDC;
class wxColor;

enum RulerFormat {
   IntFormat,
   RealFormat,
   RealLogFormat,
   TimeFormat,
   LinearDBFormat,
};

struct RulerStruct {
   RulerStruct();

   struct Fonts {
      wxFont major, minor, minorMinor;
      int lead;
   };

   int          mLeft, mTop, mRight, mBottom;
   int          mLength;

   double       mMin, mMax;
   double       mHiddenMin, mHiddenMax;

   mutable std::unique_ptr<Fonts> mpFonts;
   TranslatableString mUnits;

   int          mSpacing;
   int          mOrientation;
   double       mDbMirrorValue;
   RulerFormat  mFormat;
   bool         mFlip;
   bool         mLabelEdges;
   int          mLeftOffset;

   NumberScale mNumberScale;
};

struct Updater {

   struct Label {
      double value;
      int pos;
      int lx, ly;
      TranslatableString text;

      void Draw(wxDC& dc, bool twoTone, wxColour c) const;
   };
   using Labels = std::vector<Label>;

   using Bits = std::vector< bool >;

   const ZoomInfo* zoomInfo;

   explicit Updater(const ZoomInfo* z)
      : zoomInfo{ z }
   {}
   virtual ~Updater() = 0;

   struct TickOutputs { Labels& labels; Bits& bits; wxRect& box; };
   struct UpdateOutputs {
      Labels& majorLabels, & minorLabels, & minorMinorLabels;
      Bits& bits;
      wxRect& box;
   };

   struct TickSizes
   {
      bool useMajor = true;

      double       mMajor;
      double       mMinor;

      int          mDigits;

      TickSizes(double UPP, int orientation, RulerFormat format, bool log);

      TranslatableString LabelString(
         double d, RulerFormat format, const TranslatableString& units)
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

   static std::pair< wxRect, Label > MakeTick(
      Updater::Label lab,
      wxDC& dc, wxFont font,
      std::vector<bool>& bits,
      int left, int top, int spacing, int lead,
      bool flip, int orientation);

   bool Tick(wxDC& dc,
      int pos, double d, const TickSizes& tickSizes, wxFont font,
      TickOutputs outputs,
      const RulerStruct& context
   ) const;

   void BoxAdjust(
      UpdateOutputs& allOutputs,
      const RulerStruct& context
   )
      const;

   virtual void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs, const RulerStruct &context
   )// Envelope *speedEnv, long minSpeed, long maxSpeed )
      const = 0;
};

#endif //define __AUDACITY_UPDATER__
