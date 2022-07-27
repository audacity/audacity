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
   struct Fonts {
      wxFont major, minor, minorMinor;
      int lead;
   };

   double mMin{ 0.0 };
   double mHiddenMin{ 0.0 };
   double mMax{ 100.0 };
   double mHiddenMax{ 100.0 };

   int mOrientation{ wxHORIZONTAL };
   int mSpacing{ 6 };
   RulerFormat mFormat{ RealFormat };
   bool mFlip{ false };
   bool mLabelEdges{ false };

   int mLeft{ -1 };
   int mTop{ -1 };
   int mRight{ -1 };
   int mBottom{ -1 };
   int mLength{ 0 };

   double mDbMirrorValue{ 0.0 };


   mutable std::unique_ptr<Fonts> mpFonts;
   TranslatableString mUnits;

   int mLeftOffset;

   NumberScale mNumberScale;
};

struct Updater {

   struct Label {
      double value;
      int pos;
      int lx, ly;
      TranslatableString text;
      TranslatableString units;

      void Draw(wxDC& dc, bool twoTone, wxColour c,
         std::unique_ptr<RulerStruct::Fonts>& fonts) const;
   };
   using Labels = std::vector<Label>;

   using Bits = std::vector< bool >;

   const ZoomInfo* zoomInfo;

   explicit Updater(const ZoomInfo* z = nullptr)
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
         double d, RulerFormat format)
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
