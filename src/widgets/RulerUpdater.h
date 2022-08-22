/**********************************************************************

  Audacity: A Digital Audio Editor

  RulerUpdater.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_UPDATER__
#define __AUDACITY_UPDATER__

#include "ViewInfo.h" // for children
#include "Envelope.h"
#include "NumberScale.h" // member variable
#include "RulerFormat.h" // member variable

#include <wx/font.h>

class wxDC;
class wxColor;

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
   bool mFlip{ false };
   bool mLabelEdges{ false };

   std::unique_ptr<RulerFormat> mpRulerFormat;
   std::any mFormatData;

   int mLeft{ -1 };
   int mTop{ -1 };
   int mRight{ -1 };
   int mBottom{ -1 };
   int mLength{ 0 };

   double mDbMirrorValue{ 0.0 };


   mutable std::unique_ptr<Fonts> mpFonts;
   TranslatableString mUnits;

   NumberScale mNumberScale;
};

class RulerUpdater {
public:
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

   struct UpdateOutputs {
      Labels& majorLabels, & minorLabels, & minorMinorLabels;
      Bits& bits;
      wxRect& box;
   };

   explicit RulerUpdater() {}
   virtual ~RulerUpdater() = 0;

   virtual void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs, const RulerStruct& context, const std::any& data
   )// Envelope *speedEnv, long minSpeed, long maxSpeed )
      const = 0;

   virtual std::string Identify() const = 0;

protected:
   struct TickOutputs { Labels& labels; Bits& bits; wxRect& box; };

   struct TickSizes
   {
      bool useMajor = true;

      double       mMajor;
      double       mMinor;

      int          mDigits;

      TickSizes(
         double UPP, int orientation, const std::unique_ptr<RulerFormat>& format, bool log,
         const std::any& data
      );

      TranslatableString LabelString(
         double d, const std::unique_ptr<RulerFormat>& format,
         const std::any& data
      ) const;
   };

   static std::pair< wxRect, Label > MakeTick(
      RulerUpdater::Label lab,
      wxDC& dc, wxFont font,
      std::vector<bool>& bits,
      int left, int top, int spacing, int lead,
      bool flip, int orientation);

   void BoxAdjust(
      UpdateOutputs& allOutputs,
      const RulerStruct& context
   ) const;
};

#endif //define __AUDACITY_UPDATER__
