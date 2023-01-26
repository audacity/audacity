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
*//***************************************************************//**

\class RulerUpdater::Label
\brief An array of these created by the Updater is used to determine
what and where text annotations to the numbers on the Ruler get drawn.

\todo Check whether RulerUpdater is costing too much time in allocation/free of
array of RulerUpdater::Label.

*//******************************************************************/

#include "RulerUpdater.h"

#include "AllThemeResources.h"
#include "Theme.h"

#include <wx/dc.h>

RulerUpdater::TickSizes::TickSizes(
   double UPP, int orientation, const std::unique_ptr<RulerFormat>& format, bool log,
   const std::any& data)
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

      // As a heuristic, we want at least 22 pixels between each 
      // minor tick.  We want to show numbers like "-48"
      // in that space.
      // If vertical, we don't need as much space.
      double units = ((orientation == wxHORIZONTAL) ? 22 : 16) * fabs(UPP);

      mDigits = 0;

      format->SetTickSizes(units, mMajor, mMinor, mMinorMinor, mDigits, data);
   }

TranslatableString RulerUpdater::TickSizes::LabelString(
   double d, const std::unique_ptr<RulerFormat>& format,
   const std::any& data) const
   {
      // Given a value, turn it into a string according
      // to the current ruler format.  The number of digits of
      // accuracy depends on the resolution of the ruler,
      // i.e. how far zoomed in or out you are.

      wxString s;

      // PRL Todo: are all these cases properly localized?  (Decimal points,
      // hour-minute-second, etc.?)

      // Replace -0 with 0
      if (d < 0.0 && (d + mMinor > 0.0) && (format->Identify() != "RealLogFormat"))
         d = 0.0;

      format->SetLabelString(s, d, mMinor, mDigits, tickType, data);

      auto result = Verbatim(s);

      return result;
 }

void RulerUpdater::Label::Draw(
   wxDC& dc, bool twoTone, wxColour c,
   std::unique_ptr<RulerStruct::Fonts>& fonts) const
{
   if (!text.empty()) {
      bool altColor = twoTone && value < 0.0;

#ifdef EXPERIMENTAL_THEMING
      dc.SetTextForeground(altColor ? theTheme.Colour(clrTextNegativeNumbers) : c);
#else
      dc.SetTextForeground(altColor ? *wxBLUE : *wxBLACK);
#endif
      dc.SetBackgroundMode(wxTRANSPARENT);
      // Do not draw units as bolded
      if (dc.GetFont() == fonts->major) {
         dc.DrawText(text.Translation(), lx, ly);
         wxSize textSize = dc.GetTextExtent(text.Translation());
         dc.SetFont(fonts->minor);
         int unitX = lx + textSize.GetWidth();
         dc.DrawText(units.Translation(), unitX, ly);
         dc.SetFont(fonts->major);
      }
      else {
         auto str = text + units;
         dc.DrawText(str.Translation(), lx, ly);
      }
   }
}

auto RulerUpdater::MakeTick(
   Label lab,
   wxDC& dc, wxFont font,
   std::vector<bool>& bits,
   int left, int top, int spacing, int lead,
   bool flip, int orientation)
   -> std::pair< wxRect, Label >
{
   lab.lx = left - 1000; // don't display
   lab.ly = top - 1000;  // don't display

   auto length = bits.size() - 1;
   auto pos = lab.pos;

   dc.SetFont(font);

   wxCoord strW, strH, strD, strL;
   auto strText = lab.text;
   auto strUnits = lab.units;
   auto str = strText + strUnits;
   // Do not put the text into results until we are sure it does not overlap
   lab.text = {};
   lab.units = {};
   dc.GetTextExtent(str.Translation(), &strW, &strH, &strD, &strL);

   int strPos, strLen, strLeft, strTop;
   if (orientation == wxHORIZONTAL) {
      strLen = strW;
      strPos = pos - strW / 2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strW >= length)
         strPos = length - strW;
      strLeft = left + strPos;
      if (flip)
         strTop = top + 4;
      else
         strTop = -strH - lead;
      //         strTop = top - lead + 4;// More space was needed...
   }
   else {
      strLen = strH;
      strPos = pos - strH / 2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strH >= length)
         strPos = length - strH;
      strTop = top + strPos;
      if (flip)
         strLeft = left + 5;
      else
         strLeft = -strW - 6;
   }

   // FIXME: we shouldn't even get here if strPos < 0.
   // Ruler code currently does  not handle very small or
   // negative sized windows (i.e. don't draw) properly.
   if (strPos < 0)
      return { {}, lab };

   // See if any of the pixels we need to draw this
   // label is already covered

   int i;
   for (i = 0; i < strLen; i++)
      if (bits[strPos + i])
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

   for (i = 0; i < strLen; i++)
      bits[strPos + i] = true;

   // Good to display the text
   lab.text = strText;
   lab.units = strUnits;
   return { { strLeft, strTop, strW, strH }, lab };
}

void RulerUpdater::BoxAdjust(
   UpdateOutputs& allOutputs,
   const RulerStruct& context
)
const
{
   const int mLeft = context.mLeft;
   const int mTop = context.mTop;
   const int mBottom = context.mBottom;
   const int mRight = context.mRight;
   const int mOrientation = context.mOrientation;
   const bool mFlip = context.mFlip;

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
   auto update = [=](Label& label) {
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

RulerUpdater::~RulerUpdater() = default;
