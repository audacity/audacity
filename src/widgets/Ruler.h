/**********************************************************************

  Audacity: A Digital Audio Editor

  Ruler.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_RULER__
#define __AUDACITY_RULER__

#include "NumberScale.h" // member variable

#include <wx/colour.h> // member variable
#include <wx/font.h> // member variable
#include <wx/pen.h> // member variable

class wxDC;

class Envelope;
class ZoomInfo;
class RulerUpdater;

#include "TranslatableString.h"

class AUDACITY_DLL_API Ruler {
 public:

   enum RulerFormat {
      IntFormat,
      RealFormat,
      RealLogFormat,
      TimeFormat,
      LinearDBFormat,
   };

   //
   // Constructor / Destructor
   //

   Ruler();
   ~Ruler();

   //
   // Required Ruler Parameters
   //

   void SetBounds(int left, int top, int right, int bottom);

   // wxHORIZONTAL || wxVERTICAL
   void SetOrientation(int orient);

   // min is the value at (x, y)
   // max is the value at (x+width, y+height)
   // (at the center of the pixel, in both cases)
   void SetRange(double min, double max);

   // An overload needed for the special case of fisheye
   // min is the value at (x, y)
   // max is the value at (x+width, y+height)
   // hiddenMin, hiddenMax are the values that would be shown without the fisheye.
   // (at the center of the pixel, in both cases)
   void SetRange(double min, double max, double hiddenMin, double hiddenMax);

   // Set the kind of updater the ruler will use
   // (Linear, Logarithmic, Custom, etc.)
   void SetUpdater(std::unique_ptr<RulerUpdater> pUpdater);

   // An overload to replace SetUseZoomInfo
   void SetUpdater(std::unique_ptr<RulerUpdater> pUpdater, int leftOffset);

   //
   // Optional Ruler Parameters
   //

   // If twoTone is true, cause zero and positive numbers to appear black, negative in another color.
   void SetTwoTone(bool twoTone);

   // IntFormat, RealFormat, or TimeFormat
   void SetFormat(RulerFormat format);

   // Specify the name of the units (like "dB") if you
   // want numbers like "1.6" formatted as "1.6 dB".
   void SetUnits(const TranslatableString &units);
   void SetDbMirrorValue( const double d );

   // Minimum number of pixels between labels
   void SetSpacing(int spacing);

   // If this is true, the edges of the ruler will always
   // receive a label.  If not, the nearest round number is
   // labeled (which may or may not be the edge).
   void SetLabelEdges(bool labelEdges);

   // Makes a vertical ruler hug the left side (instead of right)
   // and a horizontal ruler hug the top (instead of bottom)
   void SetFlip(bool flip);

   // Set it to false if you don't want minor labels.
   void SetMinor(bool value);

   // Good defaults are provided, but you can override here
   void SetFonts(const wxFont &minorFont, const wxFont &majorFont, const wxFont &minorMinorFont);
   struct Fonts {
      wxFont major, minor, minorMinor;
      int lead;
   };
   Fonts GetFonts() const;

   void SetNumberScale(const NumberScale &scale);

   // The ruler will not draw text within this (pixel) range.
   // Use this if you have another graphic object obscuring part
   // of the ruler's area.  The values start and end are interpreted
   // relative to the Ruler's local coordinates.
   void OfflimitsPixels(int start, int end);

   //
   // Calculates and returns the maximum size required by the ruler
   //
   void GetMaxSize(wxCoord *width, wxCoord *height);


   // The following functions should allow a custom ruler setup:
   // autosize is a GREAT thing, but for some applications it's
   // useful the definition of a label array and label step by
   // the user.
   void SetCustomMode(bool value);
   // If this is the case, you should provide an array of labels, start
   // label position, and labels step. The range eventually specified will be
   // ignored.
   void SetCustomMajorLabels(
      const TranslatableStrings &labels, int start, int step);
   void SetCustomMinorLabels(
      const TranslatableStrings &labels, int start, int step);

   //
   // Drawing
   //

   // Note that it will not erase for you...
   void Draw(wxDC& dc) const;
   void Draw(wxDC& dc, const Envelope* envelope) const;
   // If length <> 0, draws lines perpendiculars to ruler corresponding
   // to selected ticks (major, minor, or both), in an adjacent window.
   // You may need to use the offsets if you are using part of the dc for rulers, borders etc.
   void DrawGrid(wxDC& dc, int length, bool minor = true, bool major = true, int xOffset = 0, int yOffset = 0) const;

   // So we can have white ticks on black...
   void SetTickColour( const wxColour & colour)
   { mTickColour = colour; mPen.SetColour( colour );}

   // Force regeneration of labels at next draw time
   void Invalidate();

 private:
   friend class RulerUpdater;
   friend class LinearUpdater;
   friend class LogarithmicUpdater;
   friend class CustomUpdater;

   class Label {
    public:
      double value;
      int pos;
      int lx, ly;
      TranslatableString text;

      void Draw(wxDC &dc, bool twoTone, wxColour c) const;
   };
   using Labels = std::vector<Label>;

   using Bits = std::vector< bool >;

   void ChooseFonts( wxDC &dc ) const;

   void UpdateCache( wxDC &dc, const Envelope* envelope ) const;
   
public:
   bool mbTicksOnly; // true => no line the length of the ruler
   bool mbTicksAtExtremes;

private:
   wxColour mTickColour;
   wxPen mPen;

   int          mLeft, mTop, mRight, mBottom;
   int          mLength;

   std::unique_ptr<Fonts> mpUserFonts;
   mutable std::unique_ptr<Fonts> mpFonts;

   std::unique_ptr<RulerUpdater> mpUpdater;

   double       mMin, mMax;
   double       mHiddenMin, mHiddenMax;

   Bits mUserBits;

   static std::pair< wxRect, Label > MakeTick(
      Label lab,
      wxDC &dc, wxFont font,
      std::vector<bool> &bits,
      int left, int top, int spacing, int lead,
      bool flip, int orientation );

   struct Cache;
   mutable std::unique_ptr<Cache> mpCache;

   // Returns 'zero' label coordinate (for grid drawing)
   int FindZero( const Labels &labels ) const;

   int GetZeroPosition() const;

   int          mOrientation;
   int          mSpacing;
   double       mDbMirrorValue;
   bool         mHasSetSpacing;
   bool         mLabelEdges;
   RulerFormat  mFormat;
   bool         mLog;
   bool         mFlip;
   bool         mCustom;
   bool         mbMinor;
   TranslatableString mUnits;
   bool         mTwoTone;
   const ZoomInfo *mUseZoomInfo;
   int          mLeftOffset;

   NumberScale mNumberScale;
};

#endif //define __AUDACITY_RULER__
