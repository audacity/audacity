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


\class Ruler::Label
\brief An array of these created by the Ruler is used to determine
what and where text annotations to the numbers on the Ruler get drawn.

\todo Check whether Ruler is costing too much time in allocation/free of
array of Ruler::Label.

*//******************************************************************/


#include "Ruler.h"

#include <wx/dcclient.h>
#include <wx/dcscreen.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "Envelope.h"
#include "NumberScale.h"
#include "Theme.h"
#include "ZoomInfo.h"

#include "RulerUpdater.h"
// Need to include to set default
#include "LinearUpdater.h"

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

   // This part in particular needs inspection, not giving an error
   // But is this corret? And should it be set to NULL or nullptr if a default
   // cannot be made?
   mpUpdater = std::make_unique<LinearUpdater>( *this, mUseZoomInfo );
   // mpUpdater = nullptr;
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

void Ruler::SetUpdater(std::unique_ptr<RulerUpdater> pUpdater)
{
   // Should a comparison be made between mpUpdater and pUpdater?
   // Runtime type comparison isn't clean in c++
   mpUpdater = std::move(pUpdater);
   Invalidate();
}

void Ruler::SetUpdater
   (std::unique_ptr<RulerUpdater> pUpdater, int leftOffset)
{
   // Should a comparison be made between mpUpdater and pUpdater?
   // Runtime type comparison isn't clean in c++
   mpUpdater = std::move(pUpdater);

   if (mLeftOffset != leftOffset)
      mLeftOffset = leftOffset;

   // Hm, is this invalidation sufficient?  What if *zoomInfo changes under us?
   if (mUseZoomInfo != mpUpdater->zoomInfo)
      mUseZoomInfo = mpUpdater->zoomInfo;

   Invalidate();
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

struct Ruler::Cache {
   Bits mBits;
   Labels mMajorLabels, mMinorLabels, mMinorMinorLabels;
   wxRect mRect;
};

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


void Ruler::ChooseFonts( wxDC &dc ) const
{
   const Fonts* pUserFonts = mpUserFonts.get();
   int desiredPixelHeight = mOrientation == wxHORIZONTAL
      ? mBottom - mTop - 5 // height less ticks and 1px gap
      : MaxPixelHeight;

   if (mpFonts)
      return;

   if (pUserFonts) {
      mpFonts = std::make_unique<Fonts>(*pUserFonts);
      return;
   }

   mpFonts = std::make_unique<Fonts>(Fonts{ {}, {}, {}, 0 });
   auto& fonts = *mpFonts;

   int fontSize = 4;

   desiredPixelHeight =
      std::max(MinPixelHeight, std::min(MaxPixelHeight, -desiredPixelHeight));

   // Keep making the font bigger until it's too big, then subtract one.
   wxCoord height;
   FindFontHeights(height, fonts.lead, dc, fontSize, wxFONTWEIGHT_BOLD);
   while (height <= desiredPixelHeight && fontSize < 40) {
      fontSize++;
      FindFontHeights(height, fonts.lead, dc, fontSize, wxFONTWEIGHT_BOLD);
   }
   fontSize--;
   FindFontHeights(height, fonts.lead, dc, fontSize);

   fonts.major = wxFont{ fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD };
   fonts.minor = wxFont{ fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL };
   fonts.minorMinor = wxFont{ fontSize - 1, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL };
   
}

void Ruler::UpdateCache(
   wxDC &dc, const Envelope* envelope )
   const // Envelope *speedEnv, long minSpeed, long maxSpeed )
{
   if ( mpCache )
      return;

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
   
   RulerUpdater::UpdateOutputs allOutputs{
      cache.mMajorLabels, cache.mMinorLabels, cache.mMinorMinorLabels,
      cache.mBits, cache.mRect
   };
   mpUpdater->Update(dc, envelope, allOutputs);
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
