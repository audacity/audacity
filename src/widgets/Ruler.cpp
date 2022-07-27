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

*//******************************************************************/


#include "Ruler.h"

#include <wx/dcclient.h>
#include <wx/dcscreen.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "Envelope.h"
#include "NumberScale.h"
#include "Theme.h"
#include "ViewInfo.h"

#include "Updater.h"
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
   mHasSetSpacing = false;

   mbTicksOnly = true;
   mbTicksAtExtremes = false;
   mTickColour = wxColour( theTheme.Colour( clrTrackPanelText ));
   mPen.SetColour(mTickColour);

   // Note: the font size is now adjusted automatically whenever
   // Invalidate is called on a horizontal Ruler, unless the user
   // calls SetFonts manually.  So the defaults here are not used
   // often.

   int fontSize = 10;
#ifdef __WXMSW__
   fontSize = 8;
#endif

   mbMinor = true;

   mTwoTone = false;

   mUseZoomInfo = NULL;

   // This part in particular needs inspection, not giving an error
   // But is this corret? And should it be set to NULL or nullptr if a default
   // cannot be made?
   mpUpdater = std::make_unique<LinearUpdater>( mUseZoomInfo );
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

   if (mRulerStruct.mFormat != format) {
      mRulerStruct.mFormat = format;

      Invalidate();
   }
}

void Ruler::SetUpdater(std::unique_ptr<Updater> pUpdater)
{
   // Should a comparison be made between mpUpdater and pUpdater?
   // Runtime type comparison isn't clean in c++
   mpUpdater = std::move(pUpdater);
   ResetCustomLabels(true, true, true);
   Invalidate();
}

void Ruler::SetUpdater
   (std::unique_ptr<Updater> pUpdater, int leftOffset)
{
   // Should a comparison be made between mpUpdater and pUpdater?
   // Runtime type comparison isn't clean in c++
   mpUpdater = std::move(pUpdater);

   if (mRulerStruct.mLeftOffset != leftOffset)
      mRulerStruct.mLeftOffset = leftOffset;

   // Hm, is this invalidation sufficient?  What if *zoomInfo changes under us?
   if (mUseZoomInfo != mpUpdater->zoomInfo)
      mUseZoomInfo = mpUpdater->zoomInfo;

   ResetCustomLabels(true, true, true);
   Invalidate();
}

void Ruler::SetUnits(const TranslatableString &units)
{
   // Specify the name of the units (like "dB") if you
   // want numbers like "1.6" formatted as "1.6 dB".

   if (mRulerStruct.mUnits != units) {
      mRulerStruct.mUnits = units;

      Invalidate();
   }
}

void Ruler::SetDbMirrorValue( const double d )
{
   if (mRulerStruct.mDbMirrorValue != d) {
      mRulerStruct.mDbMirrorValue = d;

      Invalidate();
   }
}

void Ruler::SetOrientation(int orient)
{
   // wxHORIZONTAL || wxVERTICAL

   if (mRulerStruct.mOrientation != orient) {
      mRulerStruct.mOrientation = orient;

      if (mRulerStruct.mOrientation == wxVERTICAL && !mHasSetSpacing)
         mRulerStruct.mSpacing = 2;

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

   if (mRulerStruct.mMin != min || mRulerStruct.mMax != max ||
      mRulerStruct.mHiddenMin != hiddenMin || mRulerStruct.mHiddenMax != hiddenMax) {
      mRulerStruct.mMin = min;
      mRulerStruct.mMax = max;
      mRulerStruct.mHiddenMin = hiddenMin;
      mRulerStruct.mHiddenMax = hiddenMax;

      Invalidate();
   }
}

void Ruler::SetSpacing(int spacing)
{
   mHasSetSpacing = true;

   if (mRulerStruct.mSpacing != spacing) {
      mRulerStruct.mSpacing = spacing;

      Invalidate();
   }
}

void Ruler::SetLabelEdges(bool labelEdges)
{
   // If this is true, the edges of the ruler will always
   // receive a label.  If not, the nearest round number is
   // labeled (which may or may not be the edge).

   if (mRulerStruct.mLabelEdges != labelEdges) {
      mRulerStruct.mLabelEdges = labelEdges;

      Invalidate();
   }
}

void Ruler::SetFlip(bool flip)
{
   // If this is true, the orientation of the tick marks
   // is reversed from the default; eg. above the line
   // instead of below

   if (mRulerStruct.mFlip != flip) {
      mRulerStruct.mFlip = flip;

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

   mpUserFonts = std::make_unique<RulerStruct::Fonts>(
      RulerStruct::Fonts{ majorFont, minorFont, minorMinorFont, 0 } );

   wxScreenDC dc;
   wxCoord height;
   FindFontHeights( height, mpUserFonts->lead, dc, majorFont );

   mRulerStruct.mpFonts.reset();
   mRulerStruct.mpFonts.reset();
   Invalidate();
}

void Ruler::SetNumberScale(const NumberScale &scale)
{
   if ( mRulerStruct.mNumberScale != scale ) {
      mRulerStruct.mNumberScale = scale;
      Invalidate();
   }
}

void Ruler::OfflimitsPixels(int start, int end)
{
   int length = mRulerStruct.mLength;
   if (mRulerStruct.mOrientation == wxHORIZONTAL)
      length = mRulerStruct.mRight - mRulerStruct.mLeft;
   else
      length = mRulerStruct.mBottom - mRulerStruct.mTop;
   if( length < 0 )
      return;

   auto size = static_cast<size_t>( length + 1 );
   if ( mUserBits.size() < size ) {
      mRulerStruct.mLength = length;
      mUserBits.resize( size, false );
   }

   if (end < start)
      std::swap( start, end );

   if (start < 0)
      start = 0;
   if (end > mRulerStruct.mLength)
      end = mRulerStruct.mLength;

   for(int i = start; i <= end; i++)
      mUserBits[i] = true;

   Invalidate();
}

void Ruler::SetBounds(int left, int top, int right, int bottom)
{
   if (mRulerStruct.mLeft != left || mRulerStruct.mTop != top ||
      mRulerStruct.mRight != right || mRulerStruct.mBottom != bottom) {
      mRulerStruct.mLeft = left;
      mRulerStruct.mTop = top;
      mRulerStruct.mRight = right;
      mRulerStruct.mBottom = bottom;

      Invalidate();
   }
}

void Ruler::Invalidate()
{
   if (mRulerStruct.mOrientation == wxHORIZONTAL)
      mRulerStruct.mLength = mRulerStruct.mRight-mRulerStruct.mLeft;
   else
      mRulerStruct.mLength = mRulerStruct.mBottom-mRulerStruct.mTop;

   mpCache.reset();
   // Bug 2316 we must preserve off-limit pixels.
   // mUserBits.clear();
}

struct Ruler::Cache {
   Updater::Bits mBits;
   Updater::Labels mMajorLabels, mMinorLabels, mMinorMinorLabels;
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
   const RulerStruct::Fonts* pUserFonts = mpUserFonts.get();
   int desiredPixelHeight = mRulerStruct.mOrientation == wxHORIZONTAL
      ? mRulerStruct.mBottom - mRulerStruct.mTop - 5 // height less ticks and 1px gap
      : MaxPixelHeight;

   if (mRulerStruct.mpFonts)
      return;

   if (pUserFonts) {
      mRulerStruct.mpFonts = std::make_unique<RulerStruct::Fonts>(*pUserFonts);
      return;
   }

   mRulerStruct.mpFonts = std::make_unique<RulerStruct::Fonts>(RulerStruct::Fonts{ {}, {}, {}, 0 });
   auto& fonts = *(mRulerStruct.mpFonts);

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
   if( mRulerStruct.mLength <= 0 )
      return;

   if (mRulerStruct.mOrientation == wxHORIZONTAL)
      cache.mRect = { 0, 0, mRulerStruct.mLength, 0 };
   else
      cache.mRect = { 0, 0, 0, mRulerStruct.mLength };

   cache.mBits = mUserBits;
   cache.mBits.resize( static_cast<size_t>(mRulerStruct.mLength + 1), false );

   cache.mMajorLabels = mCustomMajorLabels;
   cache.mMinorLabels = mCustomMinorLabels;
   cache.mMinorMinorLabels = mCustomMinorMinorLabels;
   
   Updater::UpdateOutputs allOutputs{
      cache.mMajorLabels, cache.mMinorLabels, cache.mMinorMinorLabels,
      cache.mBits, cache.mRect
   };
   if (mpUpdater != nullptr)
      mpUpdater->Update(dc, envelope, allOutputs, mRulerStruct);
}

auto Ruler::GetFonts() const -> RulerStruct::Fonts
{
   if ( !mRulerStruct.mpFonts ) {
      wxScreenDC dc;
      ChooseFonts( dc );
   }

   return *(mRulerStruct.mpFonts);
}

void Ruler::Draw(wxDC& dc) const
{
   Draw( dc, NULL);
}

void Ruler::Draw(wxDC& dc, const Envelope* envelope) const
{
   if(mRulerStruct.mLength <=0 )
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
      if (mRulerStruct.mOrientation == wxHORIZONTAL) {
         if (mRulerStruct.mFlip)
            AColor::Line(dc, mRulerStruct.mLeft, mRulerStruct.mTop, mRulerStruct.mRight, mRulerStruct.mTop);
         else
            AColor::Line(dc, mRulerStruct.mLeft, mRulerStruct.mBottom, mRulerStruct.mRight, mRulerStruct.mBottom);
      }
      else {
         if (mRulerStruct.mFlip)
            AColor::Line(dc, mRulerStruct.mLeft, mRulerStruct.mTop, mRulerStruct.mLeft, mRulerStruct.mBottom);
         else
         {
            // These calculations appear to be wrong, and to never have been used (so not tested) prior to MixerBoard.
            //    AColor::Line(dc, mRect.x-mRect.width, mTop, mRect.x-mRect.width, mBottom);
            const int nLineX = mRulerStruct.mRight - 1;
            AColor::Line(dc, nLineX, mRulerStruct.mTop, nLineX, mRulerStruct.mBottom);
         }
      }
   }

   dc.SetFont( mRulerStruct.mpFonts->major );

   // We may want to not show the ticks at the extremes,
   // though still showing the labels.
   // This gives a better look when the ruler is on a bevelled
   // button, since otherwise the tick is drawn on the bevel.
   int iMaxPos = (mRulerStruct.mOrientation==wxHORIZONTAL)? mRulerStruct.mRight : mRulerStruct.mBottom-5;

   auto drawLabel = [this, iMaxPos, &dc]( const Updater::Label &label, int length ){
      int pos = label.pos;

      if( mbTicksAtExtremes || ((pos!=0)&&(pos!=iMaxPos)))
      {
         if (mRulerStruct.mOrientation == wxHORIZONTAL) {
            if (mRulerStruct.mFlip)
               AColor::Line(dc, mRulerStruct.mLeft + pos, mRulerStruct.mTop,
                             mRulerStruct.mLeft + pos, mRulerStruct.mTop + length);
            else
               AColor::Line(dc, mRulerStruct.mLeft + pos, mRulerStruct.mBottom - length,
                             mRulerStruct.mLeft + pos, mRulerStruct.mBottom);
         }
         else {
            if (mRulerStruct.mFlip)
               AColor::Line(dc, mRulerStruct.mLeft, mRulerStruct.mTop + pos,
                             mRulerStruct.mLeft + length, mRulerStruct.mTop + pos);
            else
               AColor::Line(dc, mRulerStruct.mRight - length, mRulerStruct.mTop + pos,
                             mRulerStruct.mRight, mRulerStruct.mTop + pos);
         }
      }

      label.Draw(dc, mTwoTone, mTickColour, mRulerStruct.mpFonts);
   };

   for( const auto &label : cache.mMajorLabels )
      drawLabel( label, 4 );

   if( mbMinor ) {
      dc.SetFont( mRulerStruct.mpFonts->minor );
      for( const auto &label : cache.mMinorLabels )
         drawLabel( label, 2 );
   }

   dc.SetFont( mRulerStruct.mpFonts->minorMinor );

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
         if(mRulerStruct.mOrientation == wxHORIZONTAL) {
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
         if(mRulerStruct.mOrientation == wxHORIZONTAL) {
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
         if(mRulerStruct.mOrientation == wxHORIZONTAL) {
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

int Ruler::FindZero( const Updater::Labels &labels ) const
{
   auto begin = labels.begin(), end = labels.end(),
      iter = std::find_if( begin, end, []( const Updater::Label &label ){
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

void Ruler::ResetCustomLabels(
   bool resetMajor, bool resetMinor, bool resetMinorMinor)
{
   if (resetMajor)
      mCustomMajorLabels.clear();
   if (resetMinor)
      mCustomMinorLabels.clear();
   if (resetMinorMinor)
      mCustomMinorMinorLabels.clear();
}

void Ruler::SetCustomMajorLabels(
   const Updater::Labels& labels)
{
   const auto numLabel = labels.size();

   for(size_t i = 0; i<numLabel; i++) {
      mCustomMajorLabels.push_back(labels[i]);
   }

   Invalidate();
}

void Ruler::SetCustomMinorLabels(
   const Updater::Labels& labels)
{
   const auto numLabel = labels.size();

   for (size_t i = 0; i < numLabel; i++) {
      mCustomMinorLabels.push_back(labels[i]);
   }

   Invalidate();
}

void Ruler::SetCustomMinorMinorLabels(
   const Updater::Labels& labels)
{
   const auto numLabel = labels.size();

   for (size_t i = 0; i < numLabel; i++) {
      mCustomMinorMinorLabels.push_back(labels[i]);
   }

   Invalidate();
}
