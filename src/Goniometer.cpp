#include "Goniometer.h"

#include "AColor.h"
#include "Track.h"
#include "TrackArtist.h"
#include "TrackPanelDrawingContext.h"

#include <wx/dc.h>

// Display parameters, perhaps to be varied with preferences later
static constexpr double persistence = 1.0; // seconds
enum : size_t { nPoints = 5000 };
static constexpr bool drawLines = false;

namespace{
AttachedTrackObjects::RegisteredFactory sKey{
   []( Track &track ) {
      return std::make_shared<Goniometer>( track );
   }
};
}

Goniometer &Goniometer::Get( Track &track )
{
   return track.AttachedObjects::Get< Goniometer >( sKey );
}

Goniometer::Goniometer( Track &track )
   : mpTrack{ track.shared_from_this() }
{
}

Goniometer::~Goniometer() = default;

void Goniometer::Clear()
{
   mRecentSamples.clear();
   mLastSample = 0;
   mSampleCount = 0;
   mSampleInterval = 0;
}

void Goniometer::Reset(double sampleRate, bool)
{
   Clear();
   mRecentSamples.resize( 2 * nPoints, 0 );
   mSampleInterval = std::max< size_t >( 1,
      ( persistence * sampleRate ) / nPoints );
}

void Goniometer::Update(unsigned numChannels,
   unsigned long numFrames, const float *sampleData, bool interleaved)
{
   // TODO correct synchronization with atomics
   if ( numChannels != 2 ) {
      wxASSERT( false );
      return;
   }
   // Copy some of the samples into a circular buffer
   size_t index = mSampleInterval - mSampleCount;
   auto size = mRecentSamples.size();
   if (mSampleInterval == 0)
      return;
   const size_t majorStep = interleaved ? 2 : 1;
   const size_t minorStep = interleaved ? 1 : numFrames;
   while( index < numFrames ) {
      while( index < numFrames && mLastSample < size ) {
         auto ii = majorStep * index;
         mRecentSamples[ mLastSample++ ] = sampleData[ ii ];
         mRecentSamples[ mLastSample++ ] = sampleData[ ii + minorStep ];
         index += mSampleInterval;
      }
      if ( mLastSample >= size )
         mLastSample = 0;
   }
   mSampleCount = index - numFrames;
}

bool Goniometer::IsDisabled() const
{
   return false;
}

std::shared_ptr<Track> Goniometer::DoFindTrack()
{
   return mpTrack.lock();
}

std::vector<UIHandlePtr> Goniometer::HitTest
   (const TrackPanelMouseState &,
    const AudacityProject *)
{
   return {};
}

void Goniometer::Reparent( const std::shared_ptr<Track> &parent )
{
   mpTrack = parent;
}

void Goniometer::Draw(
   TrackPanelDrawingContext &context, const wxRect &rect, unsigned iPass )
{
   // constant drawing parameters
   // Fraction of the square reserved as margin
   static const auto q = 0.25f;

   // Colors (as in an old fashioned oscilloscope)
   static const auto &backgroundColor = *wxBLACK;
   static const auto &foregroundColor = *wxGREEN;

   // Derived parameters
   static const auto p = 1.0f - q;
   static const wxBrush backgroundBrush{ backgroundColor };
   static const wxPen backgroundPen{ backgroundColor };
   static const wxPen foregroundPen{ foregroundColor };

   static const auto foregroundRed = foregroundColor.Red(),
      foregroundBlue = foregroundColor.Blue(),
      foregroundGreen = foregroundColor.Green();
   static const auto backgroundRed = backgroundColor.Red(),
      backgroundBlue = backgroundColor.Blue(),
      backgroundGreen = backgroundColor.Green();

   if ( iPass == TrackArtist::PassTracks ) {
      auto &dc = context.dc;
      
      // Background
      dc.SetPen( backgroundPen );
      dc.SetBrush( backgroundBrush );
      dc.DrawRectangle( rect );

      // Crosshair
      wxCoord left = rect.GetLeft(),
         right = rect.GetRight(),
         top = rect.GetTop(),
         bottom = rect.GetBottom();
      wxCoord useLeft = p * left + q * right,
         useRight = p * right + q * left,
         useTop = p * top + q * bottom,
         useBottom = p * bottom + q * top;

      dc.SetPen( foregroundColor );
      AColor::Line( dc,
         useLeft, useTop,
         useRight, useBottom );
      AColor::Line( dc,
         useRight, useTop,
         useLeft, useBottom );

      // L and R labels (appropriately localized)
      // imitating code in ASlider.cpp
      int fontSize = 14;
      wxFont labelFont(
         fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
      dc.SetFont(labelFont);
      dc.SetTextBackground( backgroundColor );
      dc.SetTextForeground( foregroundColor );

      /* i18n-hint: One-letter abbreviation for Right, in the goniometer */
      auto lText = _("L");
      /* i18n-hint: One-letter abbreviation for Right, in the goniometer */
      auto rText = _("R");
   
      wxCoord textWidth, textHeight;
      dc.GetTextExtent(lText, &textWidth, &textHeight);

      wxCoord textY = std::max( top, useTop - textHeight ),
         textXL = std::max( left, useLeft - textWidth ),
         textXR = std::min( right - textWidth, useRight );
      dc.DrawText( lText, textXL, textY );
      dc.DrawText( rText, textXR, textY );

      // Dots corresponding to recent samples
      const auto nSamples = mRecentSamples.size();
      if ( nSamples < 2 )
         return;

      const wxCoord xMid = ( left + right ) / 2;
      const wxCoord yMid = ( top + bottom ) / 2;
      // Scale so that amplitude of 1 is at the end of the crosshair line
      const float scale = rect.GetWidth() * ( p - 0.5 );
      const float step = 1.0f / nSamples;
   
      auto findPoint = [&](size_t ii){
         auto leftSample = mRecentSamples[ii];
         auto rightSample = mRecentSamples[ii + 1];
         
         return wxPoint(
            0.5f + xMid - scale * (leftSample - rightSample),
            0.5f + yMid - scale * (leftSample + rightSample)
         );
      };

      wxPoint prevPoint;

      auto doSample = [&](size_t ii, size_t jj){
         const auto point = findPoint( ii );
         if (point.x < left || point.x > right)
            return;
         if (point.y < top || point.y > bottom)
            return;
         const auto s = jj * step, t = 1.0f - s;
         const wxColour color(
            s * foregroundRed +   t * backgroundRed,
            s * foregroundGreen + t * backgroundGreen,
            s * foregroundBlue +  t * backgroundBlue
         );
         dc.SetPen( wxPen{ color } );
         if (drawLines)
            AColor::Line( dc, prevPoint.x, prevPoint.y, point.x, point.y );
         else
            dc.DrawPoint( point );
         prevPoint = point;
      };

      // find oldest point
      size_t ii = mLastSample + 2;
      if (ii >= nSamples)
         ii = 0;
      prevPoint = findPoint( ii );
   
      // draw lines
      size_t jj = 2;
      for ( ii += 2; ii < nSamples; ii += 2, ++jj)
         doSample(ii, jj);
      for ( ii = 0; ii <= mLastSample; ii += 2, ++jj)
         doSample(ii, jj);
   }
}
