/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackView.cpp

Paul Licameli split from class TrackView

**********************************************************************/

#include "CommonTrackView.h"

#include "BackgroundCell.h"
#include "TimeShiftHandle.h"
#include "TrackControls.h"
#include "ZoomHandle.h"
#include "../ui/SelectHandle.h"
#include "../../AColor.h"
#include "../../AllThemeResources.h"
#include "../../ProjectSettings.h"
#include "../../Track.h"
#include "../../TrackArtist.h"
#include "../../TrackInfo.h"
#include "../../TrackPanelDrawingContext.h"
#include "../../TrackPanelMouseEvent.h"

#include <wx/dc.h>
#include <wx/graphics.h>

std::vector<UIHandlePtr> CommonTrackView::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject)
{
   UIHandlePtr result;
   using namespace ToolCodes;
   std::vector<UIHandlePtr> results;
   const auto &settings = ProjectSettings::Get( *pProject );
   const auto currentTool = settings.GetTool();
   const bool isMultiTool = ( currentTool == multiTool );

   if ( !isMultiTool && currentTool == zoomTool ) {
      // Zoom tool is a non-selecting tool that takes precedence in all tracks
      // over all other tools, no matter what detail you point at.
      result = ZoomHandle::HitAnywhere(
         BackgroundCell::Get( *pProject ).mZoomHandle );
      results.push_back(result);
      return results;
   }

   // In other tools, let subclasses determine detailed hits.
   results =
      DetailedHitTest( st, pProject, currentTool, isMultiTool );

   // There are still some general cases.

   // Sliding applies in more than one track type.
   if ( !isMultiTool && currentTool == slideTool ) {
      result = TimeShiftHandle::HitAnywhere(
         mTimeShiftHandle, FindTrack(), false);
      if (result)
         results.push_back(result);
   }

   // Let the multi-tool right-click handler apply only in default of all
   // other detailed hits.
   if ( isMultiTool ) {
      result = ZoomHandle::HitTest(
         BackgroundCell::Get( *pProject ).mZoomHandle, st.state);
      if (result)
         results.push_back(result);
   }

   // Finally, default of all is adjustment of the selection box.
   if ( isMultiTool || currentTool == selectTool ) {
      result = SelectHandle::HitTest(
         mSelectHandle, st, pProject, FindTrack() );
      if (result)
         results.push_back(result);
   }

   return results;
}

// Draws the track name on the track, if it is needed.
static void DrawTrackName(
   TrackPanelDrawingContext &context, const Track * t, const wxRect & rect )
{
   auto name = t->GetName();
   if( name.IsEmpty())
      return;
   if( !t->IsLeader())
      return;
   auto &dc = context.dc;
   wxBrush Brush;
   wxCoord x,y;
   wxFont labelFont(12, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   dc.SetFont(labelFont);
   dc.GetTextExtent( t->GetName(), &x, &y );

   // Logic for name background translucency (aka 'shields')
   // Tracks less than kOpaqueHeight high will have opaque shields.
   // Tracks more than kTranslucentHeight will have maximum translucency for shields.
   const int kOpaqueHeight = 44;
   const int kTranslucentHeight = 124;
   int h = rect.GetHeight();
   // f codes the opacity as a number between 0.0 and 1.0
   float f= wxClip((h -kOpaqueHeight)/(float)(kTranslucentHeight-kOpaqueHeight),0.0,1.0);
   // kOpaque is the shield's alpha for tracks that are not tall
   // kTranslucent is the shield's alpha for tracks that are tall.
   const int kOpaque = 255;
   const int kTranslucent = 140;
   // 0.0 maps to full opacity, 1.0 maps to full translucency.
   int opacity = 255 - (255-140)*f;

#ifdef __WXMAC__
   // Mac dc is a graphics dc already.
   AColor::UseThemeColour( &dc, clrTrackInfoSelected, clrTrackPanelText, opacity );
   dc.DrawRoundedRectangle( rect.x+7, rect.y+1,  x+16, y+4, 8.0 );
#else
   // This little dance with wxImage in order to draw to a graphic dc
   // which we can then paste as a translucent bitmap onto the real dc.
   wxImage image( x+18, y+6 );
   image.InitAlpha();
   unsigned char *alpha=image.GetAlpha();
   memset(alpha, wxIMAGE_ALPHA_TRANSPARENT, image.GetWidth()*image.GetHeight());
   
   {
      std::unique_ptr< wxGraphicsContext >
         pGc{ wxGraphicsContext::Create(image) };
      auto &gc = *pGc;
      // This is to a gc, not a dc.
      AColor::UseThemeColour( &gc, clrTrackInfoSelected, clrTrackPanelText, opacity );
      // Draw at 1,1, not at 0,0 to avoid clipping of the antialiasing.
      gc.DrawRoundedRectangle( 1, 1,  x+16, y+4, 8.0 );
      // destructor of gc updates the wxImage.
   }
   wxBitmap bitmap( image );
   dc.DrawBitmap( bitmap, rect.x+6, rect.y);
#endif
   dc.SetTextForeground(theTheme.Colour( clrTrackPanelText ));
   dc.DrawText (t->GetName(), rect.x+15, rect.y+3);  // move right 15 pixels to avoid overwriting <- symbol
}

void CommonTrackView::Draw(
   TrackPanelDrawingContext &context, const wxRect &rect, unsigned iPass )
{
   // This overpaints only the track area, so any pass after tracks is late
   // enough.
   if ( iPass == TrackArtist::PassMargins )
      DrawTrackName(
         context, FindTrack()->SubstitutePendingChangedTrack().get(), rect );
}

std::shared_ptr<TrackPanelCell> CommonTrackView::ContextMenuDelegate()
{
   return TrackControls::Get( *FindTrack() ).shared_from_this();
}

int CommonTrackView::GetMinimizedHeight() const
{
   auto height = TrackInfo::MinimumTrackHeight();
   const auto pTrack = FindTrack();
   auto channels = TrackList::Channels(pTrack->SubstituteOriginalTrack().get());
   auto nChannels = channels.size();
   auto begin = channels.begin();
   auto index =
      std::distance(begin, std::find(begin, channels.end(), pTrack.get()));
   return (height * (index + 1) / nChannels) - (height * index / nChannels);
}
