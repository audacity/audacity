/**********************************************************************

 Audacity: A Digital Audio Editor

 TrackPanelResizerCell.h

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_RESIZER_CELL__
#define __AUDACITY_TRACK_PANEL_RESIZER_CELL__

#include "ClientData.h" // to inherit
#include "tracks/ui/CommonTrackPanelCell.h" // to inherit

class Track;
class TrackPanelResizeHandle;

class TrackPanelResizerCell
   : public CommonTrackPanelCell
   , public std::enable_shared_from_this< TrackPanelResizerCell >
   , public ClientData::Base
{
   TrackPanelResizerCell(const TrackPanelResizerCell&) = delete;
   TrackPanelResizerCell &operator= (const TrackPanelResizerCell&) = delete;
public:

   static TrackPanelResizerCell &Get( Track &track );
   static const TrackPanelResizerCell &Get( const Track &track );

   explicit
   TrackPanelResizerCell( const std::shared_ptr<Track> &pTrack );

   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &, const AudacityProject *) override;

protected:
   std::shared_ptr<Track> DoFindTrack() override;

private:
   // back-pointer is weak to break a cycle
   std::weak_ptr<Track> mwTrack;

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   std::weak_ptr<TrackPanelResizeHandle> mResizeHandle;
};

#endif
