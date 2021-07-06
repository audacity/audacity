/**********************************************************************

Sneedacity: A Digital Audio Editor

TrackPanelResizeHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __SNEEDACITY_TRACK_PANEL_RESIZE_HANDLE__
#define __SNEEDACITY_TRACK_PANEL_RESIZE_HANDLE__

#include "UIHandle.h"

class Track;

class TrackPanelResizeHandle final : public UIHandle
{
   TrackPanelResizeHandle(const TrackPanelResizeHandle&) = delete;

public:
   explicit TrackPanelResizeHandle( const std::shared_ptr<Track> &pTrack, int y );

   TrackPanelResizeHandle &operator=(const TrackPanelResizeHandle&) = default;

   static HitTestPreview HitPreview(bool bLinked);

   virtual ~TrackPanelResizeHandle();

   std::shared_ptr<Track> GetTrack() const { return mpTrack.lock(); }

   Result Click
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, SneedacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(SneedacityProject *pProject) override;

private:
   enum Mode {
      IsResizing,
      IsResizingBetweenLinkedTracks,
      IsResizingBelowLinkedTracks,
   };
   Mode mMode{ IsResizing };

   std::weak_ptr<Track> mpTrack;

   bool mInitialMinimized{};
   int mInitialTrackHeight{};
   int mInitialActualHeight{};
   int mInitialUpperTrackHeight{};
   int mInitialUpperActualHeight{};

   int mMouseClickY{};
};

#endif
