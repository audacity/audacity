/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelResizeHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_RESIZE_HANDLE__
#define __AUDACITY_TRACK_PANEL_RESIZE_HANDLE__

#include "tracks/ui/CommonTrackPanelCell.h"
#include "UIHandle.h"

struct HitTestResult;
class Track;

class TrackPanelResizeHandle final : public UIHandle
{
   TrackPanelResizeHandle();
   TrackPanelResizeHandle(const TrackPanelResizeHandle&) = delete;
   TrackPanelResizeHandle &operator=(const TrackPanelResizeHandle&) = delete;

public:
   static TrackPanelResizeHandle& Instance();
   static HitTestPreview HitPreview(bool bLinked);

   virtual ~TrackPanelResizeHandle();

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseEvent &event,  const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   void OnProjectChange(AudacityProject *pProject) override;

private:
   enum Mode {
      IsResizing,
      IsResizingBetweenLinkedTracks,
      IsResizingBelowLinkedTracks,
   };
   Mode mMode{ IsResizing };

   Track *mpTrack{};

   bool mInitialMinimized{};
   int mInitialTrackHeight{};
   int mInitialActualHeight{};
   int mInitialUpperTrackHeight{};
   int mInitialUpperActualHeight{};

   int mMouseClickY{};
};

class TrackPanelResizerCell : public CommonTrackPanelCell
{
   TrackPanelResizerCell() {}
   TrackPanelResizerCell(const TrackPanelResizerCell&) = delete;
   TrackPanelResizerCell &operator= (const TrackPanelResizerCell&) = delete;
public:
   static TrackPanelResizerCell &Instance();

   HitTestResult HitTest
      (const TrackPanelMouseEvent &event,
       const AudacityProject *pProject) override;

   Track *FindTrack() override { return mpTrack; };
private:
   friend class TrackPanel;
   Track *mpTrack {};
   bool mBetweenTracks {};
};

#endif
