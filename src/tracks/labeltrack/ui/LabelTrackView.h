/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackView.h

Paul Licameli split from class LabelTrack

**********************************************************************/

#ifndef __AUDACITY_LABEL_TRACK_VIEW__
#define __AUDACITY_LABEL_TRACK_VIEW__

#include "../../ui/CommonTrackView.h"

class LabelGlyphHandle;
class LabelTextHandle;
class LabelTrack;
class SelectedRegion;

class wxKeyEvent;

class LabelTrackView final : public CommonTrackView
{
   LabelTrackView( const LabelTrackView& ) = delete;
   LabelTrackView &operator=( const LabelTrackView& ) = delete;

public:
   explicit
   LabelTrackView( const std::shared_ptr<Track> &pTrack )
      : CommonTrackView{ pTrack } {}
   ~LabelTrackView() override;

   static LabelTrackView &Get( LabelTrack& );
   static const LabelTrackView &Get( const LabelTrack& );

   bool DoCaptureKey(wxKeyEvent &event);
   bool DoKeyDown(SelectedRegion &sel, wxKeyEvent & event);
   bool DoChar(SelectedRegion &sel, wxKeyEvent & event);

private:
   std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject, int currentTool, bool bMultiTool)
      override;

   unsigned CaptureKey
     (wxKeyEvent &event, ViewInfo &viewInfo, wxWindow *pParent) override;

   unsigned KeyDown
      (wxKeyEvent &event, ViewInfo &viewInfo, wxWindow *pParent) override;

   unsigned Char
      (wxKeyEvent &event, ViewInfo &viewInfo, wxWindow *pParent) override;

   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;

   std::shared_ptr<LabelTrack> FindLabelTrack();
   std::shared_ptr<const LabelTrack> FindLabelTrack() const;

   std::weak_ptr<LabelGlyphHandle> mGlyphHandle;
   std::weak_ptr<LabelTextHandle> mTextHandle;
};

#endif
