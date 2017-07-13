/**********************************************************************

Audacity: A Digital Audio Editor

LabelGlyphHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_GLYPH_HANDLE__
#define __AUDACITY_LABEL_GLYPH_HANDLE__

#include "LabelDefaultClickHandle.h"
#include <wx/gdicmn.h>
#include "../../../MemoryX.h"

class wxMouseState;
class LabelTrack;

/// mEdge:
/// 0 if not over a glyph,
/// else a bitwise or of :
/// 1 if over the left-hand glyph,
/// 2 if over the right-hand glyph on a label,
/// 4 if over center.
///
///   mMouseLabelLeft - index of any left label hit
///   mMouseLabelRight - index of any right label hit
///
struct LabelTrackHit {
   int mEdge{};
   int mMouseOverLabelLeft{ -1 };    /// Keeps track of which left label the mouse is currently over.
   int mMouseOverLabelRight{ -1 };   /// Keeps track of which right label the mouse is currently over.
   bool mbIsMoving {};
   bool mIsAdjustingLabel {};
};

class LabelGlyphHandle final : public LabelDefaultClickHandle
{
   static HitTestPreview HitPreview(bool hitCenter);

public:
   explicit LabelGlyphHandle
      (const std::shared_ptr<LabelTrack> &pLT,
       const wxRect &rect, const LabelTrackHit &hit);

   LabelGlyphHandle &operator=(const LabelGlyphHandle&) = default;
   
   static UIHandlePtr HitTest
      (std::weak_ptr<LabelGlyphHandle> &holder,
       const wxMouseState &state,
       const std::shared_ptr<LabelTrack> &pLT, const wxRect &rect);

   virtual ~LabelGlyphHandle();

   void Enter(bool forward) override;

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   bool StopsOnKeystroke() override { return true; }

   LabelTrackHit mHit{};

   static UIHandle::Result NeedChangeHighlight
      (const LabelGlyphHandle &oldState, const LabelGlyphHandle &newState);

private:
   std::shared_ptr<LabelTrack> mpLT {};
   wxRect mRect {};
};

#endif
