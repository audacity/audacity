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
struct HitTestResult;
class LabelTrack;

class LabelGlyphHandle final : public LabelDefaultClickHandle
{
   LabelGlyphHandle(const LabelGlyphHandle&) = delete;
   static HitTestPreview HitPreview(bool hitCenter, unsigned refreshResult);

public:
   explicit LabelGlyphHandle
      (const std::shared_ptr<LabelTrack> &pLT, const wxRect &rect);

   LabelGlyphHandle &operator=(LabelGlyphHandle&&) = default;
   
   static HitTestResult HitTest
      (std::weak_ptr<LabelGlyphHandle> &holder,
       const wxMouseState &state,
       const std::shared_ptr<LabelTrack> &pLT, const wxRect &rect);

   virtual ~LabelGlyphHandle();

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

private:
   std::shared_ptr<LabelTrack> mpLT {};
   wxRect mRect {};
};

#endif
