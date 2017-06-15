/**********************************************************************

Audacity: A Digital Audio Editor

LabelGlyphHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_GLYPH_HANDLE__
#define __AUDACITY_LABEL_GLYPH_HANDLE__

#include "../../../UIHandle.h"
#include <wx/gdicmn.h>

class wxMouseEvent;
struct HitTestResult;
class LabelTrack;

class LabelGlyphHandle final : public UIHandle
{
   LabelGlyphHandle();
   LabelGlyphHandle(const LabelGlyphHandle&) = delete;
   LabelGlyphHandle &operator=(const LabelGlyphHandle&) = delete;
   static LabelGlyphHandle& Instance();
   static HitTestPreview HitPreview(bool hitCenter, unsigned refreshResult);

public:
   static HitTestResult HitTest
      (const wxMouseEvent &event, LabelTrack *pLT);

   virtual ~LabelGlyphHandle();

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseEvent &event, const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   bool StopsOnKeystroke() override { return true; }

private:
   LabelTrack *mpLT {};
   wxRect mRect {};
};

#endif
