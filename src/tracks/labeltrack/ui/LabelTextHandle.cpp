/**********************************************************************

Audacity: A Digital Audio Editor

LabelTextHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "LabelTextHandle.h"

#include "LabelTrackView.h"

#include "../../../HitTestResult.h"
#include "../../../LabelTrack.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "UndoManager.h"
#include "../../../RefreshCode.h"
#include "SelectionState.h"
#include "../../../TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "../../../../images/Cursors.h"

#include "../../ui/TextEditHelper.h"

#include <wx/clipbrd.h>

LabelTextHandle::LabelTextHandle
( const std::shared_ptr<LabelTrack> &pLT, int labelNum )
   : mpLT{ pLT }
   , mLabelNum{ labelNum }
{
}

void LabelTextHandle::Enter(bool, AudacityProject *)
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

HitTestPreview LabelTextHandle::HitPreview()
{
   static auto ibeamCursor =
      ::MakeCursor(wxCURSOR_IBEAM, IBeamCursorXpm, 17, 16);
   return {
      XO("Click to edit label text"),
      ibeamCursor.get()
   };
}

UIHandlePtr LabelTextHandle::HitTest
(std::weak_ptr<LabelTextHandle> &holder,
 const wxMouseState &state, const std::shared_ptr<LabelTrack> &pLT)
{
   // If Control is down, let the select handle be hit instead
   int labelNum;
   if (!state.ControlDown() &&
       (labelNum =
          LabelTrackView::OverATextBox(*pLT, state.m_x, state.m_y) ) >= 0) {
      auto result = std::make_shared<LabelTextHandle>( pLT, labelNum );
      result = AssignUIHandlePtr(holder, result);
      return result;
   }

   return {};
}

LabelTextHandle::~LabelTextHandle()
{
}

void LabelTextHandle::HandleTextClick(AudacityProject &project, const wxMouseEvent & evt)
{
   auto pTrack = mpLT.lock();
   if (!pTrack)
      return;

   auto &view = LabelTrackView::Get( *pTrack );
   if (evt.ButtonDown())
   {
      const auto selIndex = LabelTrackView::OverATextBox( *pTrack, evt.m_x, evt.m_y );
      if ((evt.LeftDown() || evt.RightDown()) && selIndex != -1 ) 
      {
         auto useDialog{false};
         gPrefs->Read(wxT("/GUI/DialogForNameNewLabel"), &useDialog, false);
         auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
         if (useDialog) {
            wxString title;
            auto label = *pTrack->GetLabel(selIndex);
            if (LabelTrackView::DialogForLabelName(
               project, selectedRegion, label.title, title) ==
                wxID_CANCEL) {
               return;
            }
            if(label.title != title)
            {
               ProjectHistory::Get(project).PushState(XO("Modified Label"),
                  XO("Label Edit"),
                  UndoPush::CONSOLIDATE);
               label.title = title;
               pTrack->SetLabel(selIndex, label);
            }
            return;
         }
         mTextEditHelper = view.StartLabelTextEdit(selIndex, &project);
         mTextEditHelper->OnClick(evt, &project);
      }
#if defined(__WXGTK__) && (HAVE_GTK)
      if (evt.MiddleDown()) {
         // Paste text, making a NEW label if none is selected.
         wxTheClipboard->UsePrimarySelection(true);
         view.PasteSelectedText(project, newSel.t0(), newSel.t1());
         wxTheClipboard->UsePrimarySelection(false);
      }
#endif
   }
}

bool LabelTextHandle::HandlesRightClick()
{
   return true;
}

UIHandle::Result LabelTextHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto pLT = mpLT.lock();
   if (!pLT)
      return RefreshCode::Cancelled;

   auto result = LabelDefaultClickHandle::Click( evt, pProject );

   const wxMouseEvent &event = evt.event;
   auto &viewInfo = ViewInfo::Get( *pProject );

   HandleTextClick(*pProject, event);

   return result | RefreshCode::RefreshCell;
}

UIHandle::Result LabelTextHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto &project = *pProject;
   using namespace RefreshCode;
   auto result = LabelDefaultClickHandle::Drag( evt, pProject );
   if (mTextEditHelper && mTextEditHelper->OnDrag(evt.event, pProject))
       result |= RefreshCode::RefreshCell;
   return result;
}

HitTestPreview LabelTextHandle::Preview
(const TrackPanelMouseState &, AudacityProject *)
{
   return HitPreview();
}

UIHandle::Result LabelTextHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   auto result = LabelDefaultClickHandle::Release( evt, pProject, pParent );
   if (mTextEditHelper && mTextEditHelper->OnRelease(evt.event, pProject))
       result |= RefreshCode::RefreshCell;

   return result;
}

UIHandle::Result LabelTextHandle::Cancel( AudacityProject *pProject )
{
   auto result = LabelDefaultClickHandle::Cancel( pProject );
   return result | RefreshCode::RefreshAll;
}
