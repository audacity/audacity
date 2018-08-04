/**********************************************************************

 Audacity: A Digital Audio Editor

 TrackPanel.h

 Paul Licameli

 **********************************************************************/

#ifndef __AUDACITY_CELLULAR_PANEL__
#define __AUDACITY_CELLULAR_PANEL__

#include <wx/cursor.h>
#include "widgets/OverlayPanel.h"

class ViewInfo;
class AudacityProject;

class TrackPanelCell;
struct TrackPanelMouseEvent;
struct TrackPanelMouseState;

class UIHandle;
using UIHandlePtr = std::shared_ptr<UIHandle>;

// This class manages a panel divided into a number of sub-rectangles called
// cells, that each implement hit tests returning click-drag-release handler
// objects, and other services.
// It has no dependency on the Track class.
class AUDACITY_DLL_API CellularPanel : public OverlayPanel {
public:
   CellularPanel(wxWindow * parent, wxWindowID id,
                 const wxPoint & pos,
                 const wxSize & size,
                 ViewInfo *viewInfo,
                 // default as for wxPanel:
                 long style = wxTAB_TRAVERSAL | wxNO_BORDER)
   : OverlayPanel(parent, id, pos, size, style)
   , mViewInfo( viewInfo )
   {}
   
   // Overridables:
   
   virtual AudacityProject *GetProject() const = 0;
   
   // Find track info by coordinate
   struct FoundCell {
      std::shared_ptr<TrackPanelCell> pCell;
      wxRect rect;
   };
   virtual FoundCell FindCell(int mouseX, int mouseY) = 0;
   virtual wxRect FindRect(const TrackPanelCell &cell) = 0;
   virtual TrackPanelCell *GetFocusedCell() = 0;
   virtual void SetFocusedCell() = 0;
   
   virtual void ProcessUIHandleResult
   (TrackPanelCell *pClickedCell, TrackPanelCell *pLatestCell,
    unsigned refreshResult) = 0;
   
   virtual void UpdateStatusMessage( const wxString & )  = 0;
   
public:
   UIHandlePtr Target()
   {
      if (mTargets.size())
         return mTargets[mTarget];
      else
         return {};
   }
   
   bool IsMouseCaptured();
   
   wxCoord MostRecentXCoord() const { return mMouseMostRecentX; }
   
   void HandleCursorForPresentMouseState(bool doHit = true);
   
protected:
   bool HasEscape();
   bool CancelDragging();
   void DoContextMenu( TrackPanelCell *pCell = nullptr );
   void ClearTargets()
   {
      // Forget the rotation of hit test candidates when the mouse moves from
      // cell to cell or outside of the panel entirely.
      mLastCell.reset();
      mTargets.clear();
      mTarget = 0;
      mMouseOverUpdateFlags = 0;
   }
   
private:
   bool HasRotation();
   bool ChangeTarget(bool forward, bool cycle);
   
   void OnMouseEvent(wxMouseEvent & event);
   void OnCaptureLost(wxMouseCaptureLostEvent & event);
   void OnCaptureKey(wxCommandEvent & event);
   void OnKeyDown(wxKeyEvent & event);
   void OnChar(wxKeyEvent & event);
   void OnKeyUp(wxKeyEvent & event);
   
   void OnSetFocus(wxFocusEvent & event);
   void OnKillFocus(wxFocusEvent & event);
   
   void OnContextMenu(wxContextMenuEvent & event);
   
   void HandleInterruptedDrag();
   void Uncapture( wxMouseState *pState = nullptr );
   bool HandleEscapeKey(bool down);
   void UpdateMouseState(const wxMouseState &state);
   void HandleModifierKey();
   
   void HandleClick( const TrackPanelMouseEvent &tpmEvent );
   void HandleWheelRotation( TrackPanelMouseEvent &tpmEvent );
   
   void HandleMotion( wxMouseState &state, bool doHit = true );
   void HandleMotion
   ( const TrackPanelMouseState &tpmState, bool doHit = true );
   
   
protected:
   ViewInfo *mViewInfo;
   
private:
   UIHandlePtr mUIHandle;
   
   std::weak_ptr<TrackPanelCell> mLastCell;
   std::vector<UIHandlePtr> mTargets;
   size_t mTarget {};
   unsigned mMouseOverUpdateFlags{};
   
protected:
   // To do: make a drawing method and make this private
   wxMouseState mLastMouseState;
   
private:
   int mMouseMostRecentX;
   int mMouseMostRecentY;
   
   std::weak_ptr<TrackPanelCell> mpClickedCell;
   
   bool mEnableTab{};
   
   DECLARE_EVENT_TABLE()
};

#endif
