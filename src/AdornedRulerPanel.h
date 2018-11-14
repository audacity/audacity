/**********************************************************************

  Audacity: A Digital Audio Editor

  AdornedRulerPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_ADORNED_RULER_PANEL__
#define __AUDACITY_ADORNED_RULER_PANEL__

#include "CellularPanel.h"
#include "widgets/Ruler.h"

#include "MemoryX.h"
#include <wx/bitmap.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>

class ViewInfo;
class AudacityProject;
class SnapManager;
class TrackList;

// This is an Audacity Specific ruler panel.
class AUDACITY_DLL_API AdornedRulerPanel final : public CellularPanel
{
public:
   AdornedRulerPanel(AudacityProject *project,
                     wxWindow* parent,
                     wxWindowID id,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     ViewInfo *viewinfo = NULL);

   ~AdornedRulerPanel();

   bool AcceptsFocus() const override { return s_AcceptsFocus; }
   bool AcceptsFocusFromKeyboard() const override { return true; }
   void SetFocusFromKbd() override;

public:
   int GetRulerHeight() { return GetRulerHeight(mShowScrubbing); }
   static int GetRulerHeight(bool showScrubBar);
   wxRect GetInnerRect() const { return mInner; }

   void SetLeftOffset(int offset);

   void DrawSelection();

   void SetPlayRegion(double playRegionStart, double playRegionEnd);
   void ClearPlayRegion();
   void GetPlayRegion(double* playRegionStart, double* playRegionEnd);

   void GetMaxSize(wxCoord *width, wxCoord *height);

   void InvalidateRuler();

   void UpdatePrefs();
   void ReCreateButtons();

   void RegenerateTooltips();

   void UpdateQuickPlayPos(wxCoord &mousePosX, bool shiftDown);

   bool ShowingScrubRuler() const { return mShowScrubbing; }
   void OnToggleScrubRuler(/*wxCommandEvent& */);
   void OnToggleScrubRulerFromMenu(wxCommandEvent& );
   void SetPanelSize();
   
   void DrawBothOverlays();


private:
   void OnRecordStartStop(wxCommandEvent & evt);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void UpdateRects();
   void HandleQPClick(wxMouseEvent &event, wxCoord mousePosX);
   void HandleQPDrag(wxMouseEvent &event, wxCoord mousePosX);
   void HandleQPRelease(wxMouseEvent &event);
   void StartQPPlay(bool looped, bool cutPreview);

   void DoDrawBackground(wxDC * dc);
   void DoDrawEdge(wxDC *dc);
   void DoDrawMarks(wxDC * dc, bool /*text */ );
   void DoDrawSelection(wxDC * dc);

public:
   void DoDrawIndicator(wxDC * dc, wxCoord xx, bool playing, int width, bool scrub, bool seek);
   void UpdateButtonStates();

private:
   static bool s_AcceptsFocus;
   struct Resetter { void operator () (bool *p) const { if(p) *p = false; } };
   using TempAllowFocus = std::unique_ptr<bool, Resetter>;

public:
   static TempAllowFocus TemporarilyAllowFocus();

private:
   void DoDrawPlayRegion(wxDC * dc);

   enum class MenuChoice { QuickPlay, Scrub };
   void ShowContextMenu( MenuChoice choice, const wxPoint *pPosition);

   double Pos2Time(int p, bool ignoreFisheye = false);
   int Time2Pos(double t, bool ignoreFisheye = false);

   bool IsWithinMarker(int mousePosX, double markerTime);

private:

   Ruler mRuler;
   AudacityProject *const mProject;
   TrackList *mTracks;

   wxRect mOuter;
   wxRect mScrubZone;
   wxRect mInner;

   int mLeftOffset;  // Number of pixels before we hit the 'zero position'.


   double mIndTime;
   double mQuickPlayPosUnsnapped;
   double mQuickPlayPos;

   bool mIsSnapped;

   bool   mPlayRegionLock;
   double mPlayRegionStart;
   double mPlayRegionEnd;
   double mOldPlayRegionStart;
   double mOldPlayRegionEnd;

   bool mIsRecording;

   //
   // Pop-up menu
   //
   void ShowMenu(const wxPoint & pos);
   void ShowScrubMenu(const wxPoint & pos);
   void DragSelection();
   void HandleSnapping();
   void OnToggleQuickPlay(wxCommandEvent &evt);
   void OnSyncSelToQuickPlay(wxCommandEvent &evt);
   void OnTimelineToolTips(wxCommandEvent &evt);
   void OnAutoScroll(wxCommandEvent &evt);
   void OnLockPlayRegion(wxCommandEvent &evt);

   void OnTogglePinnedState(wxCommandEvent & event);

   bool mPlayRegionDragsSelection;
   bool mTimelineToolTip;
   bool mQuickPlayEnabled;

   enum MouseEventState {
      mesNone,
      mesDraggingPlayRegionStart,
      mesDraggingPlayRegionEnd,
      mesSelectingPlayRegionClick,
      mesSelectingPlayRegionRange
   };

   MouseEventState mMouseEventState;
   double mLeftDownClickUnsnapped;  // click position in seconds, before snap
   double mLeftDownClick;  // click position in seconds
   bool mIsDragging;

   bool mShowScrubbing { false };

   DECLARE_EVENT_TABLE()

   wxWindow *mButtons[3];
   bool mNeedButtonUpdate { true };

   //
   // CellularPanel implementation
   //

   // Get the root object defining a recursive subdivision of the panel's
   // area into cells
   std::shared_ptr<TrackPanelNode> Root() override;
public:
   AudacityProject * GetProject() const override;
private:
   TrackPanelCell *GetFocusedCell() override;
   void SetFocusedCell() override;
   void ProcessUIHandleResult
      (TrackPanelCell *pClickedTrack, TrackPanelCell *pLatestCell,
       unsigned refreshResult) override;

   void UpdateStatusMessage( const wxString & ) override;

   void CreateOverlays();

   // Cooperating objects
   class QuickPlayIndicatorOverlay;
   std::shared_ptr<QuickPlayIndicatorOverlay> mOverlay;

   class QuickPlayRulerOverlay;
   
private:
   class CommonRulerHandle;
   class QPHandle;
   class ScrubbingHandle;

   class CommonCell;

   class QPCell;
   std::shared_ptr<QPCell> mQPCell;
   
   class ScrubbingCell;
   std::shared_ptr<ScrubbingCell> mScrubbingCell;

   // classes implementing subdivision for CellularPanel
   struct Subgroup;
   struct MainGroup;
};

#endif //define __AUDACITY_ADORNED_RULER_PANEL__
