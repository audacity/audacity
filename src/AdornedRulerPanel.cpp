/**********************************************************************

  Audacity: A Digital Audio Editor

  AdornedRulerPanel.cpp

  Dominic Mazzoni

*******************************************************************//**

\class AdornedRulerPanel
\brief This is an Audacity Specific ruler panel which additionally
  has border, selection markers, play marker.
  
  Once TrackPanel uses wxSizers, we will derive it from some
  wxWindow and the GetSize and SetSize functions
  will then be wxWidgets functions instead.

*//******************************************************************/


#include "AdornedRulerPanel.h"

#include <wx/app.h>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/tooltip.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "AudioIO.h"
#include "BasicMenu.h"
#include "CellularPanel.h"
#include "../images/Cursors.h"
#include "HitTestResult.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectAudioManager.h"
#include "ProjectWindows.h"
#include "ProjectStatus.h"
#include "ProjectWindow.h"
#include "RefreshCode.h"
#include "SelectUtilities.h"
#include "Snap.h"
#include "Track.h"
#include "TrackPanelMouseEvent.h"
#include "UIHandle.h"
#include "ViewInfo.h"
#include "prefs/TracksPrefs.h"
#include "prefs/ThemePrefs.h"
#include "toolbars/ToolBar.h"
#include "toolbars/ToolManager.h"
#include "tracks/ui/Scrubbing.h"
#include "tracks/ui/TrackView.h"
#include "widgets/AButton.h"
#include "AudacityMessageBox.h"
#include "widgets/Grabber.h"
#include "widgets/LinearUpdater.h"
#include "widgets/TimeFormat.h"
#include "wxWidgetsWindowPlacement.h"

#include <wx/dcclient.h>
#include <wx/menu.h>

using std::min;
using std::max;

//#define SCRUB_ABOVE

#define SELECT_TOLERANCE_PIXEL 4

#define PLAY_REGION_TRIANGLE_SIZE 6
#define PLAY_REGION_RECT_WIDTH 1
#define PLAY_REGION_RECT_HEIGHT 3
#define PLAY_REGION_GLOBAL_OFFSET_Y 7

enum : int {
   IndicatorSmallWidth = 9,
   IndicatorMediumWidth = 13,
   IndicatorOffset = 1,

   TopMargin = 1,
   BottomMargin = 2, // for bottom bevel and bottom line
   LeftMargin = 1, 

   RightMargin = 1,
};

enum {
   ScrubHeight = 14,
   ProperRulerHeight = 29
};

inline int IndicatorHeightForWidth(int width)
{
   return ((width / 2) * 3) / 2;
}

inline int IndicatorWidthForHeight(int height)
{
   // Not an exact inverse of the above, with rounding, but good enough
   return std::max(static_cast<int>(IndicatorSmallWidth),
                   (((height) * 2) / 3) * 2
                   );
}

inline int IndicatorBigHeight()
{
   return std::max((int)(ScrubHeight - TopMargin),
                   (int)(IndicatorMediumWidth));
}

inline int IndicatorBigWidth()
{
   return IndicatorWidthForHeight(IndicatorBigHeight());
}

class AdornedRulerPanel::CommonRulerHandle : public UIHandle
{
public:
   CommonRulerHandle(
      AdornedRulerPanel *pParent, wxCoord xx, MenuChoice menuChoice )
      : mParent(pParent)
      , mX( xx )
      , mClickedX( xx )
      , mChoice( menuChoice )
   {}

   bool Clicked() const { return mClicked != Button::None; }

   static UIHandle::Result NeedChangeHighlight(
      const CommonRulerHandle &oldState, const CommonRulerHandle &newState)
   {
      if (oldState.mX != newState.mX)
         return RefreshCode::DrawOverlays;
      return 0;
   }

protected:
   bool HandlesRightClick() override { return true; }

   Result Click(
      const TrackPanelMouseEvent &event, AudacityProject *) override
   {
      mClicked = event.event.LeftIsDown() ? Button::Left : Button::Right;
      mClickedX = event.event.GetX();
      return RefreshCode::DrawOverlays;
   }

   Result Drag(
      const TrackPanelMouseEvent &, AudacityProject *) override
   {
      return RefreshCode::DrawOverlays;
   }

   Result Release(
      const TrackPanelMouseEvent &event, AudacityProject *,
      wxWindow *) override
   {
      if ( mParent && mClicked == Button::Right ) {
         const auto pos = event.event.GetPosition();
         mParent->ShowContextMenu( mChoice, &pos );
      }
      return RefreshCode::DrawOverlays;
   }

   double Time(AudacityProject &project) const
   {
      auto &viewInfo = ViewInfo::Get(project);
      return viewInfo.PositionToTime(mX, viewInfo.GetLeftOffset());
   }

   // Danger!  `this` may be deleted!
   void StartPlay(AudacityProject &project, const wxMouseEvent &event)
   {
      auto &ruler = AdornedRulerPanel::Get(project);
   
      // Keep a shared pointer to self.  Otherwise *this might get deleted
      // in HandleQPRelease on Windows!  Because there is an event-loop yield
      // stopping playback, which caused OnCaptureLost to be called, which caused
      // clearing of CellularPanel targets!
      auto saveMe = ruler.Target();

      const auto startTime = Time(project);
      ruler.StartQPPlay(!event.ShiftDown(), false, &startTime);
   }

   Result Cancel(AudacityProject *) override
   {
      return RefreshCode::DrawOverlays;
   }
   
   void Enter(bool, AudacityProject *) override
   {
      mChangeHighlight = RefreshCode::DrawOverlays;
   }

   wxWeakRef<AdornedRulerPanel> mParent;

   wxCoord mX;
   wxCoord mClickedX;
   
   MenuChoice mChoice;

   enum class Button { None, Left, Right };
   Button mClicked{ Button::None };
};

class AdornedRulerPanel::PlayRegionAdjustingHandle : public CommonRulerHandle {
public:
   PlayRegionAdjustingHandle(
      AdornedRulerPanel *pParent, wxCoord xx, MenuChoice menuChoice,
      wxCursor cursor,
      size_t numGuides = 1)
   : CommonRulerHandle{ pParent, xx, menuChoice }
   , mCursor{ cursor }
   , mNumGuides{ numGuides }
   {}

   HitTestPreview Preview(
      const TrackPanelMouseState &state, AudacityProject *pProject)
   override
   {
      mParent->SetNumGuides(mNumGuides);
      const auto message = XO("Click and drag to define a looping region.");
      return {
         message,
         &mCursor,
         message
      };
   }

   Result Drag(
      const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {
      using namespace RefreshCode;
      auto result = CommonRulerHandle::Drag(event, pProject);
      if (0 != (result & Cancelled) || mClicked != Button::Left)
         return result;

      auto &ruler = AdornedRulerPanel::Get(*pProject);
      mX = event.event.m_x;
      ruler.UpdateQuickPlayPos(event.event.m_x);
   
      if (!mDragged) {
         if (fabs(mX - mClickedX) < SELECT_TOLERANCE_PIXEL)
            // Don't start a drag yet for a small mouse movement
            return RefreshNone;
         SavePlayRegion(*pProject);
         const auto time = SnappedTime(*pProject, 0);
         DoStartAdjust(*pProject, time);
         mDragged = true;
      }
      else
         DoAdjust(*pProject);

      if (AdornedRulerPanel::Get( *pProject ).mPlayRegionDragsSelection)
         DragSelection(*pProject);

      return RefreshAll;
   }
  
   Result Release(
      const TrackPanelMouseEvent &event,
      AudacityProject *pProject, wxWindow *pParent)
   override
   {
      using namespace RefreshCode;
      auto result = CommonRulerHandle::Release(event, pProject, pParent);

      if (mClicked == Button::Left && !mDragged)
         StartPlay(*pProject, event.event);

      if (!mDragged || 0 != (result & Cancelled))
         return result;

      // Set the play region endpoints correctly even if not strictly needed
      auto &viewInfo = ViewInfo::Get( *pProject );
      auto &playRegion = viewInfo.playRegion;
      playRegion.Order();

      return result;
   }

   Result Cancel(AudacityProject *pProject) override
   {
      using namespace RefreshCode;
      auto result = CommonRulerHandle::Cancel(pProject);
      if (!mSaved)
         return result;

      //! Restore state as before SavePlayRegion()
      auto &viewInfo = ViewInfo::Get(*pProject);
      viewInfo.selectedRegion = mOldSelectedRegion;
      auto &playRegion = viewInfo.playRegion;
      playRegion.SetTimes(mOldStart, mOldEnd);
      if (!mWasActive)
         playRegion.SetActive(false);
   
      return RefreshAll;
   }

   // Compare with class SelectHandle.  Perhaps a common base class for that
   // class too should be defined.
   bool HasEscape(AudacityProject *pProject) const override
   {
      auto &ruler = AdornedRulerPanel::Get(*pProject);
      auto values = ruler.mIsSnapped;
      auto identity = [](auto x){ return x; }; // in the C++20 standard...
      return std::any_of( values, values + ruler.mNumGuides, identity );
   }

   bool Escape(AudacityProject *pProject) override
   {
      if (HasEscape(pProject)) {
         Unsnap(false, pProject);
         return true;
      }
      return false;
   }

protected:

   double SnappedTime( AudacityProject &project, size_t ii )
   {
      const auto &ruler = AdornedRulerPanel::Get(project);
      bool isSnapped = ruler.mIsSnapped[ii];
      return isSnapped
         ? ruler.mQuickPlayPos[ii]
         : ruler.mQuickPlayPosUnsnapped[ii];
   }

   std::pair<double, double> SnappedTimes( AudacityProject &project )
   {
      const auto &ruler = AdornedRulerPanel::Get(project);
      for (size_t ii = 0; ii < ruler.mNumGuides; ++ii)
         if (ruler.mIsSnapped[ii]) {
            double time0 = ruler.mQuickPlayPos[ii];
            double delta = time0 - ruler.mQuickPlayPosUnsnapped[ii];
            double time1 = ruler.mQuickPlayPosUnsnapped[1 - ii] + delta;
            if (ii == 1)
               return { time1, time0 };
            else
               return { time0, time1 };
         }
      // No snap
      return { ruler.mQuickPlayPos[0], ruler.mQuickPlayPos[1] };
   }

   void Unsnap(bool use, AudacityProject *pProject)
   {
      auto &ruler = AdornedRulerPanel::Get(*pProject);
      std::fill( ruler.mIsSnapped, ruler.mIsSnapped + ruler.mNumGuides, false );
      // Repaint to turn the snap lines on or off
      mChangeHighlight = RefreshCode::RefreshAll;
      if (Clicked())
         DoAdjust(*pProject);
   }

   virtual void DoStartAdjust(AudacityProject &, double) = 0;
   virtual void DoAdjust(AudacityProject &) = 0;

   void SavePlayRegion(AudacityProject &project)
   {
      auto &viewInfo = ViewInfo::Get(project);
      mOldSelectedRegion = viewInfo.selectedRegion;
      auto &playRegion = viewInfo.playRegion;
      mWasActive = playRegion.Active();
      mOldStart = playRegion.GetLastActiveStart();
      mOldEnd = playRegion.GetLastActiveEnd();
      if (!mWasActive)
         playRegion.SetActive(true);
      mSaved = true;
   }

private:
   // wxCursor is a cheaply copied reference-counting handle to a resource
   wxCursor mCursor;
   size_t mNumGuides;

   SelectedRegion mOldSelectedRegion;
   double mOldStart = 0.0, mOldEnd = 0.0;
   bool mWasActive = false;
   bool mSaved = false;
   bool mDragged = false;
};

/**********************************************************************

ScrubbingRulerOverlay.
Graphical helper for AdornedRulerPanel.

**********************************************************************/

class TrackPanelGuidelineOverlay;

// This is an overlay drawn on the ruler.
class AdornedRulerPanel::ScrubbingRulerOverlay final : public Overlay
{
public:
   ScrubbingRulerOverlay(TrackPanelGuidelineOverlay &partner);

   int mNewQPIndicatorPos { -1 };
   int mNewIndicatorSnapped { -1 };

   bool mNewScrub {};
   bool mNewSeek {};

   void Update();

private:
   AdornedRulerPanel *GetRuler() const;

   unsigned SequenceNumber() const override;

   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

   TrackPanelGuidelineOverlay &mPartner;

   // Used by this only
   int mOldQPIndicatorPos { -1 };
   bool mOldScrub {};
   bool mOldSeek {};
};

/**********************************************************************

 TrackPanelGuidelineOverlay.
 Updated for mouse events in AdornedRulerPanel, but draws on the TrackPanel.

 **********************************************************************/

// This is an overlay drawn on a different window, the track panel.
// It draws the pale guide line that follows mouse movement.
class AdornedRulerPanel::TrackPanelGuidelineOverlay final : public Overlay
{
   friend ScrubbingRulerOverlay;
   friend AdornedRulerPanel;

public:
   TrackPanelGuidelineOverlay(AudacityProject *project);

private:
   void Update();

   unsigned SequenceNumber() const override;
   std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
   void Draw(OverlayPanel &panel, wxDC &dc) override;

   AudacityProject *mProject;

   std::shared_ptr<ScrubbingRulerOverlay> mPartner
      { std::make_shared<ScrubbingRulerOverlay>(*this) };

   bool mNewPreviewingScrub {};

   int mOldQPIndicatorPos { -1 };
   int mOldIndicatorSnapped { -1 };
   bool mOldPreviewingScrub {};
};

/**********************************************************************

 Implementation of ScrubbingRulerOverlay.

 **********************************************************************/

AdornedRulerPanel::ScrubbingRulerOverlay::ScrubbingRulerOverlay(
   TrackPanelGuidelineOverlay &partner)
: mPartner(partner)
{
}

AdornedRulerPanel *AdornedRulerPanel::ScrubbingRulerOverlay::GetRuler() const
{
   return &Get( *mPartner.mProject );
}

void AdornedRulerPanel::ScrubbingRulerOverlay::Update()
{
   const auto project = mPartner.mProject;
   auto &scrubber = Scrubber::Get( *project );
   auto ruler = GetRuler();

   bool scrubbing = (scrubber.IsScrubbing()
      && !scrubber.IsSpeedPlaying()
      && !scrubber.IsKeyboardScrubbing());

   // Hide during transport, or if mouse is not in the ruler, unless scrubbing
   if ((!ruler->LastCell() || ProjectAudioIO::Get( *project ).IsAudioActive())
       && !scrubbing)
      mNewQPIndicatorPos = -1;
   else {
      const auto &selectedRegion = ViewInfo::Get( *project ).selectedRegion;
      double latestEnd =
         std::max(ruler->mTracks->GetEndTime(), selectedRegion.t1());
      // This will determine the x coordinate of the line and of the
      // ruler indicator

      // Test all snap points
      mNewIndicatorSnapped = -1;
      for (size_t ii = 0;
           mNewIndicatorSnapped == -1 && ii < ruler->mNumGuides; ++ii) {
         if (ruler->mIsSnapped[ii]) {
            mNewIndicatorSnapped = ii;
         }
      }
      mNewQPIndicatorPos = ruler->Time2Pos(
         ruler->mQuickPlayPos[std::max(0, mNewIndicatorSnapped)]);

      // These determine which shape is drawn on the ruler, and whether
      // in the scrub or the qp zone
      mNewScrub =
         !ruler->IsMouseCaptured() && // not doing some other drag in the ruler
         (ruler->LastCell() == ruler->mScrubbingCell ||
          (scrubber.HasMark()));
      mNewSeek = mNewScrub &&
         (scrubber.Seeks() || scrubber.TemporarilySeeks());
   }
}

unsigned
AdornedRulerPanel::ScrubbingRulerOverlay::SequenceNumber() const
{
   return 30;
}

std::pair<wxRect, bool>
AdornedRulerPanel::ScrubbingRulerOverlay::DoGetRectangle(wxSize /*size*/)
{
   Update();

   const auto x = mOldQPIndicatorPos;
   if (x >= 0) {
      // These dimensions are always sufficient, even if a little
      // excessive for the small triangle:
      const int width = IndicatorBigWidth() * 3 / 2;
      //const auto height = IndicatorHeightForWidth(width);

      const int indsize = width / 2;

      auto xx = x - indsize;
      auto yy = 0;
      return {
         { xx, yy,
            indsize * 2 + 1,
            GetRuler()->GetSize().GetHeight() },
         (x != mNewQPIndicatorPos
          || (mOldScrub != mNewScrub)
          || (mOldSeek != mNewSeek) )
      };
   }
   else
      return { {}, mNewQPIndicatorPos >= 0 };
}

void AdornedRulerPanel::ScrubbingRulerOverlay::Draw(
   OverlayPanel & /*panel*/, wxDC &dc)
{
   mOldQPIndicatorPos = mNewQPIndicatorPos;
   mOldScrub = mNewScrub;
   mOldSeek = mNewSeek;
   if (mOldQPIndicatorPos >= 0) {
      auto ruler = GetRuler();
      auto width = mOldScrub ? IndicatorBigWidth() : IndicatorSmallWidth;
      ruler->DoDrawScrubIndicator(
         &dc, mOldQPIndicatorPos, width, mOldScrub, mOldSeek);
   }
}

/**********************************************************************

 Implementation of TrackPanelGuidelineOverlay.

 **********************************************************************/

AdornedRulerPanel::TrackPanelGuidelineOverlay::TrackPanelGuidelineOverlay(
   AudacityProject *project)
   : mProject(project)
{
}

unsigned
AdornedRulerPanel::TrackPanelGuidelineOverlay::SequenceNumber() const
{
   return 30;
}

void AdornedRulerPanel::TrackPanelGuidelineOverlay::Update()
{
   const auto project = mProject;
   auto &scrubber = Scrubber::Get( *project );
   const auto ruler = &Get( *project );

   // Determine the color of the line stroked over
   // the track panel, green for scrub or yellow for snapped or white
   mNewPreviewingScrub =
      ruler->LastCell() == ruler->mScrubbingCell &&
      !scrubber.IsScrubbing();
}

std::pair<wxRect, bool>
AdornedRulerPanel::TrackPanelGuidelineOverlay::DoGetRectangle(wxSize size)
{
   Update();

   wxRect rect(mOldQPIndicatorPos, 0, 1, size.GetHeight());
   return std::make_pair(
      rect,
      (mOldQPIndicatorPos != mPartner->mNewQPIndicatorPos ||
       mOldIndicatorSnapped != mPartner->mNewIndicatorSnapped ||
       mOldPreviewingScrub != mNewPreviewingScrub)
   );
}

void AdornedRulerPanel::TrackPanelGuidelineOverlay::Draw(
   OverlayPanel &panel, wxDC &dc)
{
   mOldQPIndicatorPos = mPartner->mNewQPIndicatorPos;
   mOldIndicatorSnapped = mPartner->mNewIndicatorSnapped;
   mOldPreviewingScrub = mNewPreviewingScrub;

   if (mOldQPIndicatorPos >= 0) {
      if (!mOldPreviewingScrub && mOldIndicatorSnapped < 0) {
         auto &ruler = AdornedRulerPanel::Get(*mProject);
         if (auto pHandle =
             dynamic_cast<PlayRegionAdjustingHandle*>(ruler.Target().get());
             pHandle != nullptr && pHandle->Clicked())
            // Do not draw the quick-play guideline
            return;
      }
   
      mOldPreviewingScrub
         ? AColor::IndicatorColor(&dc, true) // Draw green line for preview.
         : (mOldIndicatorSnapped >= 0)
            ? AColor::SnapGuidePen(&dc) // Yellow snap guideline
            : AColor::Light(&dc, false);

      // Draw indicator in all visible tracks
      auto pCellularPanel = dynamic_cast<CellularPanel*>( &panel );
      if ( !pCellularPanel ) {
         wxASSERT( false );
         return;
      }
      pCellularPanel
         ->VisitCells( [&]( const wxRect &rect, TrackPanelCell &cell ) {
            const auto pTrackView = dynamic_cast<TrackView*>(&cell);
            if (!pTrackView)
               return;

            // Draw the NEW indicator in its NEW location
            AColor::Line(dc,
               mOldQPIndicatorPos,
               rect.GetTop(),
               mOldQPIndicatorPos,
               rect.GetBottom());
      } );
   }
}

/**********************************************************************

  Implementation of AdornedRulerPanel.
  Either we find a way to make this more generic, Or it will move
  out of the widgets subdirectory into its own source file.

**********************************************************************/

enum {
   OnSyncQuickPlaySelID = 7000,
   OnAutoScrollID,
   OnTogglePlayRegionID,
   OnClearPlayRegionID,
   OnSetPlayRegionToSelectionID,
   OnTogglePinnedStateID,
};

BEGIN_EVENT_TABLE(AdornedRulerPanel, CellularPanel)
   EVT_IDLE( AdornedRulerPanel::OnIdle )
   EVT_PAINT(AdornedRulerPanel::OnPaint)
   EVT_SIZE(AdornedRulerPanel::OnSize)
   EVT_LEAVE_WINDOW(AdornedRulerPanel::OnLeave)

   // Context menu commands
   EVT_MENU(OnSyncQuickPlaySelID, AdornedRulerPanel::OnSyncSelToQuickPlay)
   EVT_MENU(OnAutoScrollID, AdornedRulerPanel::OnAutoScroll)
   EVT_MENU(OnTogglePlayRegionID, AdornedRulerPanel::OnTogglePlayRegion)
   EVT_MENU(OnClearPlayRegionID, AdornedRulerPanel::OnClearPlayRegion)
   EVT_MENU(OnSetPlayRegionToSelectionID,
      AdornedRulerPanel::OnSetPlayRegionToSelection)
   EVT_MENU( OnTogglePinnedStateID, AdornedRulerPanel::OnTogglePinnedState )

   EVT_COMMAND( OnTogglePinnedStateID,
               wxEVT_COMMAND_BUTTON_CLICKED,
               AdornedRulerPanel::OnPinnedButton )

END_EVENT_TABLE()

class AdornedRulerPanel::CommonCell : public TrackPanelCell
{
public:
   explicit
   CommonCell( AdornedRulerPanel *parent, MenuChoice menuChoice )
   : mParent{ parent }
   , mMenuChoice{ menuChoice }
   {}
   
   HitTestPreview DefaultPreview(
      const TrackPanelMouseState &, const AudacityProject *)
      override
   {
      // May come here when recording is in progress, so hit tests are turned
      // off.
      TranslatableString tooltip;
      if (mParent->mTimelineToolTip)
         tooltip = XO("Timeline actions disabled during recording");

      static wxCursor cursor{ wxCURSOR_DEFAULT };
      return {
         {},
         &cursor,
         tooltip,
      };
   }

   unsigned DoContextMenu(
      const wxRect &,
      wxWindow *, const wxPoint *pPosition, AudacityProject*) final
   {
      mParent->ShowContextMenu(mMenuChoice, pPosition);
      return 0;
   }

protected:
   AdornedRulerPanel *mParent;
   const MenuChoice mMenuChoice;
};

#undef QUICK_PLAY_HANDLE
#ifdef QUICK_PLAY_HANDLE
class AdornedRulerPanel::QPHandle final : public CommonRulerHandle
{
public:
   explicit
   QPHandle( AdornedRulerPanel *pParent, wxCoord xx )
   : CommonRulerHandle( pParent, xx, MenuChoice::QuickPlay )
   {
   }
   
private:
   Result Click(
      const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag(
      const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview(
      const TrackPanelMouseState &state, AudacityProject *pProject)
   override;

   Result Release(
      const TrackPanelMouseEvent &event, AudacityProject *pProject,
      wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   SelectedRegion mOldSelection;
};
#endif

static auto handOpenCursor =
    MakeCursor(wxCURSOR_HAND, RearrangeCursorXpm, 16, 16);

class AdornedRulerPanel::MovePlayRegionHandle final : public PlayRegionAdjustingHandle {
public:
   MovePlayRegionHandle( AdornedRulerPanel *pParent, wxCoord xx )
   : PlayRegionAdjustingHandle( pParent, xx, MenuChoice::QuickPlay, *handOpenCursor, 2 )
   {
   }

   ~MovePlayRegionHandle()
   {
      mParent->mQuickPlayOffset[0] = 0;
      mParent->mQuickPlayOffset[1] = 0;
   }
   
private:
   void DoStartAdjust(AudacityProject &project, double) override
   {
      // Ignore the snapping of the time at the mouse
      const auto time = Time(project);
      auto &playRegion = ViewInfo::Get(project).playRegion;
      mParent->mQuickPlayOffset[0] = playRegion.GetStart() - time;
      mParent->mQuickPlayOffset[1] = playRegion.GetEnd() - time;
   }

   void DoAdjust(AudacityProject &project) override
   {
      // Move the whole play region rigidly (usually)
      // though the length might change slightly if only one edge snaps
      const auto times = SnappedTimes(project);

      auto &playRegion = ViewInfo::Get(project).playRegion;
      playRegion.SetTimes(times.first, times.second);
   }
};

class AdornedRulerPanel::ResizePlayRegionHandle final : public PlayRegionAdjustingHandle {
public:
   ResizePlayRegionHandle(
      AdornedRulerPanel *pParent, wxCoord xx, bool hitLeft )
   : PlayRegionAdjustingHandle( pParent, xx, MenuChoice::QuickPlay, {wxCURSOR_SIZEWE} )
   , mHitLeft{ hitLeft }
   {
   }
   
private:
   void DoStartAdjust(AudacityProject &project, double time) override
   {
   }

   void DoAdjust(AudacityProject &project) override
   {
      const auto time = SnappedTime(project, 0);

      // Change the play region
      // The check whether this new time should be start or end isn't
      // important.  The accessors for PlayRegion use min and max of stored
      // values.
      auto &playRegion = ViewInfo::Get(project).playRegion;
      if (mHitLeft)
         playRegion.SetStart(time);
      else
         playRegion.SetEnd(time);
   }

   bool mHitLeft = false;
};

class AdornedRulerPanel::NewPlayRegionHandle final : public PlayRegionAdjustingHandle {
public:
   NewPlayRegionHandle( AdornedRulerPanel *pParent, wxCoord xx )
   : PlayRegionAdjustingHandle( pParent, xx, MenuChoice::QuickPlay, {wxCURSOR_DEFAULT} )
   {
   }
   
private:
   void DoStartAdjust(AudacityProject &project, double time) override
   {
      auto &playRegion = ViewInfo::Get(project).playRegion;
      playRegion.SetTimes(time, time);
   }

   void DoAdjust(AudacityProject &project) override
   {
      const auto time = SnappedTime(project, 0);

      // Change the play region
      // The check whether this new time should be start or end isn't
      // important.  The accessors for PlayRegion use min and max of stored
      // values.
      auto &playRegion = ViewInfo::Get(project).playRegion;
      playRegion.SetEnd(time);
   }
};

namespace
{

wxCoord GetPlayHeadX( const AudacityProject *pProject )
{
   const auto &viewInfo = ViewInfo::Get( *pProject );
   auto width = viewInfo.GetTracksUsableWidth();
   return viewInfo.GetLeftOffset()
      + width * TracksPrefs::GetPinnedHeadPositionPreference();
}

double GetPlayHeadFraction( const AudacityProject *pProject, wxCoord xx )
{
   const auto &viewInfo = ViewInfo::Get( *pProject );
   auto width = viewInfo.GetTracksUsableWidth();
   auto fraction = (xx - viewInfo.GetLeftOffset()) / double(width);
   return std::max(0.0, std::min(1.0, fraction));
}

// Handle for dragging the pinned play head, which so far does not need
// to be a friend of the AdornedRulerPanel class, so we don't make it nested.
class PlayheadHandle : public UIHandle
{
public:
   PlayheadHandle( AdornedRulerPanel &parent, wxCoord xx )
      : mpParent{ &parent }
      , mX( xx )
   {}

   static UIHandle::Result NeedChangeHighlight(
      const PlayheadHandle &oldState, const PlayheadHandle &newState)
   {
      if (oldState.mX != newState.mX)
         return RefreshCode::DrawOverlays;
      return 0;
   }
   
   static std::shared_ptr<PlayheadHandle>
   HitTest(
      const AudacityProject *pProject, AdornedRulerPanel &parent, wxCoord xx )
   {
      if( Scrubber::Get( *pProject )
         .IsTransportingPinned() &&
          ProjectAudioIO::Get( *pProject ).IsAudioActive() )
      {
         const auto targetX = GetPlayHeadX( pProject );
         if ( abs( xx - targetX ) <= SELECT_TOLERANCE_PIXEL )
            return std::make_shared<PlayheadHandle>( parent, xx );
      }
      return {};
   }
   
protected:
   Result Click(
      const TrackPanelMouseEvent &event, AudacityProject *) override
   {
      if (event.event.LeftDClick()) {
         // Restore default position on double click
         TracksPrefs::SetPinnedHeadPositionPreference( 0.5, true );
      
         return RefreshCode::DrawOverlays |
            // Do not start a drag
            RefreshCode::Cancelled;
      }
      // Fix for Bug 2357
      if (!event.event.LeftIsDown())
         return RefreshCode::Cancelled;

      mOrigPreference = TracksPrefs::GetPinnedHeadPositionPreference();
      return 0;
   }

   Result Drag(
      const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {

      auto value = GetPlayHeadFraction(pProject, event.event.m_x );
      TracksPrefs::SetPinnedHeadPositionPreference( value );
      return RefreshCode::DrawOverlays;
   }

   HitTestPreview Preview(
      const TrackPanelMouseState &, AudacityProject *)
      override
   {
      mpParent->SetNumGuides(1);
      static wxCursor cursor{ wxCURSOR_SIZEWE };
      return {
         XO( "Click and drag to adjust, double-click to reset" ),
         &cursor,
         /* i18n-hint: This text is a tooltip on the icon (of a pin) representing 
         the temporal position in the audio.  */
         XO( "Record/Play head" )
      };
   }

   Result Release(
      const TrackPanelMouseEvent &event, AudacityProject *pProject,
      wxWindow *) override
   {
      auto value = GetPlayHeadFraction(pProject, event.event.m_x );
      TracksPrefs::SetPinnedHeadPositionPreference( value, true );
      return RefreshCode::DrawOverlays;
   }

   Result Cancel(AudacityProject *) override
   {
      TracksPrefs::SetPinnedHeadPositionPreference( mOrigPreference );
      return RefreshCode::DrawOverlays;
   }
   
   void Enter(bool, AudacityProject *) override
   {
      mChangeHighlight = RefreshCode::DrawOverlays;
   }

   AdornedRulerPanel *mpParent;
   wxCoord mX;
   double mOrigPreference {};
};

}

class AdornedRulerPanel::QPCell final : public CommonCell
{
public:
   explicit
   QPCell( AdornedRulerPanel *parent )
   : AdornedRulerPanel::CommonCell{ parent, MenuChoice::QuickPlay }
   {}
   
   std::vector<UIHandlePtr> HitTest(
      const TrackPanelMouseState &state,
      const AudacityProject *pProject) override;
   
   // Return shared_ptr to self, stored in parent
   std::shared_ptr<TrackPanelCell> ContextMenuDelegate() override
      { return mParent->mQPCell; }

   bool Clicked() const {
#ifdef QUICK_PLAY_HANDLE
      if (auto ptr = mHolder.lock())
         return ptr->Clicked();
#endif
      return false;
   }
   
#ifdef QUICK_PLAY_HANDLE
   std::weak_ptr<QPHandle> mHolder;
#endif

   std::weak_ptr<ResizePlayRegionHandle> mResizePlayRegionHolder;
   std::weak_ptr<MovePlayRegionHandle> mMovePlayRegionHolder;
   std::weak_ptr<NewPlayRegionHandle> mNewPlayRegionHolder;
   std::weak_ptr<PlayheadHandle> mPlayheadHolder;
};

std::vector<UIHandlePtr> AdornedRulerPanel::QPCell::HitTest(
   const TrackPanelMouseState &state,
   const AudacityProject *pProject)
{
   // Creation of overlays on demand here -- constructor of AdornedRulerPanel
   // is too early to do it
   mParent->CreateOverlays();
   
   std::vector<UIHandlePtr> results;
   auto xx = state.state.m_x;

#ifdef EXPERIMENTAL_DRAGGABLE_PLAY_HEAD
   {
      // Allow click and drag on the play head even while recording
      // Make this handle more prominent then the quick play handle
      auto result = PlayheadHandle::HitTest( pProject, *mParent, xx );
      if (result) {
         result = AssignUIHandlePtr( mPlayheadHolder, result );
         results.push_back( result );
      }
   }
#endif
   
   // Disable mouse actions on Timeline while recording.
   if (!mParent->mIsRecording) {
      mParent->UpdateQuickPlayPos( xx );

      #if 0
      auto result = std::make_shared<QPHandle>( mParent, xx );
      result = AssignUIHandlePtr( mHolder, result );
      results.push_back( result );
      #endif
   }

   // High priority hit is a handle to change the existing play region
   bool hitLeft = false;
   const auto &playRegion = ViewInfo::Get(*pProject).playRegion;
   if ((hitLeft =
        mParent->IsWithinMarker(xx, playRegion.GetLastActiveStart())) ||
       mParent->IsWithinMarker(xx, playRegion.GetLastActiveEnd()))
   {
      auto result =
         std::make_shared<ResizePlayRegionHandle>( mParent, xx, hitLeft );
      result = AssignUIHandlePtr( mResizePlayRegionHolder, result );
      results.push_back(result);
   }

   // Middle priority hit is a handle to change the existing play region at
   // both ends, but only when the play region is active
   if (auto time = mParent->Pos2Time(xx);
       playRegion.Active() &&
       time >= playRegion.GetStart() &&
       time <= playRegion.GetEnd())
   {
      auto result =
         std::make_shared<MovePlayRegionHandle>( mParent, xx );
      result = AssignUIHandlePtr( mMovePlayRegionHolder, result );
      results.push_back(result);
   }

   // Lowest priority hit is a handle to drag a completely new play region
   {
      auto result = std::make_shared<NewPlayRegionHandle>( mParent, xx );
      result = AssignUIHandlePtr( mNewPlayRegionHolder, result );
      results.push_back(result);
   }

   return results;
}

class AdornedRulerPanel::ScrubbingHandle final : public CommonRulerHandle
{
public:
   explicit
   ScrubbingHandle( AdornedRulerPanel *pParent, wxCoord xx )
   : CommonRulerHandle( pParent, xx, MenuChoice::Scrub )
   {
   }

private:
   Result Click(
      const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {
      auto result = CommonRulerHandle::Click(event, pProject);
      if (!( result & RefreshCode::Cancelled )) {
         if (mClicked == Button::Left) {
            auto &scrubber = Scrubber::Get( *pProject );
            // only if scrubbing is allowed now
            bool canScrub =
               scrubber.CanScrub() &&
               mParent &&
               mParent->ShowingScrubRuler();

            if (!canScrub)
               return RefreshCode::Cancelled;
            if (!scrubber.HasMark()) {
               // Asynchronous scrub poller gets activated here
               scrubber.MarkScrubStart(
                  event.event.m_x, Scrubber::ShouldScrubPinned(), false);
            }
         }
      }
      return result;
   }

   Result Drag(
      const TrackPanelMouseEvent &event, AudacityProject *pProject) override
   {
      auto result = CommonRulerHandle::Drag(event, pProject);
      if (!( result & RefreshCode::Cancelled )) {
         // Nothing needed here.  The scrubber works by polling mouse state
         // after the start has been marked.
      }
      return result;
   }

   HitTestPreview Preview(
      const TrackPanelMouseState &state, AudacityProject *pProject)
   override;

   Result Release(
      const TrackPanelMouseEvent &event, AudacityProject *pProject,
      wxWindow *pParent) override {
      auto result = CommonRulerHandle::Release(event, pProject, pParent);
      if (!( result & RefreshCode::Cancelled )) {
         // Nothing needed here either.  The scrub poller may have decided to
         // seek because a drag happened before button up, or it may decide
         // to start a scrub, as it watches mouse movement after the button up.
      }
      return result;
   }

   Result Cancel(AudacityProject *pProject) override
   {
      auto result = CommonRulerHandle::Cancel(pProject);

      if (mClicked == Button::Left) {
         auto &scrubber = Scrubber::Get( *pProject );
         scrubber.Cancel();
         
         ProjectAudioManager::Get( *pProject ).Stop();
      }

      return result;
   }
};

class AdornedRulerPanel::ScrubbingCell final : public CommonCell
{
public:
   explicit
   ScrubbingCell( AdornedRulerPanel *parent )
   : AdornedRulerPanel::CommonCell{ parent, MenuChoice::Scrub }
   {}
   
   std::vector<UIHandlePtr> HitTest(
      const TrackPanelMouseState &state,
      const AudacityProject *pProject) override;
   
   // Return shared_ptr to self, stored in parent
   std::shared_ptr<TrackPanelCell> ContextMenuDelegate() override
      { return mParent->mScrubbingCell; }
   
   bool Hit() const { return !mHolder.expired(); }
   bool Clicked() const {
      if (auto ptr = mHolder.lock())
         return ptr->Clicked();
      return false;
   }
   
private:
   std::weak_ptr<ScrubbingHandle> mHolder;
};

std::vector<UIHandlePtr> AdornedRulerPanel::ScrubbingCell::HitTest(
   const TrackPanelMouseState &state, const AudacityProject *)
{
   // Creation of overlays on demand here -- constructor of AdornedRulerPanel
   // is too early to do it
   mParent->CreateOverlays();
   
   std::vector<UIHandlePtr> results;
   
   // Disable mouse actions on Timeline while recording.
   if (!mParent->mIsRecording) {
      auto xx = state.state.m_x;
      mParent->UpdateQuickPlayPos( xx );
      auto result = std::make_shared<ScrubbingHandle>( mParent, xx );
      result = AssignUIHandlePtr( mHolder, result );
      results.push_back( result );
   }
   
   return results;
}

namespace{
AttachedWindows::RegisteredFactory sKey{
[]( AudacityProject &project ) -> wxWeakRef< wxWindow > {
   auto &viewInfo = ViewInfo::Get( project );
   auto &window = ProjectWindow::Get( project );

   return safenew AdornedRulerPanel( &project, window.GetTrackListWindow(),
      wxID_ANY,
      wxDefaultPosition,
      wxSize( -1, AdornedRulerPanel::GetRulerHeight(false) ),
      &viewInfo );
}
};
}

AdornedRulerPanel &AdornedRulerPanel::Get( AudacityProject &project )
{
   return GetAttachedWindows(project).Get< AdornedRulerPanel >( sKey );
}

const AdornedRulerPanel &AdornedRulerPanel::Get(
   const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

void AdornedRulerPanel::Destroy( AudacityProject &project )
{
   auto *pPanel = GetAttachedWindows(project).Find( sKey );
   if (pPanel) {
      pPanel->wxWindow::Destroy();
      GetAttachedWindows(project).Assign( sKey, nullptr );
   }
}

AdornedRulerPanel::AdornedRulerPanel(AudacityProject* project,
   wxWindow *parent,
   wxWindowID id,
   const wxPoint& pos,
   const wxSize& size,
   ViewInfo *viewinfo
)  : CellularPanel(parent, id, pos, size, viewinfo)
   , mProject(project)
{
   SetLayoutDirection(wxLayout_LeftToRight);

   mQPCell = std::make_shared<QPCell>( this );
   mScrubbingCell = std::make_shared<ScrubbingCell>( this );
   
   for (auto &button : mButtons)
      button = nullptr;

   SetLabel( XO("Timeline") );
   SetName();
   SetBackgroundStyle(wxBG_STYLE_PAINT);

   mLeftOffset = 0;
   mIndTime = -1;

   mLeftDownClick = -1;
   mMouseEventState = mesNone;
   mIsDragging = false;

   mOuter = GetClientRect();

   mUpdater.SetData(mViewInfo, mLeftOffset);
   mRuler.SetLabelEdges( false );

   mTracks = &TrackList::Get( *project );

   mIsRecording = false;

   mTimelineToolTip = !!gPrefs->Read(wxT("/QuickPlay/ToolTips"), 1L);
   mPlayRegionDragsSelection = (gPrefs->Read(wxT("/QuickPlay/DragSelection"), 0L) == 1)? true : false; 

#if wxUSE_TOOLTIPS
   wxToolTip::Enable(true);
#endif

   mAudioIOSubscription = AudioIO::Get()
      ->Subscribe(*this, &AdornedRulerPanel::OnAudioStartStop);

   // Delay until after CommandManager has been populated:
   this->CallAfter( &AdornedRulerPanel::UpdatePrefs );

   mThemeChangeSubscription =
      theTheme.Subscribe(*this, &AdornedRulerPanel::OnThemeChange);

   // Bind event that updates the play region
   mPlayRegionSubscription = mViewInfo->selectedRegion.Subscribe(
      *this, &AdornedRulerPanel::OnSelectionChange);

   // And call it once to initialize it
   DoSelectionChange( mViewInfo->selectedRegion );
}

AdornedRulerPanel::~AdornedRulerPanel()
{
}

void AdornedRulerPanel::Refresh( bool eraseBackground, const wxRect *rect )
{
   CellularPanel::Refresh( eraseBackground, rect );
   CallAfter([this]{ CellularPanel::HandleCursorForPresentMouseState(); } );
}

void AdornedRulerPanel::UpdatePrefs()
{
   if (mNeedButtonUpdate) {
      // Visit this block once only in the lifetime of this panel
      mNeedButtonUpdate = false;
      // Do this first time setting of button status texts
      // when we are sure the CommandManager is initialized.
      ReCreateButtons();
   }

   // Update button texts for language change
   UpdateButtonStates();

   mTimelineToolTip = !!gPrefs->Read(wxT("/QuickPlay/ToolTips"), 1L);

#ifdef EXPERIMENTAL_SCROLLING_LIMITS
#ifdef EXPERIMENTAL_TWO_TONE_TIME_RULER
   {
      auto scrollBeyondZero = ScrollingPreference.Read();
      mRuler.SetTwoTone(scrollBeyondZero);
   }
#endif
#endif
}

void AdornedRulerPanel::ReCreateButtons()
{
   // TODO: Should we do this to destroy the grabber??
   // Get rid of any children we may have
   // DestroyChildren();

   ToolBar::MakeButtonBackgroundsSmall();
   SetBackgroundColour(theTheme.Colour( clrMedium ));

   for (auto & button : mButtons) {
      if (button)
         button->Destroy();
      button = nullptr;
   }

   size_t iButton = 0;
   // Make the short row of time ruler pushbottons.
   // Don't bother with sizers.  Their sizes and positions are fixed.
   // Add a grabber converted to a spacer.
   // This makes it visually clearer that the button is a button.

   wxPoint position( 1, 0 );

   Grabber * pGrabber = safenew Grabber(this, {});
   pGrabber->SetAsSpacer( true );
   //pGrabber->SetSize( 10, 27 ); // default is 10,27
   pGrabber->SetPosition( position );

   position.x = 12;

   auto size = theTheme.ImageSize( bmpRecoloredUpSmall );
   size.y = std::min(size.y, GetRulerHeight(false));

   auto buttonMaker = [&]
   (wxWindowID id, teBmps bitmap, bool toggle)
   {
      const auto button =
      ToolBar::MakeButton(
         this,
         bmpRecoloredUpSmall, bmpRecoloredDownSmall, 
         bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall, 
         bitmap, bitmap, bitmap,
         id, position, toggle, size
      );

      position.x += size.GetWidth();
      mButtons[iButton++] = button;
      return button;
   };
   auto button = buttonMaker(OnTogglePinnedStateID, bmpPlayPointerPinned, true);
   ToolBar::MakeAlternateImages(
	   *button, 3,
	   bmpRecoloredUpSmall, bmpRecoloredDownSmall,
	   bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall,
	   //bmpUnpinnedPlayHead, bmpUnpinnedPlayHead, bmpUnpinnedPlayHead,
	   bmpRecordPointer, bmpRecordPointer, bmpRecordPointer,
	   size);
   ToolBar::MakeAlternateImages(
	   *button, 2,
	   bmpRecoloredUpSmall, bmpRecoloredDownSmall,
	   bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall,
	   //bmpUnpinnedPlayHead, bmpUnpinnedPlayHead, bmpUnpinnedPlayHead,
	   bmpRecordPointerPinned, bmpRecordPointerPinned, bmpRecordPointerPinned,
	   size);
   ToolBar::MakeAlternateImages(
      *button, 1,
      bmpRecoloredUpSmall, bmpRecoloredDownSmall, 
      bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall, 
      //bmpUnpinnedPlayHead, bmpUnpinnedPlayHead, bmpUnpinnedPlayHead,
      bmpPlayPointer, bmpPlayPointer, bmpPlayPointer,
      size);

   UpdateButtonStates();
}

void AdornedRulerPanel::InvalidateRuler()
{
   mRuler.Invalidate();
}

namespace {
   const TranslatableString StartScrubbingMessage(const Scrubber &/*scrubber*/)
   {
#if 0
      if(scrubber.Seeks())
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
         return XO("Click or drag to begin Seek");
      else
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
         return XO("Click or drag to begin Scrub");
#else
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
      return XO("Click & move to Scrub. Click & drag to Seek.");
#endif
   }

   const TranslatableString ContinueScrubbingMessage(
      const Scrubber &scrubber, bool clicked)
   {
#if 0
      if(scrubber.Seeks())
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
         return XO("Move to Seek");
      else
      /* i18n-hint: These commands assist the user in finding a sound by ear. ...
       "Scrubbing" is variable-speed playback, ...
       "Seeking" is normal speed playback but with skips
       */
         return XO("Move to Scrub");
#else
      if( clicked ) {
         // Since mouse is down, mention dragging first.
         // IsScrubbing is true if Scrubbing OR seeking.
         if( scrubber.IsScrubbing() )
            // User is dragging already, explain.
            return XO("Drag to Seek. Release to stop seeking.");
         else
            // User has clicked but not yet moved or released.
            return XO("Drag to Seek. Release and move to Scrub.");
      }
      // Since mouse is up, mention moving first.
      return XO("Move to Scrub. Drag to Seek.");
#endif
   }

   const TranslatableString ScrubbingMessage(const Scrubber &scrubber, bool clicked)
   {
      if (scrubber.HasMark())
         return ContinueScrubbingMessage(scrubber, clicked);
      else
         return StartScrubbingMessage(scrubber);
   }
}

void AdornedRulerPanel::OnIdle( wxIdleEvent &evt )
{
   evt.Skip();
   DoIdle();
}

void AdornedRulerPanel::DoIdle()
{
   bool changed = UpdateRects();
   changed = SetPanelSize() || changed;

   auto &project = *mProject;
   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;
   const auto &playRegion = viewInfo.playRegion;

   changed = changed
     || mLastDrawnSelectedRegion != selectedRegion
     || mLastDrawnPlayRegion != std::pair{
         playRegion.GetLastActiveStart(), playRegion.GetLastActiveEnd() }
     || mLastDrawnH != viewInfo.h
     || mLastDrawnZoom != viewInfo.GetZoom()
     || mLastPlayRegionActive != viewInfo.playRegion.Active()
   ;
   if (changed)
      // Cause ruler redraw anyway, because we may be zooming or scrolling,
      // showing or hiding the scrub bar, etc.
      Refresh();
}

void AdornedRulerPanel::OnAudioStartStop(AudioIOEvent evt)
{
   if (evt.type == AudioIOEvent::MONITOR)
      return;
   if ( evt.type == AudioIOEvent::CAPTURE ) {
      if (evt.on)
      {
         mIsRecording = true;
         this->CellularPanel::CancelDragging( false );
         this->CellularPanel::ClearTargets();

         UpdateButtonStates();
      }
      else {
         mIsRecording = false;
         UpdateButtonStates();
      }
   }

   if ( !evt.on )
      // So that the play region is updated
      DoSelectionChange( mViewInfo->selectedRegion );
}

void AdornedRulerPanel::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   const auto &viewInfo = ViewInfo::Get( *GetProject() );
   const auto &playRegion = viewInfo.playRegion;
   const auto playRegionBounds = std::pair{
      playRegion.GetLastActiveStart(), playRegion.GetLastActiveEnd() };
   mLastDrawnH = viewInfo.h;
   mLastDrawnZoom = viewInfo.GetZoom();
   mLastDrawnPlayRegion = playRegionBounds;
   mLastDrawnSelectedRegion = viewInfo.selectedRegion;
   // To do, note other fisheye state when we have that

   wxPaintDC dc(this);

   auto &backDC = GetBackingDCForRepaint();

   DoDrawBackground(&backDC);

   // Find play region rectangle, selected rectangle, and their overlap
   const auto rectP = PlayRegionRectangle(),
      rectS = SelectedRegionRectangle(),
      rectO = rectP.Intersect(rectS);

   // What's left and right of the overlap?  Assume same tops and bottoms
   const auto top = rectP.GetTop(),
      bottom = rectP.GetBottom();
   wxRect rectL{
      wxPoint{ 0, top }, wxPoint{ this->GetSize().GetWidth() - 1, bottom } };
   wxRect rectR = {};
   if (!rectO.IsEmpty()) {
      rectR = { wxPoint{ rectO.GetRight() + 1, top }, rectL.GetBottomRight() };
      rectL = { rectL.GetTopLeft(), wxPoint{ rectO.GetLeft() - 1, bottom } };
   }

   DoDrawPlayRegion(&backDC, rectP, rectL, rectR);
   DoDrawOverlap(&backDC, rectO);
   DoDrawSelection(&backDC, rectS, rectL, rectR);

   DoDrawPlayRegionLimits(&backDC, rectP);

   DoDrawMarks(&backDC, true);

   DoDrawEdge(&backDC);

   DisplayBitmap(dc);

   // Stroke extras direct to the client area,
   // maybe outside of the damaged area
   // As with TrackPanel, do not make a NEW wxClientDC or else Mac flashes badly!
   dc.DestroyClippingRegion();
   DrawOverlays(true, &dc);
}

void AdornedRulerPanel::OnSize(wxSizeEvent &evt)
{
   mOuter = GetClientRect();
   if (mOuter.GetWidth() == 0 || mOuter.GetHeight() == 0)
   {
      return;
   }

   UpdateRects();

   OverlayPanel::OnSize(evt);
}

void AdornedRulerPanel::OnLeave(wxMouseEvent& evt)
{
   evt.Skip();
   CallAfter([this]{
      DrawBothOverlays();
   });
}

void AdornedRulerPanel::OnThemeChange(ThemeChangeMessage message)
{
   if (message.appearance)
      return;
   ReCreateButtons();
}

void AdornedRulerPanel::OnSelectionChange(Observer::Message)
{
   auto &selectedRegion = mViewInfo->selectedRegion;
   DoSelectionChange( selectedRegion );
}

void AdornedRulerPanel::DoSelectionChange(
   const SelectedRegion &selectedRegion )
{

   auto gAudioIO = AudioIOBase::Get();
   if ( !ViewInfo::Get( *mProject ).playRegion.Active() ) {
      // "Inactivated" play region follows the selection.
      SetPlayRegion( selectedRegion.t0(), selectedRegion.t1() );
   }
}

bool AdornedRulerPanel::UpdateRects()
{
   auto inner = mOuter;
   wxRect scrubZone;
   inner.x += LeftMargin;
   inner.width -= (LeftMargin + RightMargin);

   auto top = &inner;
   auto bottom = &inner;

   if (ShowingScrubRuler()) {
      scrubZone = inner;
      auto scrubHeight = std::min(scrubZone.height, (int)(ScrubHeight));

      int topHeight;
#ifdef SCRUB_ABOVE
      top = &scrubZone, topHeight = scrubHeight;
#else
      auto qpHeight = scrubZone.height - scrubHeight;
      bottom = &scrubZone, topHeight = qpHeight;
      // Increase scrub zone height so that hit testing finds it and
      // not QP region, when on bottom 'edge'.
      scrubZone.height+=BottomMargin;
#endif

      top->height = topHeight;
      bottom->height -= topHeight;
      bottom->y += topHeight;
   }

   top->y += TopMargin;
   top->height -= TopMargin;

   bottom->height -= BottomMargin;

   if (!ShowingScrubRuler())
      scrubZone = inner;

   if ( inner == mInner && scrubZone == mScrubZone )
      // no changes
      return false;

   mInner = inner;
   mScrubZone = scrubZone;

   mRuler.SetBounds(mInner.GetLeft(),
                    mInner.GetTop(),
                    mInner.GetRight(),
                    mInner.GetBottom());

   return true;
}

double AdornedRulerPanel::Pos2Time(int p, bool ignoreFisheye) const
{
   return mViewInfo->PositionToTime(p, mLeftOffset
      , ignoreFisheye
   );
}

int AdornedRulerPanel::Time2Pos(double t, bool ignoreFisheye) const
{
   return mViewInfo->TimeToPosition(t, mLeftOffset
      , ignoreFisheye
   );
}

bool AdornedRulerPanel::IsWithinMarker(int mousePosX, double markerTime)
{
   if (markerTime < 0)
      return false;

   int pixelPos = Time2Pos(markerTime);
   int boundLeft = pixelPos - SELECT_TOLERANCE_PIXEL;
   int boundRight = pixelPos + SELECT_TOLERANCE_PIXEL;

   return mousePosX >= boundLeft && mousePosX < boundRight;
}

#ifdef QUICK_PLAY_HANDLE
auto AdornedRulerPanel::QPHandle::Click(
   const TrackPanelMouseEvent &event, AudacityProject *pProject) -> Result
{
   auto result = CommonRulerHandle::Click(event, pProject);
   if (!( result & RefreshCode::Cancelled )) {
      if (mClicked == Button::Left) {
         if (!mParent)
            return RefreshCode::Cancelled;

         auto &scrubber = Scrubber::Get( *pProject );
         if(scrubber.HasMark()) {
            // We can't stop scrubbing yet (see comments in Bug 1391),
            // but we can pause it.
            ProjectAudioManager::Get( *pProject ).OnPause();
         }

         // Store the initial play region state
         const auto &viewInfo = ViewInfo::Get( *pProject );
         const auto &playRegion = viewInfo.playRegion;
         mParent->mOldPlayRegion = playRegion;

         // Save old selection, in case drag of selection is cancelled
         mOldSelection = ViewInfo::Get( *pProject ).selectedRegion;

         mParent->HandleQPClick( event.event, mX );
         mParent->HandleQPDrag( event.event, mX );
      }
   }
   
   return result;
}

void AdornedRulerPanel::HandleQPClick(wxMouseEvent &evt, wxCoord mousePosX)
{
   // Temporarily inactivate play region
   if (mOldPlayRegion.Active() && evt.LeftDown()) {
      //mPlayRegionLock = true;
      SelectUtilities::InactivatePlayRegion(*mProject);
   }

   mLeftDownClickUnsnapped[0] = mQuickPlayPosUnsnapped[0];
   mLeftDownClick = mQuickPlayPos[0];
   bool isWithinStart = IsWithinMarker(mousePosX, mOldPlayRegion.GetStart());
   bool isWithinEnd = IsWithinMarker(mousePosX, mOldPlayRegion.GetEnd());

   if (isWithinStart || isWithinEnd) {
      // If Quick-Play is playing from a point, we need to treat it as a click
      // not as dragging.
      if (mOldPlayRegion.Empty())
         mMouseEventState = mesSelectingPlayRegionClick;
      // otherwise check which marker is nearer
      else {
         // Don't compare times, compare positions.
         //if (fabs(mQuickPlayPos[0] - mPlayRegionStart) < fabs(mQuickPlayPos[0] - mPlayRegionEnd))
         auto start = mOldPlayRegion.GetStart();
         auto end = mOldPlayRegion.GetEnd();
         if (abs(Time2Pos(mQuickPlayPos[0]) - Time2Pos(start)) <
             abs(Time2Pos(mQuickPlayPos[0]) - Time2Pos(end)))
            mMouseEventState = mesDraggingPlayRegionStart;
         else
            mMouseEventState = mesDraggingPlayRegionEnd;
      }
   }
   else {
      // Clicked but not yet dragging
      mMouseEventState = mesSelectingPlayRegionClick;
   }
}

auto AdornedRulerPanel::QPHandle::Drag(
   const TrackPanelMouseEvent &event, AudacityProject *pProject) -> Result
{
   auto result = CommonRulerHandle::Drag(event, pProject);
   if (!( result & RefreshCode::Cancelled )) {
      if (mClicked == Button::Left) {
         if ( mParent ) {
            mX = event.event.m_x;
            mParent->UpdateQuickPlayPos( mX );
            mParent->HandleQPDrag( event.event, mX );
         }
      }
   }
   return result;
}

void AdornedRulerPanel::HandleQPDrag(wxMouseEvent &/*event*/, wxCoord mousePosX)
{
   bool isWithinClick =
      (mLeftDownClickUnsnapped >= 0) &&
      IsWithinMarker(mousePosX, mLeftDownClickUnsnapped);
   bool isWithinStart = IsWithinMarker(mousePosX, mOldPlayRegion.GetStart());
   bool isWithinEnd = IsWithinMarker(mousePosX, mOldPlayRegion.GetEnd());
   bool canDragSel = !mOldPlayRegion.Active() && mPlayRegionDragsSelection;
   auto &viewInfo = ViewInfo::Get( *GetProject() );
   auto &playRegion = viewInfo.playRegion;

   switch (mMouseEventState)
   {
      case mesNone:
         // If close to either end of play region, snap to closest
         if (isWithinStart || isWithinEnd) {
            if (fabs(mQuickPlayPos[0] - mOldPlayRegion.GetStart()) < fabs(mQuickPlayPos[0] - mOldPlayRegion.GetEnd()))
               mQuickPlayPos[0] = mOldPlayRegion.GetStart();
            else
               mQuickPlayPos[0] = mOldPlayRegion.GetEnd();
         }
         break;
      case mesDraggingPlayRegionStart:
         // Don't start dragging until beyond tolerance initial playback start
         if (!mIsDragging && isWithinStart)
            mQuickPlayPos[0] = mOldPlayRegion.GetStart();
         else
            mIsDragging = true;
         // avoid accidental tiny selection
         if (isWithinEnd)
            mQuickPlayPos[0] = mOldPlayRegion.GetEnd();
         playRegion.SetStart( mQuickPlayPos[0] );
         if (canDragSel) {
            DragSelection(*GetProject());
         }
         break;
      case mesDraggingPlayRegionEnd:
         if (!mIsDragging && isWithinEnd) {
            mQuickPlayPos[0] = mOldPlayRegion.GetEnd();
         }
         else
            mIsDragging = true;
         if (isWithinStart) {
            mQuickPlayPos[0] = mOldPlayRegion.GetStart();
         }
         playRegion.SetEnd( mQuickPlayPos[0] );
         if (canDragSel) {
            DragSelection(*GetProject());
         }
         break;
      case mesSelectingPlayRegionClick:

         // Don't start dragging until mouse is beyond tolerance of initial click.
         if (isWithinClick || mLeftDownClick == -1) {
            mQuickPlayPos[0] = mLeftDownClick;
            playRegion.SetTimes(mLeftDownClick, mLeftDownClick);
         }
         else {
            mMouseEventState = mesSelectingPlayRegionRange;
         }
         break;
      case mesSelectingPlayRegionRange:
         if (isWithinClick) {
            mQuickPlayPos[0] = mLeftDownClick;
         }

         if (mQuickPlayPos[0] < mLeftDownClick)
            playRegion.SetTimes( mQuickPlayPos[0], mLeftDownClick );
         else
            playRegion.SetTimes( mLeftDownClick, mQuickPlayPos[0] );
         if (canDragSel) {
            DragSelection(*GetProject());
         }
         break;
   }
   Refresh();
   Update();
}
#endif

auto AdornedRulerPanel::ScrubbingHandle::Preview(
   const TrackPanelMouseState &, AudacityProject *pProject)
      -> HitTestPreview
{
   auto &scrubber = Scrubber::Get( *pProject );
   auto message = ScrubbingMessage(scrubber, mClicked == Button::Left);

   mParent->SetNumGuides(1);
   return {
      message,
      {},
      // Tooltip is same as status message, or blank
      ((mParent && mParent->mTimelineToolTip) ? message : TranslatableString{}),
   };
}

#ifdef QUICK_PLAY_HANDLE
auto AdornedRulerPanel::QPHandle::Preview(
   const TrackPanelMouseState &state, AudacityProject *pProject)
      -> HitTestPreview
{
   mParent->SetNumGuides(1);
   TranslatableString tooltip;
   #if 0
   if (mParent && mParent->mTimelineToolTip) {
      if (!mParent->mQuickPlayEnabled)
         tooltip = XO("Quick-Play disabled");
      else
         tooltip = XO("Quick-Play enabled");
   }
   #endif

   TranslatableString message;
   auto &scrubber = Scrubber::Get( *pProject );
   const bool scrubbing = scrubber.HasMark();
   if (scrubbing)
      // Don't distinguish zones
      message = ScrubbingMessage(scrubber, false);
   else
      // message = Insert timeline status bar message here
      ;

   static wxCursor cursorHand{ wxCURSOR_HAND };
   static wxCursor cursorSizeWE{ wxCURSOR_SIZEWE };
   
   bool showArrows = false;
   if (mParent)
      showArrows =
         (mClicked == Button::Left)
         || mParent->IsWithinMarker(
               state.state.m_x, mParent->mOldPlayRegion.GetStart())
         || mParent->IsWithinMarker(
               state.state.m_x, mParent->mOldPlayRegion.GetEnd());
   
   return {
      message,
      showArrows ? &cursorSizeWE : &cursorHand,
      tooltip,
   };
}

auto AdornedRulerPanel::QPHandle::Release(
   const TrackPanelMouseEvent &event, AudacityProject *pProject,
   wxWindow *pParent)
      -> Result
{
   // Keep a shared pointer to self.  Otherwise *this might get deleted
   // in HandleQPRelease on Windows!  Because there is an event-loop yield
   // stopping playback, which caused OnCaptureLost to be called, which caused
   // clearing of CellularPanel targets!
   auto saveMe = mParent->mQPCell->mHolder.lock();

   auto result = CommonRulerHandle::Release(event, pProject, pParent);
   if (!( result & RefreshCode::Cancelled )) {
      if (mClicked == Button::Left) {
         if ( mParent ) {
            mParent->HandleQPRelease( event.event );
            // Update the hot zones for cursor changes
            const auto &viewInfo = ViewInfo::Get( *pProject );
            const auto &playRegion = viewInfo.playRegion;
            mParent->mOldPlayRegion = playRegion;
         }
      }
   }
   return result;
}

void AdornedRulerPanel::HandleQPRelease(wxMouseEvent &evt)
{
   auto &viewInfo = ViewInfo::Get( *GetProject() );
   auto &playRegion = viewInfo.playRegion;
   playRegion.Order();

   const double t0 = mTracks->GetStartTime();
   const double t1 = mTracks->GetEndTime();
   const auto &selectedRegion = viewInfo.selectedRegion;
   const double sel0 = selectedRegion.t0();
   const double sel1 = selectedRegion.t1();

   // We want some audio in the selection, but we allow a dragged
   // region to include selected white-space and space before audio start.
   if (evt.ShiftDown() && playRegion.Empty()) {
      // Looping the selection or project.
      // Disable if track selection is in white-space beyond end of tracks and
      // play position is outside of track contents.
      if (((sel1 < t0) || (sel0 > t1)) &&
          ((playRegion.GetStart() < t0) || (playRegion.GetStart() > t1))) {
         ClearPlayRegion();
      }
   }
   // Disable if beyond end.
   else if (playRegion.GetStart() >= t1) {
      ClearPlayRegion();
   }
   // Disable if empty selection before start.
   // (allow Quick-Play region to include 'pre-roll' white space)
   else if (
      playRegion.GetEnd() - playRegion.GetStart() > 0.0 &&
      playRegion.GetEnd() < t0
   ) {
      ClearPlayRegion();
   }

   mMouseEventState = mesNone;
   mIsDragging = false;
   mLeftDownClick = -1;

   auto cleanup = finally( [&] {
      if (mOldPlayRegion.Active()) {
         // Restore Locked Play region
         SetPlayRegion(mOldPlayRegion.GetStart(), mOldPlayRegion.GetEnd());
         SelectUtilities::ActivatePlayRegion(*mProject);
         // and release local lock
         mOldPlayRegion.SetActive( false );
      }
   } );

   StartQPPlay(!evt.ShiftDown(), evt.ControlDown());
}

auto AdornedRulerPanel::QPHandle::Cancel(AudacityProject *pProject) -> Result
{
   auto result = CommonRulerHandle::Cancel(pProject);

   if (mClicked == Button::Left) {
      if( mParent ) {
         ViewInfo::Get( *pProject ).selectedRegion = mOldSelection;
         mParent->mMouseEventState = mesNone;
         mParent->SetPlayRegion(
            mParent->mOldPlayRegion.GetStart(), mParent->mOldPlayRegion.GetEnd());
         if (mParent->mOldPlayRegion.Active()) {
            // Restore Locked Play region
            SelectUtilities::ActivatePlayRegion(*pProject);
            // and release local lock
            mParent->mOldPlayRegion.SetActive( false );
         }
      }
   }

   return result;
}
#endif

void AdornedRulerPanel::StartQPPlay(
   bool newDefault, bool cutPreview, const double *pStartTime)
{
   const double t0 = mTracks->GetStartTime();
   const double t1 = mTracks->GetEndTime();
   auto &viewInfo = ViewInfo::Get( *mProject );
   const auto &playRegion = viewInfo.playRegion;
   const auto &selectedRegion = viewInfo.selectedRegion;
   const double sel0 = selectedRegion.t0();
   const double sel1 = selectedRegion.t1();

   // Start / Restart playback on left click.
   bool startPlaying = true; // = (playRegion.GetStart() >= 0);

   if (startPlaying) {
      bool loopEnabled = true;
      auto oldStart = std::max(0.0, playRegion.GetStart());
      double start = oldStart, end = 0;

      if (playRegion.Empty()) {
         // Play either a selection or the project.
         if (oldStart > sel0 && oldStart < sel1) {
            // we are in a selection, so use the selection
            start = sel0;
            end = sel1;
         } // not in a selection, so use the project
         else {
            start = t0;
            end = t1;
         }
      }
      else
         end = std::max(start, playRegion.GetEnd());

      // Looping a tiny selection may freeze, so just play it once.
      loopEnabled = ((end - start) > 0.001)? true : false;

      newDefault = (loopEnabled && newDefault);
      if (newDefault)
         cutPreview = false;
      auto options = ProjectAudioIO::GetDefaultOptions(*mProject, newDefault);

      if (!cutPreview) {
         if (pStartTime)
            options.pStartTime.emplace(*pStartTime);
      }
      else
         options.envelope = nullptr;

      auto mode =
         cutPreview ? PlayMode::cutPreviewPlay
         : newDefault ? PlayMode::loopedPlay
         : PlayMode::normalPlay;

      // Stop only after deciding where to start again, because an event
      // callback may change the play region back to the selection
      auto &projectAudioManager = ProjectAudioManager::Get( *mProject );
      projectAudioManager.Stop();

      // Don't change play region, assume caller set it as needed
      // playRegion.SetTimes( start, end );
      // Refresh();

      projectAudioManager.PlayPlayRegion((SelectedRegion(start, end)),
                          options, mode,
                          false);

   }
}

#if 0
// This version toggles ruler state indirectly via the scrubber
// to ensure that all the places where the state is shown update.
// For example buttons and menus must update.
void AdornedRulerPanel::OnToggleScrubRulerFromMenu(wxCommandEvent&)
{
   auto &scrubber = Scrubber::Get( *mProject );
   scrubber.OnToggleScrubRuler(*mProject);
}
#endif


bool AdornedRulerPanel::SetPanelSize()
{
   const auto oldSize = GetSize();
   wxSize size { oldSize.GetWidth(), GetRulerHeight(ShowingScrubRuler()) };
   if ( size != oldSize ) {
      SetSize(size);
      SetMinSize(size);
      PostSizeEventToParent();
      return true;
   }
   else
      return false;
}

void AdornedRulerPanel::DrawBothOverlays()
{
   auto pCellularPanel =
      dynamic_cast<CellularPanel*>( &GetProjectPanel( *GetProject() ) );
   if ( !pCellularPanel ) {
      wxASSERT( false );
   }
   else
      pCellularPanel->DrawOverlays( false );
   DrawOverlays( false );
}

void AdornedRulerPanel::UpdateButtonStates()
{
   auto common = [this](
      AButton &button, const CommandID &commandName, const TranslatableString &label) {
      ComponentInterfaceSymbol command{ commandName, label };
      ToolBar::SetButtonToolTip( *mProject, button, &command, 1u );
      button.SetLabel( Verbatim( button.GetToolTipText() ) );

      button.UpdateStatus();
   };

   {
      // The button always reflects the pinned head preference, even though
      // there is also a Playback preference that may overrule it for scrubbing
      bool state = TracksPrefs::GetPinnedHeadPreference();
      auto pinButton = static_cast<AButton*>(FindWindow(OnTogglePinnedStateID));
      if( !state )
         pinButton->PopUp();
      else
         pinButton->PushDown();
      auto gAudioIO = AudioIO::Get();
      pinButton->SetAlternateIdx(
         (gAudioIO->IsCapturing() ? 2 : 0) + (state ? 0 : 1));
      // Bug 1584: Tooltip now shows what clicking will do.
      // Bug 2357: Action of button (and hence tooltip wording) updated.
      const auto label = XO("Timeline Options");
      common(*pinButton, wxT("PinnedHead"), label);
   }
}

void AdornedRulerPanel::OnPinnedButton(wxCommandEvent & /*event*/)
{
   ShowContextMenu(MenuChoice::QuickPlay, NULL);
}

void AdornedRulerPanel::OnTogglePinnedState(wxCommandEvent & /*event*/)
{
   TogglePinnedHead();
   UpdateButtonStates();
}

void AdornedRulerPanel::UpdateQuickPlayPos(wxCoord &mousePosX)
{
   // Invoked for mouse-over preview events, or dragging, or scrub position
   // polling updates.  Remember x coordinates, converted to times, for
   // drawing of guides.

   // Keep Quick-Play within usable track area.  (Dependent on zoom)
   const auto &viewInfo = ViewInfo::Get( *mProject );
   auto width = viewInfo.GetTracksUsableWidth();
   mousePosX = std::max(mousePosX, viewInfo.GetLeftOffset());
   mousePosX = std::min(mousePosX, viewInfo.GetLeftOffset() + width - 1);
   const auto time = Pos2Time(mousePosX);

   for (size_t ii = 0; ii < mNumGuides; ++ii) {
      mQuickPlayPosUnsnapped[ii] = mQuickPlayPos[ii] =
         time + mQuickPlayOffset[ii];
      HandleSnapping(ii);
   }
}

// Pop-up menus

void AdornedRulerPanel::ShowMenu(const wxPoint & pos)
{
   const auto &viewInfo = ViewInfo::Get( *GetProject() );
   const auto &playRegion = viewInfo.playRegion;
   wxMenu rulerMenu;

   auto pDrag = rulerMenu.AppendCheckItem(OnSyncQuickPlaySelID, _("Enable dragging selection"));
   pDrag->Check(mPlayRegionDragsSelection && playRegion.Active());
   pDrag->Enable(playRegion.Active());

   rulerMenu.AppendCheckItem(OnAutoScrollID, _("Update display while playing"))->
      Check(mViewInfo->bUpdateTrackIndicator);

   {
      auto item = rulerMenu.AppendCheckItem(OnTogglePlayRegionID,
         LoopToggleText.Stripped().Translation());
      item->Check(playRegion.Active());
   }

   {
      auto item = rulerMenu.Append(OnClearPlayRegionID,
         /* i18n-hint Clear is a verb */
         _("Clear Loop"));
   }

   {
      auto item = rulerMenu.Append(OnSetPlayRegionToSelectionID,
         _("Set Loop To Selection"));
   }

   rulerMenu.AppendSeparator();
   rulerMenu.AppendCheckItem(OnTogglePinnedStateID, _("Pinned Play Head"))->
      Check(TracksPrefs::GetPinnedHeadPreference());

   BasicMenu::Handle{ &rulerMenu }.Popup(
      wxWidgetsWindowPlacement{ this },
      { pos.x, pos.y }
   );
}

void AdornedRulerPanel::ShowScrubMenu(const wxPoint & pos)
{
   auto &scrubber = Scrubber::Get( *mProject );
   PushEventHandler(&scrubber);
   auto cleanup = finally([this]{ PopEventHandler(); });

   wxMenu rulerMenu;
   scrubber.PopulatePopupMenu(rulerMenu);
   BasicMenu::Handle{ &rulerMenu }.Popup(
      wxWidgetsWindowPlacement{ this },
      { pos.x, pos.y }
   );
}

void AdornedRulerPanel::OnSyncSelToQuickPlay(wxCommandEvent&)
{
   mPlayRegionDragsSelection = (mPlayRegionDragsSelection)? false : true;
   gPrefs->Write(wxT("/QuickPlay/DragSelection"), mPlayRegionDragsSelection);
   gPrefs->Flush();
}

void AdornedRulerPanel::DragSelection(AudacityProject &project)
{
   auto &viewInfo = ViewInfo::Get( project );
   const auto &playRegion = viewInfo.playRegion;
   auto &selectedRegion = viewInfo.selectedRegion;
   selectedRegion.setT0(playRegion.GetStart(), false);
   selectedRegion.setT1(playRegion.GetEnd(), true);
}

void AdornedRulerPanel::HandleSnapping(size_t index)
{
   // Play region dragging can snap to selection boundaries
   const auto &selectedRegion = ViewInfo::Get(*GetProject()).selectedRegion;
   SnapPointArray candidates;
   if (!mPlayRegionDragsSelection)
      candidates = {
         SnapPoint{ selectedRegion.t0() },
         SnapPoint{ selectedRegion.t1() },
      };
   SnapManager snapManager{ *mProject, *mTracks, *mViewInfo, move(candidates) };
   auto results = snapManager.Snap(nullptr, mQuickPlayPos[index], false);
   mQuickPlayPos[index] = results.outTime;
   mIsSnapped[index] = results.Snapped();
}

#if 0
void AdornedRulerPanel::OnTimelineToolTips(wxCommandEvent&)
{
   mTimelineToolTip = (mTimelineToolTip)? false : true;
   gPrefs->Write(wxT("/QuickPlay/ToolTips"), mTimelineToolTip);
   gPrefs->Flush();
}
#endif

void AdornedRulerPanel::OnAutoScroll(wxCommandEvent&)
{
   if (mViewInfo->bUpdateTrackIndicator)
      gPrefs->Write(wxT("/GUI/AutoScroll"), false);
   else
      gPrefs->Write(wxT("/GUI/AutoScroll"), true);

   gPrefs->Flush();

   PrefsListener::Broadcast(ViewInfo::UpdateScrollPrefsID());
}


void AdornedRulerPanel::OnTogglePlayRegion(wxCommandEvent&)
{
   SelectUtilities::TogglePlayRegion(*mProject);
}

void AdornedRulerPanel::OnClearPlayRegion(wxCommandEvent&)
{
   SelectUtilities::ClearPlayRegion(*mProject);
}

void AdornedRulerPanel::OnSetPlayRegionToSelection(wxCommandEvent&)
{
   SelectUtilities::SetPlayRegionToSelection(*mProject);
}


void AdornedRulerPanel::ShowContextMenu(
   MenuChoice choice, const wxPoint *pPosition)
{
   wxPoint position;
   if(pPosition)
      position = *pPosition;
   else
   {
      auto rect = GetRect();
      //Old code put menu too low down.  y position applied twice.
      //position = { rect.GetLeft() + 1, rect.GetBottom() + 1 };

      // The cell does not pass in the mouse or button position.
      // We happen to know this is the pin/unpin button
      // so these magic values 'fix a bug' - but really the cell should
      // pass more information to work with in.
      position = { rect.GetLeft() + 38, rect.GetHeight()/2 + 1 };
   }

   switch (choice) {
      case MenuChoice::QuickPlay:
         ShowMenu(position); 
         UpdateButtonStates();
         break;
      case MenuChoice::Scrub:
         ShowScrubMenu(position); break;
      default:
         return;
   }
}

using ColorId = decltype(clrTrackInfo);

inline ColorId TimelineBackgroundColor()
{
   return clrTrackInfo;
}

inline ColorId TimelineTextColor()
{
   return clrTrackPanelText;
}

inline ColorId TimelineLimitsColor()
{
   return TimelineTextColor();
}

inline ColorId TimelineLoopRegionColor(bool isActive)
{
   return isActive ? clrRulerBackground : clrClipAffordanceInactiveBrush;
}

static inline wxColour AlphaBlend(ColorId fg, ColorId bg, double alpha)
{
   const auto &fgc = theTheme.Colour(fg);
   const auto &bgc = theTheme.Colour(bg);
   return wxColour{
      wxColour::AlphaBlend(fgc.Red(), bgc.Red(), alpha),
      wxColour::AlphaBlend(fgc.Green(), bgc.Green(), alpha),
      wxColour::AlphaBlend(fgc.Blue(), bgc.Blue(), alpha)
   };
}

void AdornedRulerPanel::DoDrawBackground(wxDC * dc)
{
   // Draw AdornedRulerPanel border
   AColor::UseThemeColour( dc, TimelineBackgroundColor() );
   dc->DrawRectangle( mInner );

   if (ShowingScrubRuler()) {
      // Let's distinguish the scrubbing area by using a themable
      // colour and a line to set it off.  
      AColor::UseThemeColour(dc, clrScrubRuler, TimelineTextColor() );
      wxRect ScrubRect = mScrubZone;
      ScrubRect.Inflate( 1,0 );
      dc->DrawRectangle(ScrubRect);
   }
}

void AdornedRulerPanel::DoDrawEdge(wxDC *dc)
{
   wxRect r = mOuter;
   r.width -= RightMargin;
   r.height -= BottomMargin;
   AColor::BevelTrackInfo( *dc, true, r );

   // Black stroke at bottom
   dc->SetPen( *wxBLACK_PEN );
   AColor::Line( *dc, mOuter.x,
                mOuter.y + mOuter.height - 1,
                mOuter.x + mOuter.width - 1	,
                mOuter.y + mOuter.height - 1 );
}

void AdornedRulerPanel::DoDrawMarks(wxDC * dc, bool /*text */ )
{
   const double min = Pos2Time(0);
   const double hiddenMin = Pos2Time(0, true);
   const double max = Pos2Time(mInner.width);
   const double hiddenMax = Pos2Time(mInner.width, true);

   mRuler.SetTickColour( theTheme.Colour( TimelineTextColor() ) );
   mRuler.SetRange( min, max, hiddenMin, hiddenMax );
   mRuler.Draw( *dc );
}

void AdornedRulerPanel::DrawSelection()
{
   Refresh();
}

wxRect AdornedRulerPanel::PlayRegionRectangle() const
{
   const auto &viewInfo = ViewInfo::Get(*mProject);
   const auto &playRegion = viewInfo.playRegion;
   const auto t0 = playRegion.GetLastActiveStart(),
      t1 = playRegion.GetLastActiveEnd();
   return RegionRectangle(t0, t1);
}

wxRect AdornedRulerPanel::SelectedRegionRectangle() const
{
   const auto &viewInfo = ViewInfo::Get(*mProject);
   const auto &selectedRegion = viewInfo.selectedRegion;
   const auto t0 = selectedRegion.t0(), t1 = selectedRegion.t1();
   return RegionRectangle(t0, t1);
}

wxRect AdornedRulerPanel::RegionRectangle(double t0, double t1) const
{
   int p0 = -1, p1 = -1;
   if (t0 == t1)
      // Make the rectangle off-screen horizontally, but set the height
      ;
   else {
      p0 = max(1, Time2Pos(t0));
      p1 = min(mInner.width, Time2Pos(t1));
   }

   const int left = p0, top = mInner.y, right = p1, bottom = mInner.GetBottom();
   return { wxPoint{left, top}, wxPoint{right, bottom} };
}

void AdornedRulerPanel::DoDrawPlayRegion(
   wxDC * dc, const wxRect &rectP, const wxRect &rectL, const wxRect &rectR)
{
   const auto &viewInfo = ViewInfo::Get(*mProject);
   const auto &playRegion = viewInfo.playRegion;
   if (playRegion.IsLastActiveRegionClear())
      return;

   const bool isActive = (mLastPlayRegionActive = playRegion.Active());

   // Paint the selected region bolder if independently varying, else dim
   const auto color = TimelineLoopRegionColor(isActive);
   dc->SetBrush( wxBrush( theTheme.Colour( color )) );
   dc->SetPen(   wxPen(   theTheme.Colour( color )) );

   dc->DrawRectangle( rectP.Intersect(rectL) );
   dc->DrawRectangle( rectP.Intersect(rectR) );
}

void AdornedRulerPanel::DoDrawPlayRegionLimits(wxDC * dc, const wxRect &rect)
{
   // Color the edges of the play region like the ticks and numbers
   ADCChanger cleanup( dc );
   const auto edgeColour = theTheme.Colour(TimelineLimitsColor());
   dc->SetPen( { edgeColour } );
   dc->SetBrush( { edgeColour } );

   constexpr int side = 7;
   constexpr int sideLessOne = side - 1;

   // Paint two shapes, each a line plus triangle at bottom
   const auto left = rect.GetLeft(),
      right = rect.GetRight(),
      bottom = rect.GetBottom(),
      top = rect.GetTop();
   {
      wxPoint points[]{
         {left, bottom - sideLessOne},
         {left - sideLessOne, bottom},
         {left, bottom},
         {left, top},
      };
      dc->DrawPolygon( 4, points );
   }

   {
      wxPoint points[]{
         {right, top},
         {right, bottom},
         {right + sideLessOne, bottom},
         {right, bottom - sideLessOne},
      };
      dc->DrawPolygon( 4, points );
   }
}

constexpr double SelectionOpacity = 0.2;

void AdornedRulerPanel::DoDrawOverlap(wxDC * dc, const wxRect &rect)
{
   dc->SetBrush( wxBrush{ AlphaBlend(
      TimelineLimitsColor(), TimelineLoopRegionColor(mLastPlayRegionActive),
      SelectionOpacity) } );
   dc->SetPen( *wxTRANSPARENT_PEN );
   dc->DrawRectangle( rect );
}

void AdornedRulerPanel::DoDrawSelection(
   wxDC * dc, const wxRect &rectS, const wxRect &rectL, const wxRect &rectR)
{
   dc->SetBrush( wxBrush{ AlphaBlend(
      TimelineLimitsColor(), TimelineBackgroundColor(), SelectionOpacity) } );
   dc->SetPen( *wxTRANSPARENT_PEN );
   dc->DrawRectangle( rectS.Intersect(rectL) );
   dc->DrawRectangle( rectS.Intersect(rectR) );
}

int AdornedRulerPanel::GetRulerHeight(bool showScrubBar)
{
   return ProperRulerHeight + (showScrubBar ? ScrubHeight : 0);
}

void AdornedRulerPanel::SetLeftOffset(int offset)
{
   if (mLeftOffset != offset) {
      mLeftOffset = offset;
      mUpdater.SetData(mViewInfo, offset);
      mRuler.Invalidate();
   }
}

// Draws the scrubbing/seeking indicator.
void AdornedRulerPanel::DoDrawScrubIndicator(
   wxDC * dc, wxCoord xx, int width, bool scrub, bool seek)
{
   ADCChanger changer(dc); // Undo pen and brush changes at function exit

   wxPoint tri[ 3 ];
   if (seek) {
      auto height = IndicatorHeightForWidth(width);
      // Make four triangles
      const int TriangleWidth = width * 3 / 8;

      // Double-double headed, left-right
      auto yy = ShowingScrubRuler()
      ? mScrubZone.y
      : (mInner.GetBottom() + 1) - 1 /* bevel */ - height;
      tri[ 0 ].x = xx - IndicatorOffset;
      tri[ 0 ].y = yy;
      tri[ 1 ].x = xx - IndicatorOffset;
      tri[ 1 ].y = yy + height;
      tri[ 2 ].x = xx - TriangleWidth;
      tri[ 2 ].y = yy + height / 2;
      dc->DrawPolygon( 3, tri );

      tri[ 0 ].x -= TriangleWidth;
      tri[ 1 ].x -= TriangleWidth;
      tri[ 2 ].x -= TriangleWidth;
      dc->DrawPolygon( 3, tri );

      tri[ 0 ].x = tri[ 1 ].x = xx + IndicatorOffset;
      tri[ 2 ].x = xx + TriangleWidth;
      dc->DrawPolygon( 3, tri );


      tri[ 0 ].x += TriangleWidth;
      tri[ 1 ].x += TriangleWidth;
      tri[ 2 ].x += TriangleWidth;
      dc->DrawPolygon( 3, tri );
   }
   else if (scrub) {
      auto height = IndicatorHeightForWidth(width);
      const int IndicatorHalfWidth = width / 2;

      // Double headed, left-right
      auto yy = ShowingScrubRuler()
         ? mScrubZone.y
         : (mInner.GetBottom() + 1) - 1 /* bevel */ - height;
      tri[ 0 ].x = xx - IndicatorOffset;
      tri[ 0 ].y = yy;
      tri[ 1 ].x = xx - IndicatorOffset;
      tri[ 1 ].y = yy + height;
      tri[ 2 ].x = xx - IndicatorHalfWidth;
      tri[ 2 ].y = yy + height / 2;
      dc->DrawPolygon( 3, tri );
      tri[ 0 ].x = tri[ 1 ].x = xx + IndicatorOffset;
      tri[ 2 ].x = xx + IndicatorHalfWidth;
      dc->DrawPolygon( 3, tri );
   }
}

void AdornedRulerPanel::SetPlayRegion(
   double playRegionStart, double playRegionEnd)
{
   // This is called by AudacityProject to make the play region follow
   // the current selection. But while the user is selecting a play region
   // with the mouse directly in the ruler, changes from outside are blocked.
   if (mMouseEventState != mesNone)
      return;

   auto &viewInfo = ViewInfo::Get( *GetProject() );
   auto &playRegion = viewInfo.playRegion;
   playRegion.SetTimes( playRegionStart, playRegionEnd );

   Refresh();
}

void AdornedRulerPanel::ClearPlayRegion()
{
   ProjectAudioManager::Get( *mProject ).Stop();

   auto &viewInfo = ViewInfo::Get( *GetProject() );
   auto &playRegion = viewInfo.playRegion;
   playRegion.SetTimes( -1, -1 );

   Refresh();
}

void AdornedRulerPanel::GetMaxSize(wxCoord *width, wxCoord *height)
{
   mRuler.GetMaxSize(width, height);
}

bool AdornedRulerPanel::s_AcceptsFocus{ false };

auto AdornedRulerPanel::TemporarilyAllowFocus() -> TempAllowFocus {
   s_AcceptsFocus = true;
   return TempAllowFocus{ &s_AcceptsFocus };
}

void AdornedRulerPanel::SetNumGuides(size_t nn)
{
   nn = std::min(nn, MAX_GUIDES);
   // If increasing the number of guides, reinitialize newer ones
   for (size_t ii = mNumGuides; ii < nn; ++ii) {
      mQuickPlayOffset[ii] = 0;
      mQuickPlayPosUnsnapped[ii] = 0;
      mQuickPlayPos[ii] = 0;
      mIsSnapped[ii] = false;
   }
   mNumGuides = nn;
}

void AdornedRulerPanel::SetFocusFromKbd()
{
   auto temp = TemporarilyAllowFocus();
   SetFocus();
}

// Second-level subdivision includes quick-play region and maybe the scrub bar
// and also shaves little margins above and below
struct AdornedRulerPanel::Subgroup final : TrackPanelGroup {
   explicit Subgroup( const AdornedRulerPanel &ruler ) : mRuler{ ruler } {}
   Subdivision Children( const wxRect & ) override
   {
      return { Axis::Y, ( mRuler.ShowingScrubRuler() )
         ? Refinement{
            { mRuler.mInner.GetTop(), mRuler.mQPCell },
            { mRuler.mScrubZone.GetTop(), mRuler.mScrubbingCell },
            { mRuler.mScrubZone.GetBottom() + 1, nullptr }
         }
         : Refinement{
            { mRuler.mInner.GetTop(), mRuler.mQPCell },
            { mRuler.mInner.GetBottom() + 1, nullptr }
         }
      };
   }
   const AdornedRulerPanel &mRuler;
};

// Top-level subdivision shaves little margins off left and right
struct AdornedRulerPanel::MainGroup final : TrackPanelGroup {
   explicit MainGroup( const AdornedRulerPanel &ruler ) : mRuler{ ruler } {}
   Subdivision Children( const wxRect & ) override
   { return { Axis::X, Refinement{
      // Subgroup is a throwaway object
      { mRuler.mInner.GetLeft(), std::make_shared< Subgroup >( mRuler ) },
      { mRuler.mInner.GetRight() + 1, nullptr }
   } }; }
   const AdornedRulerPanel &mRuler;
};

bool AdornedRulerPanel::ShowingScrubRuler() const
{
   auto &scrubber = Scrubber::Get( *GetProject() );
   return scrubber.ShowsBar();
}

// CellularPanel implementation
std::shared_ptr<TrackPanelNode> AdornedRulerPanel::Root()
{
   // Root is a throwaway object
   return std::make_shared< MainGroup >( *this );
}

AudacityProject * AdornedRulerPanel::GetProject() const
{
   return mProject;
}


TrackPanelCell *AdornedRulerPanel::GetFocusedCell()
{
   // No switching of focus yet to the other, scrub zone
   return mQPCell.get();
}


void AdornedRulerPanel::SetFocusedCell()
{
}


void AdornedRulerPanel::ProcessUIHandleResult(
   TrackPanelCell *, TrackPanelCell *, unsigned refreshResult)
{
   if (refreshResult & RefreshCode::RefreshAll)
      Refresh(); // Overlays will be repainted too
   else if (refreshResult & RefreshCode::DrawOverlays)
      DrawBothOverlays(); // cheaper redrawing of guidelines only
}

void AdornedRulerPanel::UpdateStatusMessage( const TranslatableString &message )
{
   ProjectStatus::Get( *GetProject() ).Set(message);
}

void AdornedRulerPanel::CreateOverlays()
{
   if (!mOverlay) {
      mOverlay =
         std::make_shared<TrackPanelGuidelineOverlay>( mProject );
      auto pCellularPanel =
         dynamic_cast<CellularPanel*>( &GetProjectPanel( *GetProject() ) );
      if ( !pCellularPanel ) {
         wxASSERT( false );
      }
      else
         pCellularPanel->AddOverlay( mOverlay );
      this->AddOverlay( mOverlay->mPartner );
   }
}

void AdornedRulerPanel::TogglePinnedHead()
{
   bool value = !TracksPrefs::GetPinnedHeadPreference();
   TracksPrefs::SetPinnedHeadPreference(value, true);
   ToolManager::ModifyAllProjectToolbarMenus();

   auto &project = *mProject;
   // Update button image
   UpdateButtonStates();

   auto &scrubber = Scrubber::Get( project );
   if (scrubber.HasMark())
      scrubber.SetScrollScrubbing(value);
}

// Attach menu item

#include "commands/CommandContext.h"
#include "commands/CommandManager.h"
#include "CommonCommandFlags.h"

namespace {
void OnTogglePinnedHead(const CommandContext &context)
{
   AdornedRulerPanel::Get( context.project ).TogglePinnedHead();
}

using namespace MenuTable;
using Options = CommandManager::Options;
AttachedItem sAttachment{
   { wxT("Transport/Other/Options/Part2"), { OrderingHint::Begin, {} } },
   Command( wxT("PinnedHead"), XXO("Pinned Play/Record &Head (on/off)"),
      OnTogglePinnedHead,
      // Switching of scrolling on and off is permitted
      // even during transport
      AlwaysEnabledFlag,
      Options{}.CheckTest([](const AudacityProject&){
         return TracksPrefs::GetPinnedHeadPreference(); } ) )
};
}
