/**********************************************************************

Audacity: A Digital Audio Editor

ProjectWindow.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_WINDOW__
#define __AUDACITY_PROJECT_WINDOW__

#include <memory>
#include <wx/frame.h> // to inherit
#include "TrackPanelListener.h" // to inherit

class AudacityProject;
class Track;

class wxScrollBar;
class wxPanel;

///\brief A top-level window associated with a project, and handling scrollbars
/// and zooming
class ProjectWindow final : public wxFrame
   , public TrackPanelListener
{
public:
   static ProjectWindow &Get( AudacityProject &project );
   static const ProjectWindow &Get( const AudacityProject &project );
   static ProjectWindow *Find( AudacityProject *pProject );
   static const ProjectWindow *Find( const AudacityProject *pProject );
   AudacityProject &GetProject() { return mProject; }

   explicit ProjectWindow(
      wxWindow * parent, wxWindowID id,
      const wxPoint & pos, const wxSize &size,
      AudacityProject &project );
   ~ProjectWindow() override;

   // Next available ID for sub-windows
   int NextWindowID();

   void Init();

   bool IsActive() override;
   bool IsIconized() const override;

   bool IsBeingDeleted() const { return mIsDeleting; }
   void SetIsBeingDeleted() { mIsDeleting = true; }

   wxWindow *GetMainPage() { return mMainPage; }
   wxPanel *GetTopPanel() { return mTopPanel; }

   class PlaybackScroller final : public wxEvtHandler
   {
   public:
      explicit PlaybackScroller(AudacityProject *project);

      enum class Mode {
         Off,
         Refresh,
         Pinned,
         Right,
      };

      Mode GetMode() const { return mMode; }
      void Activate(Mode mode)
      {
         mMode = mode;
      }

   private:
      void OnTimer(wxCommandEvent &event);

      AudacityProject *mProject;
      Mode mMode { Mode::Off };
   };
   PlaybackScroller &GetPlaybackScroller() { return *mPlaybackScroller; }

   void SetNormalizedWindowState(wxRect pSizeAndLocation) {  mNormalizedWindowState = pSizeAndLocation;   }
   wxRect GetNormalizedWindowState() const { return mNormalizedWindowState;   }

   void RedrawProject(const bool bForceWaveTracks = false);

   void Zoom(double level);
   void ZoomInByFactor( double ZoomFactor );
   void ZoomOutByFactor( double ZoomFactor );
   void ZoomBy(double multiplier);
   void ZoomAfterImport(Track *pTrack);
   double GetZoomOfToFit() const;
   void DoZoomFit();

   void ApplyUpdatedTheme();

   // Scrollbars

   wxScrollBar &GetVerticalScrollBar() { return *mVsbar; }

   void OnScrollLeft();
   void OnScrollRight();

   void Rewind(bool shift);
   void SkipEnd(bool shift);

   void OnScrollLeftButton(wxScrollEvent & event);
   void OnScrollRightButton(wxScrollEvent & event);

   void FinishAutoScroll();
   void FixScrollbars();

   bool MayScrollBeyondZero() const;
   double ScrollingLowerBoundTime() const;
   // How many pixels are covered by the period from lowermost scrollable time, to the given time:
   // PRL: Bug1197: we seem to need to compute all in double, to avoid differing results on Mac
   double PixelWidthBeforeTime(double scrollto) const;
   void SetHorizontalThumb(double scrollto);

   // PRL:  old and incorrect comment below, these functions are used elsewhere than TrackPanel
   // TrackPanel access
   wxSize GetTPTracksUsableArea() /* not override */;
   void RefreshTPTrack(Track* pTrk, bool refreshbacking = true) /* not override */;

   // TrackPanel callback methods, overrides of TrackPanelListener
   void TP_DisplaySelection() override;

   void TP_RedrawScrollbars() override;
   void TP_ScrollLeft() override;
   void TP_ScrollRight() override;
   void TP_ScrollWindow(double scrollto) override;
   bool TP_ScrollUpDown(int delta) override;
   void TP_HandleResize() override;

 private:

   void OnThemeChange(wxCommandEvent & evt);

 public:
   // Message Handlers

   void OnMenu(wxCommandEvent & event);
   void OnUpdateUI(wxUpdateUIEvent & event);

   void MacShowUndockedToolbars(bool show);
   void OnActivate(wxActivateEvent & event);

   void OnMouseEvent(wxMouseEvent & event);
   void OnIconize(wxIconizeEvent &event);
   void OnSize(wxSizeEvent & event);
   void HandleResize();
   void UpdateLayout();
   void OnShow(wxShowEvent & event);
   void OnMove(wxMoveEvent & event);
   void DoScroll();
   void OnScroll(wxScrollEvent & event);
   void OnToolBarUpdate(wxCommandEvent & event);
   void OnUndoRedo( wxCommandEvent & );
   void OnUndoReset( wxCommandEvent & );

   bool mbInitializingScrollbar{ false };

private:
   AudacityProject &mProject;
   wxRect mNormalizedWindowState;

   wxPanel *mTopPanel{};
   wxWindow * mMainPage{};
   wxPanel * mMainPanel{};
   wxScrollBar *mHsbar{};
   wxScrollBar *mVsbar{};

   int mNextWindowID{};

   bool mAutoScrolling{ false };
   bool mActive{ true };
   bool mIconized;
   bool mShownOnce{ false };



   bool mIsDeleting{ false };

private:

   std::unique_ptr<PlaybackScroller> mPlaybackScroller;

   DECLARE_EVENT_TABLE()
};

void GetDefaultWindowRect(wxRect *defRect);
void GetNextWindowPlacement(wxRect *nextRect, bool *pMaximized, bool *pIconized);

#endif
