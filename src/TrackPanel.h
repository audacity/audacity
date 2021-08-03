/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL__
#define __AUDACITY_TRACK_PANEL__




#include <vector>

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/timer.h> // to inherit

#include "HitTestResult.h"
#include "Prefs.h"

#include "SelectedRegion.h"

#include "CellularPanel.h"

#include "commands/CommandManagerWindowClasses.h"

// For context menu.
#include <wx/menu.h>
#include "commands/CommandManager.h"
#include "commands/CommandContext.h"
#include "SelectUtilities.h"
#include "menus/TrackMenus.h"
#include "menus/EditMenus.h"
#include "menus/FileMenus.h"

class wxRect;

class SpectrumAnalyst;
class Track;
class TrackList;
struct TrackListEvent;
class TrackPanel;
class TrackArtist;
class Ruler;
class SnapManager;
class AdornedRulerPanel;
class LWSlider;

class TrackPanelAx;

// Declared elsewhere, to reduce compilation dependencies
class TrackPanelListener;

struct TrackPanelDrawingContext;

enum class UndoPush : unsigned char;

enum {
   kTimerInterval = 50, // milliseconds
};

const int DragThreshold = 3;// Anything over 3 pixels is a drag, else a click.

class AUDACITY_DLL_API TrackPanel final
   : public CellularPanel
   , public NonKeystrokeInterceptingWindow
   , private PrefsListener
{
 public:
   static TrackPanel &Get( AudacityProject &project );
   static const TrackPanel &Get( const AudacityProject &project );
   static void Destroy( AudacityProject &project );
 
   TrackPanel(wxWindow * parent,
              wxWindowID id,
              const wxPoint & pos,
              const wxSize & size,
              const std::shared_ptr<TrackList> &tracks,
              ViewInfo * viewInfo,
              AudacityProject * project,
              AdornedRulerPanel * ruler );

   virtual ~ TrackPanel();

   void UpdatePrefs() override;

   void OnAudioIO(wxCommandEvent & evt);

   void OnPaint(wxPaintEvent & event);
   void OnMouseEvent(wxMouseEvent & event);
   void OnOutsideTrackContextMenu(wxCommandEvent& event);
   void OnOverTrackContextMenu(wxCommandEvent& event);
   void OnKeyDown(wxKeyEvent & event);

   void OnTrackListResizing(TrackListEvent & event);
   void OnTrackListDeletion(wxEvent & event);
   void OnEnsureVisible(TrackListEvent & event);
   void UpdateViewIfNoTracks(); // Call this to update mViewInfo, etc, after track(s) removal, before Refresh().

   double GetMostRecentXPos();

   void OnSize( wxSizeEvent & );
   void OnIdle(wxIdleEvent & event);
   void OnTimer(wxTimerEvent& event);
   void OnProjectSettingsChange(wxCommandEvent &event);
   void OnTrackFocusChange( wxCommandEvent &event );

   void OnUndoReset( wxCommandEvent &event );

   void Refresh
      (bool eraseBackground = true, const wxRect *rect = (const wxRect *) NULL)
      override;

   void RefreshTrack(Track *trk, bool refreshbacking = true);

   void HandlePageUpKey();
   void HandlePageDownKey();
   AudacityProject * GetProject() const override;

   void OnTrackMenu(Track *t = NULL);

   void VerticalScroll( float fracPosition);

   TrackPanelCell *GetFocusedCell() override;
   void SetFocusedCell() override;

   void UpdateVRulers();
   void UpdateVRuler(Track *t);
   void UpdateTrackVRuler(Track *t);
   void UpdateVRulerSize();

 protected:
   bool IsAudioActive();

public:
   size_t GetSelectedTrackCount() const;

protected:
   void UpdateSelectionDisplay();

public:
   void MakeParentRedrawScrollbars();

   // Rectangle includes track control panel, and the vertical ruler, and
   // the proper track area of all channels, and the separators between them.
   wxRect FindTrackRect( const Track * target );

protected:
   // Get the root object defining a recursive subdivision of the panel's
   // area into cells
   std::shared_ptr<TrackPanelNode> Root() override;

public:
// JKC Nov-2011: These four functions only used from within a dll
// They work around some messy problems with constructors.
   const TrackList * GetTracks() const { return mTracks.get(); }
   TrackList * GetTracks() { return mTracks.get(); }
   ViewInfo * GetViewInfo(){ return mViewInfo;}
   TrackPanelListener * GetListener(){ return mListener;}
   AdornedRulerPanel * GetRuler(){ return mRuler;}

protected:
   void DrawTracks(wxDC * dc);

public:
   // Set the object that performs catch-all event handling when the pointer
   // is not in any track or ruler or control panel.
   void SetBackgroundCell
      (const std::shared_ptr< TrackPanelCell > &pCell);
   std::shared_ptr< TrackPanelCell > GetBackgroundCell();

public:

protected:
   TrackPanelListener *mListener;

   std::shared_ptr<TrackList> mTracks;

   AdornedRulerPanel *mRuler;

   std::unique_ptr<TrackArtist> mTrackArtist;

   class AUDACITY_DLL_API AudacityTimer final : public wxTimer {
   public:
     void Notify() override{
       // (From Debian)
       //
       // Don't call parent->OnTimer(..) directly here, but instead post
       // an event. This ensures that this is a pure wxWidgets event
       // (no GDK event behind it) and that it therefore isn't processed
       // within the YieldFor(..) of the clipboard operations (workaround
       // for Debian bug #765341).
       // QueueEvent() will take ownership of the event
       parent->GetEventHandler()->QueueEvent(safenew wxTimerEvent(*this));
     }
     TrackPanel *parent;
   } mTimer;

   int mTimeCount;

   bool mRefreshBacking;


protected:

   SelectedRegion mLastDrawnSelectedRegion {};

 protected:

   std::shared_ptr<TrackPanelCell> mpBackground;

   DECLARE_EVENT_TABLE()

   void ProcessUIHandleResult
      (TrackPanelCell *pClickedTrack, TrackPanelCell *pLatestCell,
       unsigned refreshResult) override;

   void UpdateStatusMessage( const TranslatableString &status ) override;
};

// A predicate class
struct AUDACITY_DLL_API IsVisibleTrack
{
   IsVisibleTrack(AudacityProject *project);

   bool operator () (const Track *pTrack) const;

   wxRect mPanelRect;
};

//! A class with context menu over audio track.
class AUDACITY_DLL_API AudacityOverTrackContextMenu : public wxMenu
{
public:
    enum MenuItemID
    {
        kItemID_Cut = wxID_LAST + 1,
        kItemID_Copy,
        kItemID_Paste,
        kItemID_Split,
        kItemID_Mute,
        kItemID_Rename
    };
    
    AudacityOverTrackContextMenu(CommandManager* commandManager)
    {
        auto cutItemString = XO("Cut");
        auto copyItemString = XO("Copy");
        auto pasteItemString = XO("Paste");
        auto splitItemString = XO("Split clip");
        auto muteItemString = XO("Mute/unmute track");
        // TODO: When task #1258 will finished.
        // For more info see #998 task description.
        //auto renameClipItemString = XO("Rename clip");
        
        if (commandManager)
        {
            ComponentInterfaceSymbol cmdName;
            
            cmdName = ComponentInterfaceSymbol(wxT("Cut"), cutItemString);
            cutItemString = commandManager->DescribeCommandsAndShortcuts(&cmdName, 1u);
            
            cmdName = ComponentInterfaceSymbol(wxT("Copy"), copyItemString);
            copyItemString = commandManager->DescribeCommandsAndShortcuts(&cmdName, 1u);
            
            cmdName = ComponentInterfaceSymbol(wxT("Paste"), pasteItemString);
            pasteItemString = commandManager->DescribeCommandsAndShortcuts(&cmdName, 1u);
            
            cmdName = ComponentInterfaceSymbol(wxT("Split"), splitItemString);
            splitItemString = commandManager->DescribeCommandsAndShortcuts(&cmdName, 1u);
            
            cmdName = ComponentInterfaceSymbol(wxT("MuteTracks"), muteItemString);
            muteItemString = commandManager->DescribeCommandsAndShortcuts(&cmdName, 1u);
        }
        
        Append(MenuItemID::kItemID_Cut, cutItemString.Translation());
        Append(MenuItemID::kItemID_Copy, copyItemString.Translation());
        Append(MenuItemID::kItemID_Paste, pasteItemString.Translation());
        AppendSeparator();
        Append(MenuItemID::kItemID_Split, splitItemString.Translation());
        Append(MenuItemID::kItemID_Mute, muteItemString.Translation());
    }
};

//! A class with context menu outside audio track.
class AUDACITY_DLL_API AudacityOutsideTrackContextMenu : public wxMenu
{
public:
    enum MenuItemID
    {
        kItemID_AddMonoTrack = wxID_LAST + 1,
        kItemID_AddStereoTrack,
        kItemID_AddLabelTrack,
        kItemID_ExportProject,
        kItemID_SelectAll,
    };
    
    AudacityOutsideTrackContextMenu(CommandManager* commandManager)
    {
        auto addMonoTrackItemString = XO("Add mono track");
        auto addStereoTrackItemString = XO("Add stereo track");
        auto addLabelTrackItemString = XO("Add label track");
        auto exportProjectItemString = XO("Export Audio...");
        auto selectAllItemString = XO("Select All");
        
        if (commandManager)
        {
            ComponentInterfaceSymbol cmdName;
            
            cmdName = ComponentInterfaceSymbol(wxT("NewMonoTrack"), addMonoTrackItemString);
            addMonoTrackItemString = commandManager->DescribeCommandsAndShortcuts(&cmdName, 1u);
            
            cmdName = ComponentInterfaceSymbol(wxT("NewStereoTrack"), addStereoTrackItemString);
            addStereoTrackItemString = commandManager->DescribeCommandsAndShortcuts(&cmdName, 1u);
            
            cmdName = ComponentInterfaceSymbol(wxT("NewLabelTrack"), addLabelTrackItemString);
            addLabelTrackItemString = commandManager->DescribeCommandsAndShortcuts(&cmdName, 1u);
            
            cmdName = ComponentInterfaceSymbol(wxT("Export"), exportProjectItemString);
            exportProjectItemString = commandManager->DescribeCommandsAndShortcuts(&cmdName, 1u);
            
            cmdName = ComponentInterfaceSymbol(wxT("SelectAll"), selectAllItemString);
            selectAllItemString = commandManager->DescribeCommandsAndShortcuts(&cmdName, 1u);
        }
        
        Append(MenuItemID::kItemID_AddMonoTrack, addMonoTrackItemString.Translation());
        Append(MenuItemID::kItemID_AddStereoTrack, addStereoTrackItemString.Translation());
        Append(MenuItemID::kItemID_AddLabelTrack, addLabelTrackItemString.Translation());
        AppendSeparator();
        Append(MenuItemID::kItemID_ExportProject, exportProjectItemString.Translation());
        AppendSeparator();
        Append(MenuItemID::kItemID_SelectAll, selectAllItemString.Translation());
    }
};

#endif
