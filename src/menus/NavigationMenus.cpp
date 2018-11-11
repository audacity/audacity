#include "../Audacity.h"

#include "../Menus.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../TrackPanel.h"
#include "../TrackPanelAx.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../toolbars/ToolManager.h"
#include "../widgets/AButton.h"
#include "../widgets/ASlider.h"
#include "../widgets/Meter.h"

// private helper classes and functions
namespace {

void NextOrPrevFrame(AudacityProject &project, bool forward)
{
   // Focus won't take in a dock unless at least one descendant window
   // accepts focus.  Tell controls to take focus for the duration of this
   // function, only.  Outside of this, they won't steal the focus when
   // clicked.
   auto temp1 = AButton::TemporarilyAllowFocus();
   auto temp2 = ASlider::TemporarilyAllowFocus();
   auto temp3 = MeterPanel::TemporarilyAllowFocus();

   auto toolManager = project.GetToolManager();
   auto botDock = toolManager->GetBotDock();


   // Define the set of windows we rotate among.
   static const unsigned rotationSize = 3u;

   wxWindow *const begin [rotationSize] = {
      project.GetTopPanel(),
      project.GetTrackPanel(),
      botDock,
   };

   const auto end = begin + rotationSize;

   // helper functions
   auto IndexOf = [&](wxWindow *pWindow) {
      return std::find(begin, end, pWindow) - begin;
   };

   auto FindAncestor = [&]() {
      wxWindow *pWindow = wxWindow::FindFocus();
      unsigned index = rotationSize;
      while ( pWindow &&
              (rotationSize == (index = IndexOf(pWindow) ) ) )
         pWindow = pWindow->GetParent();
      return index;
   };

   const auto idx = FindAncestor();
   if (idx == rotationSize)
      return;

   auto idx2 = idx;
   auto increment = (forward ? 1 : rotationSize - 1);

   while( idx != (idx2 = (idx2 + increment) % rotationSize) ) {
      wxWindow *toFocus = begin[idx2];
      bool bIsAnEmptyDock=false;
      if( idx2 != 1 )
         bIsAnEmptyDock = ((idx2==0) ? toolManager->GetTopDock() : botDock)->
         GetChildren().GetCount() < 1;

      // Skip docks that are empty (Bug 1564).
      if( !bIsAnEmptyDock ){
         toFocus->SetFocus();
         if ( FindAncestor() == idx2 )
            // The focus took!
            break;
      }
   }
}

/// \todo Merge related methods, OnPrevTrack and OnNextTrack.
void DoPrevTrack(
   AudacityProject &project, bool shift, bool circularTrackNavigation )
{
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();
   auto &selectionState = project.GetSelectionState();

   Track* t = trackPanel->GetFocusedTrack();
   if( t == NULL )   // if there isn't one, focus on last
   {
      t = *tracks->Any().rbegin();
      trackPanel->SetFocusedTrack( t );
      trackPanel->EnsureVisible( t );
      project.ModifyState(false);
      return;
   }

   Track* p = NULL;
   bool tSelected = false;
   bool pSelected = false;
   if( shift )
   {
      p = * -- tracks->FindLeader( t ); // Get previous track
      if( p == NULL )   // On first track
      {
         // JKC: wxBell() is probably for accessibility, so a blind
         // user knows they were at the top track.
         wxBell();
         if( circularTrackNavigation )
            p = *tracks->Any().rbegin();
         else
         {
            trackPanel->EnsureVisible( t );
            return;
         }
      }
      tSelected = t->GetSelected();
      if (p)
         pSelected = p->GetSelected();
      if( tSelected && pSelected )
      {
         selectionState.SelectTrack
            ( *t, false, false );
         trackPanel->SetFocusedTrack( p );   // move focus to next track up
         trackPanel->EnsureVisible( p );
         project.ModifyState(false);
         return;
      }
      if( tSelected && !pSelected )
      {
         selectionState.SelectTrack
            ( *p, true, false );
         trackPanel->SetFocusedTrack( p );   // move focus to next track up
         trackPanel->EnsureVisible( p );
         project.ModifyState(false);
         return;
      }
      if( !tSelected && pSelected )
      {
         selectionState.SelectTrack
            ( *p, false, false );
         trackPanel->SetFocusedTrack( p );   // move focus to next track up
         trackPanel->EnsureVisible( p );
         project.ModifyState(false);
         return;
      }
      if( !tSelected && !pSelected )
      {
         selectionState.SelectTrack
            ( *t, true, false );
         trackPanel->SetFocusedTrack( p );   // move focus to next track up
         trackPanel->EnsureVisible( p );
          project.ModifyState(false);
         return;
      }
   }
   else
   {
      p = * -- tracks->FindLeader( t ); // Get previous track
      if( p == NULL )   // On first track so stay there?
      {
         wxBell();
         if( circularTrackNavigation )
         {
            auto range = tracks->Leaders();
            p = * range.rbegin(); // null if range is empty
            trackPanel->SetFocusedTrack( p );   // Wrap to the last track
            trackPanel->EnsureVisible( p );
             project.ModifyState(false);
            return;
         }
         else
         {
            trackPanel->EnsureVisible( t );
            return;
         }
      }
      else
      {
         trackPanel->SetFocusedTrack( p );   // move focus to next track up
         trackPanel->EnsureVisible( p );
         project.ModifyState(false);
         return;
      }
   }
}

/// The following method moves to the next track,
/// selecting and unselecting depending if you are on the start of a
/// block or not.
void DoNextTrack(
   AudacityProject &project, bool shift, bool circularTrackNavigation )
{
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();
   auto &selectionState = project.GetSelectionState();

   auto t = trackPanel->GetFocusedTrack();   // Get currently focused track
   if( t == NULL )   // if there isn't one, focus on first
   {
      t = *tracks->Any().begin();
      trackPanel->SetFocusedTrack( t );
      trackPanel->EnsureVisible( t );
      project.ModifyState(false);
      return;
   }

   if( shift )
   {
      auto n = * ++ tracks->FindLeader( t ); // Get next track
      if( n == NULL )   // On last track so stay there
      {
         wxBell();
         if( circularTrackNavigation )
            n = *tracks->Any().begin();
         else
         {
            trackPanel->EnsureVisible( t );
            return;
         }
      }
      auto tSelected = t->GetSelected();
      auto nSelected = n->GetSelected();
      if( tSelected && nSelected )
      {
         selectionState.SelectTrack
            ( *t, false, false );
         trackPanel->SetFocusedTrack( n );   // move focus to next track down
         trackPanel->EnsureVisible( n );
         project.ModifyState(false);
         return;
      }
      if( tSelected && !nSelected )
      {
         selectionState.SelectTrack
            ( *n, true, false );
         trackPanel->SetFocusedTrack( n );   // move focus to next track down
         trackPanel->EnsureVisible( n );
         project.ModifyState(false);
         return;
      }
      if( !tSelected && nSelected )
      {
         selectionState.SelectTrack
            ( *n, false, false );
         trackPanel->SetFocusedTrack( n );   // move focus to next track down
         trackPanel->EnsureVisible( n );
         project.ModifyState(false);
         return;
      }
      if( !tSelected && !nSelected )
      {
         selectionState.SelectTrack
            ( *t, true, false );
         trackPanel->SetFocusedTrack( n );   // move focus to next track down
         trackPanel->EnsureVisible( n );
         project.ModifyState(false);
         return;
      }
   }
   else
   {
      auto n = * ++ tracks->FindLeader( t ); // Get next track
      if( n == NULL )   // On last track so stay there
      {
         wxBell();
         if( circularTrackNavigation )
         {
            n = *tracks->Any().begin();
            trackPanel->SetFocusedTrack( n );   // Wrap to the first track
            trackPanel->EnsureVisible( n );
            project.ModifyState(false);
            return;
         }
         else
         {
            trackPanel->EnsureVisible( t );
            return;
         }
      }
      else
      {
         trackPanel->SetFocusedTrack( n );   // move focus to next track down
         trackPanel->EnsureVisible( n );
         project.ModifyState(false);
         return;
      }
   }
}

}

/// Namespace for functions for project navigation menu (part of Extra menu)
namespace NavigationActions {

// exported helper functions
// none

// Menu handler functions

struct Handler
   : CommandHandlerObject // MUST be the first base class!
   , PrefsListener
{

void OnPrevWindow(const CommandContext &context)
{
   auto &project = context.project;
   auto isEnabled = project.IsEnabled();

   wxWindow *w = wxGetTopLevelParent(wxWindow::FindFocus());
   const auto & list = project.GetChildren();
   auto iter = list.rbegin(), end = list.rend();

   // If the project window has the current focus, start the search with the
   // last child
   if (w == &project)
   {
   }
   // Otherwise start the search with the current window's previous sibling
   else
   {
      while (iter != end && *iter != w)
         ++iter;
      if (iter != end)
         ++iter;
   }

   // Search for the previous toplevel window
   for (; iter != end; ++iter)
   {
      // If it's a toplevel and is visible (we have come hidden windows), then
      // we're done
      w = *iter;
      if (w->IsTopLevel() && w->IsShown() && isEnabled)
      {
         break;
      }
   }

   // Ran out of siblings, so make the current project active
   if ((iter == end) && isEnabled)
   {
      w = &project;
   }

   // And make sure it's on top (only for floating windows...project window will
   // not raise)
   // (Really only works on Windows)
   w->Raise();


#if defined(__WXMAC__) || defined(__WXGTK__)
   // bug 868
   // Simulate a TAB key press before continuing, else the cycle of
   // navigation among top level windows stops because the keystrokes don't
   // go to the CommandManager.
   if (dynamic_cast<wxDialog*>(w)) {
      w->SetFocus();
   }
#endif
}

void OnNextWindow(const CommandContext &context)
{
   auto &project = context.project;
   auto isEnabled = project.IsEnabled();

   wxWindow *w = wxGetTopLevelParent(wxWindow::FindFocus());
   const auto & list = project.GetChildren();
   auto iter = list.begin(), end = list.end();

   // If the project window has the current focus, start the search with the
   // first child
   if (w == &project)
   {
   }
   // Otherwise start the search with the current window's next sibling
   else
   {
      // Find the window in this projects children.  If the window with the
      // focus isn't a child of this project (like when a dialog is created
      // without specifying a parent), then we'll get back NULL here.
      while (iter != end && *iter != w)
         ++iter;
      if (iter != end)
         ++iter;
   }

   // Search for the next toplevel window
   for (; iter != end; ++iter)
   {
      // If it's a toplevel, visible (we have hidden windows) and is enabled,
      // then we're done.  The IsEnabled() prevents us from moving away from
      // a modal dialog because all other toplevel windows will be disabled.
      w = *iter;
      if (w->IsTopLevel() && w->IsShown() && w->IsEnabled())
      {
         break;
      }
   }

   // Ran out of siblings, so make the current project active
   if ((iter == end) && isEnabled)
   {
      w = &project;
   }

   // And make sure it's on top (only for floating windows...project window will
   // not raise)
   // (Really only works on Windows)
   w->Raise();


#if defined(__WXMAC__) || defined(__WXGTK__)
   // bug 868
   // Simulate a TAB key press before continuing, else the cycle of
   // navigation among top level windows stops because the keystrokes don't
   // go to the CommandManager.
   if (dynamic_cast<wxDialog*>(w)) {
      w->SetFocus();
   }
#endif
}

void OnPrevFrame(const CommandContext &context)
{
   auto &project = context.project;
   NextOrPrevFrame(project, false);
}

void OnNextFrame(const CommandContext &context)
{
   auto &project = context.project;
   NextOrPrevFrame(project, true);
}

// Handler state:
bool mCircularTrackNavigation{};

void OnCursorUp(const CommandContext &context)
{
   auto &project = context.project;
   DoPrevTrack( project, false, mCircularTrackNavigation );
}

void OnCursorDown(const CommandContext &context)
{
   auto &project = context.project;
   DoNextTrack( project, false, mCircularTrackNavigation );
}

void OnFirstTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   Track *t = trackPanel->GetFocusedTrack();
   if (!t)
      return;

   auto f = *tracks->Any().begin();
   if (t != f)
   {
      trackPanel->SetFocusedTrack(f);
      project.ModifyState(false);
   }
   trackPanel->EnsureVisible(f);
}

void OnLastTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   Track *t = trackPanel->GetFocusedTrack();
   if (!t)
      return;

   auto l = *tracks->Any().rbegin();
   if (t != l)
   {
      trackPanel->SetFocusedTrack(l);
      project.ModifyState(false);
   }
   trackPanel->EnsureVisible(l);
}

void OnShiftUp(const CommandContext &context)
{
   auto &project = context.project;
   DoPrevTrack( project, true, mCircularTrackNavigation );
}

void OnShiftDown(const CommandContext &context)
{
   auto &project = context.project;
   DoNextTrack( project, true, mCircularTrackNavigation );
}

void OnToggle(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto &selectionState = project.GetSelectionState();

   Track *t;

   t = trackPanel->GetFocusedTrack();   // Get currently focused track
   if (!t)
      return;

   selectionState.SelectTrack
      ( *t, !t->GetSelected(), true );
   trackPanel->EnsureVisible( t );
   project.ModifyState(false);

   trackPanel->GetAx().Updated();

   return;
}

void UpdatePrefs() override
{
   mCircularTrackNavigation =
      gPrefs->ReadBool(wxT("/GUI/CircularTrackNavigation"), false);
}
Handler()
{
   UpdatePrefs();
}

}; // struct Handler

} // namespace

// Handler is stateful.  Needs a factory registered with
// AudacityProject.
static const AudacityProject::RegisteredAttachedObjectFactory factory{ []{
   return std::make_unique< NavigationActions::Handler >();
} };
static CommandHandlerObject &findCommandHandler(AudacityProject &project) {
   return static_cast<NavigationActions::Handler&>(
      project.GetAttachedObject(factory));
};

// Menu definitions

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& NavigationActions::Handler :: X)
#define XXO(X) _(X), wxString{X}.Contains("...")

MenuTable::BaseItemPtr ExtraGlobalCommands( AudacityProject & )
{
   // Ceci n'est pas un menu
   using namespace MenuTable;
   using Options = CommandManager::Options;
   return Items(
      Command( wxT("PrevWindow"), XXO("Move Backward Through Active Windows"),
         FN(OnPrevWindow), AlwaysEnabledFlag,
         Options{ wxT("Alt+Shift+F6") }.IsGlobal() ),
      Command( wxT("NextWindow"), XXO("Move Forward Through Active Windows"),
         FN(OnNextWindow), AlwaysEnabledFlag,
         Options{ wxT("Alt+F6") }.IsGlobal() )
   );
}

MenuTable::BaseItemPtr ExtraFocusMenu( AudacityProject & )
{
   using namespace MenuTable;
   constexpr auto FocusedTracksFlags = TracksExistFlag | TrackPanelHasFocus;

   return Menu( _("F&ocus"),
      Command( wxT("PrevFrame"),
         XXO("Move &Backward from Toolbars to Tracks"), FN(OnPrevFrame),
         AlwaysEnabledFlag, wxT("Ctrl+Shift+F6") ),
      Command( wxT("NextFrame"),
         XXO("Move F&orward from Toolbars to Tracks"), FN(OnNextFrame),
         AlwaysEnabledFlag, wxT("Ctrl+F6") ),
      Command( wxT("PrevTrack"), XXO("Move Focus to &Previous Track"),
         FN(OnCursorUp), FocusedTracksFlags, wxT("Up") ),
      Command( wxT("NextTrack"), XXO("Move Focus to &Next Track"),
         FN(OnCursorDown), FocusedTracksFlags, wxT("Down") ),
      Command( wxT("FirstTrack"), XXO("Move Focus to &First Track"),
         FN(OnFirstTrack), FocusedTracksFlags, wxT("Ctrl+Home") ),
      Command( wxT("LastTrack"), XXO("Move Focus to &Last Track"),
         FN(OnLastTrack), FocusedTracksFlags, wxT("Ctrl+End") ),
      Command( wxT("ShiftUp"), XXO("Move Focus to P&revious and Select"),
         FN(OnShiftUp), FocusedTracksFlags, wxT("Shift+Up") ),
      Command( wxT("ShiftDown"), XXO("Move Focus to N&ext and Select"),
         FN(OnShiftDown), FocusedTracksFlags, wxT("Shift+Down") ),
      Command( wxT("Toggle"), XXO("&Toggle Focused Track"), FN(OnToggle),
         FocusedTracksFlags, wxT("Return") ),
      Command( wxT("ToggleAlt"), XXO("Toggle Focuse&d Track"), FN(OnToggle),
         FocusedTracksFlags, wxT("NUMPAD_ENTER") )
   );
}

#undef XXO
#undef FN
