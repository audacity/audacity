/**********************************************************************

  Audacity: A Digital Audio Editor

  EditToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

  See EditToolBar.h for details

*******************************************************************//*!

\class EditToolBar
\brief A ToolBar that has the edit buttons on it.

  This class, which is a child of Toolbar, creates the
  window containing interfaces to commonly-used edit
  functions that are otherwise only available through
  menus. The window can be embedded within a normal project
  window, or within a ToolbarFrame that is managed by a
  global ToolBarStub called gControlToolBarStub.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform.

*//*******************************************************************/


#include "../Audacity.h"
#include "EditToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/tooltip.h>
#endif

#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Internat.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../Track.h"
#include "../UndoManager.h"
#include "../widgets/AButton.h"

#include "../Experimental.h"

IMPLEMENT_CLASS(EditToolBar, ToolBar);

const int BUTTON_WIDTH = 27;
const int SEPARATOR_WIDTH = 14;

////////////////////////////////////////////////////////////
/// Methods for EditToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( EditToolBar, ToolBar )
   EVT_COMMAND_RANGE( ETBCutID,
                      ETBCutID + ETBNumButtons - 1,
                      wxEVT_COMMAND_BUTTON_CLICKED,
                      EditToolBar::OnButton )
END_EVENT_TABLE()

//Standard contructor
EditToolBar::EditToolBar()
: ToolBar(EditBarID, _("Edit"), wxT("Edit"))
{
}

EditToolBar::~EditToolBar()
{
}

void EditToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
}

void EditToolBar::AddSeparator()
{
   AddSpacer();
}

/// This is a convenience function that allows for button creation in
/// MakeButtons() with fewer arguments
/// Very similar to code in ControlToolBar...
AButton *EditToolBar::AddButton(
   teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
   int id,
   const wxChar *label,
   bool toggle)
{
   AButton *&r = mButtons[id];

   r = ToolBar::MakeButton(this,
      bmpRecoloredUpSmall, bmpRecoloredDownSmall, bmpRecoloredHiliteSmall,
      eEnabledUp, eEnabledDown, eDisabled,
      wxWindowID(id),
      wxDefaultPosition,
      toggle,
      theTheme.ImageSize( bmpRecoloredUpSmall ));

   r->SetLabel(label);
// JKC: Unlike ControlToolBar, does not have a focus rect.  Shouldn't it?
// r->SetFocusRect( r->GetRect().Deflate( 4, 4 ) );

   Add( r, 0, wxALIGN_CENTER );

   return r;
}

void EditToolBar::Populate()
{
   MakeButtonBackgroundsSmall();

   /* Buttons */
   AddButton(bmpCut, bmpCut, bmpCutDisabled, ETBCutID,
      _("Cut selection"));
   AddButton(bmpCopy, bmpCopy, bmpCopyDisabled, ETBCopyID,
      _("Copy selection"));
   AddButton(bmpPaste, bmpPaste, bmpPasteDisabled, ETBPasteID,
      _("Paste"));
   AddButton(bmpTrim, bmpTrim, bmpTrimDisabled, ETBTrimID,
      _("Trim audio outside selection"));
   AddButton(bmpSilence, bmpSilence, bmpSilenceDisabled, ETBSilenceID,
      _("Silence audio selection"));

   AddSeparator();

   AddButton(bmpUndo, bmpUndo, bmpUndoDisabled, ETBUndoID,
      _("Undo"));
   AddButton(bmpRedo, bmpRedo, bmpRedoDisabled, ETBRedoID,
      _("Redo"));

   AddSeparator();

#ifdef EXPERIMENTAL_SYNC_LOCK
   AddButton(bmpSyncLockTracksUp, bmpSyncLockTracksDown, bmpSyncLockTracksDisabled, ETBSyncLockID,
               _("Sync-Lock Tracks"), true);

   AddSeparator();
#endif

   AddButton(bmpZoomIn, bmpZoomIn, bmpZoomInDisabled, ETBZoomInID,
      _("Zoom In"));
   AddButton(bmpZoomOut, bmpZoomOut, bmpZoomOutDisabled, ETBZoomOutID,
      _("Zoom Out"));

   AddButton(bmpZoomSel, bmpZoomSel, bmpZoomSelDisabled, ETBZoomSelID,
      _("Fit selection in window"));
   AddButton(bmpZoomFit, bmpZoomFit, bmpZoomFitDisabled, ETBZoomFitID,
      _("Fit project in window"));

   mButtons[ETBZoomInID]->SetEnabled(false);
   mButtons[ETBZoomOutID]->SetEnabled(false);

   mButtons[ETBZoomSelID]->SetEnabled(false);
   mButtons[ETBZoomFitID]->SetEnabled(false);
   mButtons[ETBPasteID]->SetEnabled(false);

#ifdef EXPERIMENTAL_SYNC_LOCK
   mButtons[ETBSyncLockID]->PushDown();
#endif

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   AddSeparator();
   AddButton(bmpEditEffects, bmpEditEffects, bmpEditEffects, ETBEffectsID,
      _("Show Effects Rack"), true);
#endif

   RegenerateTooltips();
}

void EditToolBar::UpdatePrefs()
{
   RegenerateTooltips();

   // Set label to pull in language change
   SetLabel(_("Edit"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void EditToolBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   static const struct Entry {
      int tool;
      wxString commandName;
      wxString untranslatedLabel;
   } table[] = {
      { ETBCutID,      wxT("Cut"),         XO("Cut")  },
      { ETBCopyID,     wxT("Copy"),        XO("Copy")  },
      { ETBPasteID,    wxT("Paste"),       XO("Paste")  },
      { ETBTrimID,     wxT("Trim"),        XO("Trim Audio")  },
      { ETBSilenceID,  wxT("Silence"),     XO("Silence Audio")  },
      { ETBUndoID,     wxT("Undo"),        XO("Undo")  },
      { ETBRedoID,     wxT("Redo"),        XO("Redo")  },

#ifdef EXPERIMENTAL_SYNC_LOCK
      { ETBSyncLockID, wxT("SyncLock"),    XO("Sync-Lock Tracks")  },
#endif

      { ETBZoomInID,   wxT("ZoomIn"),      XO("Zoom In")  },
      { ETBZoomOutID,  wxT("ZoomOut"),     XO("Zoom Out")  },
      { ETBZoomSelID,  wxT("ZoomSel"),     XO("Fit Selection")  },
      { ETBZoomFitID,  wxT("FitInWindow"), XO("Fit Project")  },

#if defined(EXPERIMENTAL_EFFECTS_RACK)
      { ETBEffectsID,  wxT(""),            XO("Open Effects Rack")  },
#endif
   };

   std::vector<wxString> commands;
   for (const auto &entry : table) {
      commands.clear();
      commands.push_back(wxGetTranslation(entry.untranslatedLabel));
      commands.push_back(entry.commandName);
      ToolBar::SetButtonToolTip(*mButtons[entry.tool], commands);
   }
#endif
}

void EditToolBar::OnButton(wxCommandEvent &event)
{
   AudacityProject *p = GetActiveProject();
   if (!p) return;

   bool busy = gAudioIO->IsBusy();
   int id = event.GetId();

   switch (id) {
      case ETBCutID:
         if (!busy) p->OnCut();
         break;
      case ETBCopyID:
         if (!busy) p->OnCopy();
         break;
      case ETBPasteID:
         if (!busy) p->OnPaste();
         break;
      case ETBTrimID:
         if (!busy) p->OnTrim();
         break;
      case ETBSilenceID:
         if (!busy) p->OnSilence();
         break;
      case ETBUndoID:
         if (!busy) p->OnUndo();
         break;
      case ETBRedoID:
         if (!busy) p->OnRedo();
         break;
#ifdef EXPERIMENTAL_SYNC_LOCK
      case ETBSyncLockID:
         p->OnSyncLock();
         return;//avoiding the call to SetButton()
#endif
      case ETBZoomInID:
         p->OnZoomIn();
         break;
      case ETBZoomOutID:
         p->OnZoomOut();
         break;

#if 0 // Disabled for version 1.2.0 since it doesn't work quite right...
      case ETBZoomToggleID:
         p->OnZoomToggle();
         break;
#endif

      case ETBZoomSelID:
         p->OnZoomSel();
         break;
      case ETBZoomFitID:
         p->OnZoomFit();
         break;
#if defined(EXPERIMENTAL_EFFECTS_RACK)
      case ETBEffectsID:
         EffectManager::Get().ShowRack();
         break;
#endif
   }

   SetButton(false, mButtons[id]);
}

void EditToolBar::EnableDisableButtons()
{
   AudacityProject *p = GetActiveProject();
   if (!p) return;

   // Is anything selected?
   bool selection = false;
   TrackListIterator iter(p->GetTracks());
   for (Track *t = iter.First(); t; t = iter.Next())
      if (t->GetSelected()) {
         selection = true;
         break;
      }
   selection &= (p->GetSel0() < p->GetSel1());

   mButtons[ETBCutID]->SetEnabled(selection);
   mButtons[ETBCopyID]->SetEnabled(selection);
   mButtons[ETBTrimID]->SetEnabled(selection);
   mButtons[ETBSilenceID]->SetEnabled(selection);

   mButtons[ETBUndoID]->SetEnabled(p->GetUndoManager()->UndoAvailable());
   mButtons[ETBRedoID]->SetEnabled(p->GetUndoManager()->RedoAvailable());

   bool tracks = (!p->GetTracks()->IsEmpty());

   mButtons[ETBZoomInID]->SetEnabled(tracks && (p->ZoomInAvailable()));
   mButtons[ETBZoomOutID]->SetEnabled(tracks && (p->ZoomOutAvailable()) );

   #if 0 // Disabled for version 1.2.0 since it doesn't work quite right...
   mButtons[ETBZoomToggleID]->SetEnabled(tracks);
   #endif

   mButtons[ETBZoomSelID]->SetEnabled(selection);
   mButtons[ETBZoomFitID]->SetEnabled(tracks);

   mButtons[ETBPasteID]->SetEnabled(p->Clipboard());

#ifdef EXPERIMENTAL_SYNC_LOCK
   bool bSyncLockTracks;
   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &bSyncLockTracks, false);

   if (bSyncLockTracks)
      mButtons[ETBSyncLockID]->PushDown();
   else
      mButtons[ETBSyncLockID]->PopUp();
#endif
}
