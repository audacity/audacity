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
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
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

#ifdef OPTION_SYNC_LOCK_BUTTON
   AddButton(bmpSyncLockTracksUp, bmpSyncLockTracksDown, bmpSyncLockTracksUp, ETBSyncLockID,
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

#ifdef OPTION_SYNC_LOCK_BUTTON
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
   ForAllButtons( ETBActTooltips );
}

void EditToolBar::EnableDisableButtons()
{
   ForAllButtons( ETBActEnableDisable );
}


static const struct Entry {
   int tool;
   wxString commandName;
   wxString untranslatedLabel;
} EditToolbarButtonList[] = {
   { ETBCutID,      wxT("Cut"),         XO("Cut")  },
   { ETBCopyID,     wxT("Copy"),        XO("Copy")  },
   { ETBPasteID,    wxT("Paste"),       XO("Paste")  },
   { ETBTrimID,     wxT("Trim"),        XO("Trim audio outside selection")  },
   { ETBSilenceID,  wxT("Silence"),     XO("Silence audio selection")  },
   { ETBUndoID,     wxT("Undo"),        XO("Undo")  },
   { ETBRedoID,     wxT("Redo"),        XO("Redo")  },

#ifdef OPTION_SYNC_LOCK_BUTTON
   { ETBSyncLockID, wxT("SyncLock"),    XO("Sync-Lock Tracks")  },
#endif

   { ETBZoomInID,   wxT("ZoomIn"),      XO("Zoom In")  },
   { ETBZoomOutID,  wxT("ZoomOut"),     XO("Zoom Out")  },
   { ETBZoomSelID,  wxT("ZoomSel"),     XO("Fit selection in window")  },
   { ETBZoomFitID,  wxT("FitInWindow"), XO("Fit project in window")  },

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   { ETBEffectsID,  wxT(""),            XO("Open Effects Rack")  },
#endif
};


void EditToolBar::ForAllButtons(int Action)
{
   AudacityProject *p;
   CommandManager* cm = nullptr;

   if( Action & ETBActEnableDisable ){
      p = GetActiveProject();
      if (!p) return;
      cm = p->GetCommandManager();
      if (!cm) return;
#ifdef OPTION_SYNC_LOCK_BUTTON
      bool bSyncLockTracks;
      gPrefs->Read(wxT("/GUI/SyncLockTracks"), &bSyncLockTracks, false);

      if (bSyncLockTracks)
         mButtons[ETBSyncLockID]->PushDown();
      else
         mButtons[ETBSyncLockID]->PopUp();
#endif
   }


   std::vector<wxString> commands;
   for (const auto &entry : EditToolbarButtonList) {
#if wxUSE_TOOLTIPS
      if( Action & ETBActTooltips ){
         commands.clear();
         commands.push_back(wxGetTranslation(entry.untranslatedLabel));
         commands.push_back(entry.commandName);
         ToolBar::SetButtonToolTip(*mButtons[entry.tool], commands);
      }
#endif
      if (cm) {
         mButtons[entry.tool]->SetEnabled(cm->GetEnabled(entry.commandName));
      }
   }
}

void EditToolBar::OnButton(wxCommandEvent &event)
{
   int id = event.GetId();
   // Be sure the pop-up happens even if there are exceptions, except for buttons which toggle.
   auto cleanup = finally( [&] { mButtons[id]->InteractionOver();});

   AudacityProject *p = GetActiveProject();
   if (!p) return;
   CommandManager* cm = p->GetCommandManager();
   if (!cm) return;

   auto flags = p->GetUpdateFlags();
   cm->HandleTextualCommand(EditToolbarButtonList[id].commandName, flags, NoFlagsSpecifed);
}


