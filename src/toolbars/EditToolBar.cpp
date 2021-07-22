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
  window, or within a ToolBarFrame.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform.

*//*******************************************************************/



#include "EditToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/tooltip.h>
#endif

#include "../AllThemeResources.h"
#include "../BatchCommands.h"
#include "../ImageManipulation.h"
#include "../Menus.h"
#include "Prefs.h"
#include "../Project.h"
#include "../UndoManager.h"
#include "../widgets/AButton.h"

#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"

IMPLEMENT_CLASS(EditToolBar, ToolBar);

const int BUTTON_WIDTH = 27;
const int SEPARATOR_WIDTH = 14;

////////////////////////////////////////////////////////////
/// Methods for EditToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( EditToolBar, ToolBar )
   EVT_COMMAND_RANGE( ETBCutID+first_ETB_ID,
                      ETBCutID+first_ETB_ID + ETBNumButtons - 1,
                      wxEVT_COMMAND_BUTTON_CLICKED,
                      EditToolBar::OnButton )
END_EVENT_TABLE()

//Standard constructor
EditToolBar::EditToolBar( AudacityProject &project )
: ToolBar(project, EditBarID, XO("Edit"), wxT("Edit"))
{
}

EditToolBar::~EditToolBar()
{
}

void EditToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}

void EditToolBar::AddSeparator()
{
   AddSpacer();
}

/// This is a convenience function that allows for button creation in
/// MakeButtons() with fewer arguments
/// Very similar to code in ControlToolBar...
AButton *EditToolBar::AddButton(
   EditToolBar *pBar,
   teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
   int id,
   const TranslatableString &label,
   bool toggle)
{
   AButton *&r = pBar->mButtons[id];

   r = ToolBar::MakeButton(pBar,
      bmpRecoloredUpSmall, bmpRecoloredDownSmall, bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall,
      eEnabledUp, eEnabledDown, eDisabled,
      wxWindowID(id+first_ETB_ID),
      wxDefaultPosition,
      toggle,
      theTheme.ImageSize( bmpRecoloredUpSmall ));

   r->SetLabel(label);
// JKC: Unlike ControlToolBar, does not have a focus rect.  Shouldn't it?
// r->SetFocusRect( r->GetRect().Deflate( 4, 4 ) );

   pBar->Add( r, 0, wxALIGN_CENTER );

   return r;
}

void EditToolBar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
   MakeButtonBackgroundsSmall();

   /* Buttons */
   // Tooltips slightly more verbose than the menu entries are.
   AddButton(this, bmpCut, bmpCut, bmpCutDisabled, ETBCutID,
      XO("Cut selection"));
   AddButton(this, bmpCopy, bmpCopy, bmpCopyDisabled, ETBCopyID,
      XO("Copy selection"));
   AddButton(this, bmpPaste, bmpPaste, bmpPasteDisabled, ETBPasteID,
      XO("Paste"));
   AddButton(this, bmpTrim, bmpTrim, bmpTrimDisabled, ETBTrimID,
      XO("Trim audio outside selection"));
   AddButton(this, bmpSilence, bmpSilence, bmpSilenceDisabled, ETBSilenceID,
      XO("Silence audio selection"));

   AddSeparator();

   AddButton(this, bmpUndo, bmpUndo, bmpUndoDisabled, ETBUndoID,
      XO("Undo"));
   AddButton(this, bmpRedo, bmpRedo, bmpRedoDisabled, ETBRedoID,
      XO("Redo"));

   AddSeparator();

#ifdef OPTION_SYNC_LOCK_BUTTON
   AddButton(this, bmpSyncLockTracksUp, bmpSyncLockTracksDown, bmpSyncLockTracksUp, ETBSyncLockID,
               XO("Sync-Lock Tracks"), true);

   AddSeparator();
#endif

   // Tooltips match menu entries.
   // We previously had longer tooltips which were not more clear.
   AddButton(this, bmpZoomIn, bmpZoomIn, bmpZoomInDisabled, ETBZoomInID,
      XO("Zoom In"));
   AddButton(this, bmpZoomOut, bmpZoomOut, bmpZoomOutDisabled, ETBZoomOutID,
      XO("Zoom Out"));
   AddButton(this, bmpZoomSel, bmpZoomSel, bmpZoomSelDisabled, ETBZoomSelID,
      XO("Zoom to Selection"));
   AddButton(this, bmpZoomFit, bmpZoomFit, bmpZoomFitDisabled, ETBZoomFitID,
      XO("Fit to Width"));

#ifdef EXPERIMENTAL_ZOOM_TOGGLE_BUTTON
   AddButton(this, bmpZoomToggle, bmpZoomToggle, bmpZoomToggleDisabled, ETBZoomToggleID,
      XO("Zoom Toggle"));
#endif



   mButtons[ETBZoomInID]->SetEnabled(false);
   mButtons[ETBZoomOutID]->SetEnabled(false);
#ifdef EXPERIMENTAL_ZOOM_TOGGLE_BUTTON
   mButtons[ETBZoomToggleID]->SetEnabled(false);
#endif

   mButtons[ETBZoomSelID]->SetEnabled(false);
   mButtons[ETBZoomFitID]->SetEnabled(false);
   mButtons[ETBPasteID]->SetEnabled(false);

#ifdef OPTION_SYNC_LOCK_BUTTON
   mButtons[ETBSyncLockID]->PushDown();
#endif

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   AddSeparator();
   AddButton(this, bmpEditEffects, bmpEditEffects, bmpEditEffects, ETBEffectsID,
      XO("Show Effects Rack"), true);
#endif

   RegenerateTooltips();
}

void EditToolBar::UpdatePrefs()
{
   RegenerateTooltips();

   // Set label to pull in language change
   SetLabel(XO("Edit"));

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
   CommandID commandName;
   TranslatableString untranslatedLabel;
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
#ifdef EXPERIMENTAL_ZOOM_TOGGLE_BUTTON
   { ETBZoomToggleID,   wxT("ZoomToggle"),      XO("Zoom Toggle")  },
#endif 
   { ETBZoomSelID,  wxT("ZoomSel"),     XO("Fit selection to width")  },
   { ETBZoomFitID,  wxT("FitInWindow"), XO("Fit project to width")  },

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   { ETBEffectsID,  wxT("ShowEffectsRack"), XO("Open Effects Rack")  },
#endif
};


void EditToolBar::ForAllButtons(int Action)
{
   AudacityProject *p;
   CommandManager* cm = nullptr;

   if( Action & ETBActEnableDisable ){
      p = &mProject;
      cm = &CommandManager::Get( *p );
#ifdef OPTION_SYNC_LOCK_BUTTON
      bool bSyncLockTracks;
      gPrefs->Read(wxT("/GUI/SyncLockTracks"), &bSyncLockTracks, false);

      if (bSyncLockTracks)
         mButtons[ETBSyncLockID]->PushDown();
      else
         mButtons[ETBSyncLockID]->PopUp();
#endif
   }


   for (const auto &entry : EditToolbarButtonList) {
#if wxUSE_TOOLTIPS
      if( Action & ETBActTooltips ){
         ComponentInterfaceSymbol command{
            entry.commandName, entry.untranslatedLabel };
         ToolBar::SetButtonToolTip( mProject,
            *mButtons[entry.tool], &command, 1u );
      }
#endif
      if (cm) {
         mButtons[entry.tool]->SetEnabled(cm->GetEnabled(entry.commandName));
      }
   }
}

void EditToolBar::OnButton(wxCommandEvent &event)
{
   int id = event.GetId()-first_ETB_ID;
   // Be sure the pop-up happens even if there are exceptions, except for buttons which toggle.
   auto cleanup = finally( [&] { mButtons[id]->InteractionOver();});

   AudacityProject *p = &mProject;
   auto &cm = CommandManager::Get( *p );

   auto flags = MenuManager::Get(*p).GetUpdateFlags();
   const CommandContext context( *p );
   MacroCommands::HandleTextualCommand( cm,
      EditToolbarButtonList[id].commandName, context, flags, false);

#if defined(__WXMAC__)
   // Bug 2402
   // LLL: It seems that on the Mac the IDLE events are processed
   //      differently than on Windows/GTK and the AdornedRulerPanel's
   //      OnPaint() method gets called sooner that expected. This is
   //      evident when zooming from this toolbar only. When zooming from
   //      the Menu or from keyboard ommand, the zooming works correctly.
   wxTheApp->ProcessIdle();
#endif
}

static RegisteredToolbarFactory factory{ EditBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew EditToolBar{ project } }; }
};

#include "ToolManager.h"

namespace {
AttachedToolBarMenuItem sAttachment{
   /* i18n-hint: Clicking this menu item shows the toolbar for editing */
   EditBarID, wxT("ShowEditTB"), XXO("&Edit Toolbar")
};
}

