/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dominic Mazzoni
   Dan Horgan
   James Crook

**********************************************************************/

#ifndef __SCREENSHOT_COMMAND__
#define __SCREENSHOT_COMMAND__

#include "Command.h"
#include "../commands/AudacityCommand.h"

#include <wx/colour.h>
class wxWindow;
class wxTopLevelWindow;
class wxCommandEvent;
class wxRect;
class ToolManager;
class CommandOutputTargets;
class TrackPanel;
class AdornedRulerPanel;
class AudacityProject;
class CommandContext;

#define SCREENSHOT_PLUGIN_SYMBOL XO("Screenshot")

class ScreenshotCommand : public AudacityCommand
{
public:
   ScreenshotCommand(){ mbBringToTop=true;};
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return SCREENSHOT_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Takes screenshots.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Help_Menu:_Tools#screenshot_tools");};

private:
   wxString mWhat;
   wxString mBack;
   wxString mPath;
   bool mbBringToTop;
   bool bHasBackground;
   bool bHasBringToTop;
   friend class ScreenshotCommand;
   friend class ScreenFrame;

public:
   bool Apply(const CommandContext & context) override;
   void GetDerivedParams();

private:
   // May need to ignore the screenshot dialog
   wxWindow *mIgnore;

   bool mBackground;
   wxColour mBackColor;
   wxString mDirToWriteTo;

   wxString mFilePath;
   wxString mFileName;
   wxString mCaptureMode;

   wxString MakeFileName(const wxString &path, const wxString &basename);

   wxRect GetBackgroundRect();

   bool CaptureToolbar(const CommandContext & Context, ToolManager *man, int type, const wxString &name);
   bool CaptureDock(const CommandContext & Context, wxWindow *win, const wxString &fileName);
   void CaptureMenus(const CommandContext & Context, wxMenuBar*pBar, const wxString &fileName);
   void CaptureEffects(const CommandContext & Context, AudacityProject * pProject, const wxString &fileName );
   void CapturePreferences(const CommandContext & Context, AudacityProject * pProject, const wxString &fileName );
   bool Capture(
      const CommandContext & Context,
      const wxString &basename,
         wxWindow *window, wxRect rect, 
         bool bg = false);
   wxRect GetWindowRect(wxTopLevelWindow *w);
   wxRect GetFullWindowRect(wxTopLevelWindow *w);
   wxRect GetScreenRect();
   wxRect GetPanelRect(TrackPanel * panel);
   wxRect GetRulerRect(AdornedRulerPanel *ruler);
   wxRect GetTracksRect(TrackPanel * panel);
   wxRect GetTrackRect( AudacityProject * pProj, TrackPanel * panel,int n);
   wxString WindowFileName(AudacityProject * proj, wxTopLevelWindow *w);

public:
   static ScreenshotCommand * mpShooter;
   static void (*mIdleHandler)(wxIdleEvent& event);
   static void SetIdleHandler( void (*pHandler)(wxIdleEvent& event) ){mIdleHandler=pHandler;};
   static bool MayCapture( wxDialog * pDlg );

   void CaptureWindowOnIdle( const CommandContext & context, wxWindow * pWin );
   wxTopLevelWindow *GetFrontWindow(AudacityProject *project);
};

#endif /* End of include guard: __SCREENSHOT_COMMAND__ */
