/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dominic Mazzoni
   Dan Horgan
   James Crook

******************************************************************//**

\class ScreenshotCommand
\brief Implements a command for capturing various areas of the screen or
project window.  It's one big if-elseif switch statement with lots of 
small calculations of rectangles.

*//*******************************************************************/

#include "../Audacity.h"
#include "ScreenshotCommand.h"
#include "CommandTargets.h"
#include "../Project.h"
#include <wx/toplevel.h>
#include <wx/dcscreen.h>
#include <wx/dcmemory.h>
#include <wx/settings.h>
#include <wx/bitmap.h>
#include <wx/valgen.h>

#include "../Track.h"
#include "../TrackPanel.h"
#include "../toolbars/ToolManager.h"
#include "../toolbars/ToolBar.h"
#include "../toolbars/ControlToolBar.h"
#include "../toolbars/DeviceToolBar.h"
#include "../toolbars/EditToolBar.h"
#include "../toolbars/MeterToolBar.h"
#include "../toolbars/MixerToolBar.h"
#include "../toolbars/SelectionBar.h"
#include "../toolbars/ToolsToolBar.h"
#include "../toolbars/TranscriptionToolBar.h"
#include "../widgets/Ruler.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"


enum kCaptureTypes
{
   kwindow,
   kfullwindow,
   kwindowplus,
   kfullscreen,
   ktoolbars,
   kmenus,
   keffects,
   kpreferences,
   kselectionbar,
   kspectralselection,
   ktools,
   ktransport,
   kmixer,
   kmeter,
   kplaymeter,
   krecordmeter,
   kedit,
   kdevice,
   kscrub,
   ktranscription,
   ktrackpanel,
   kruler,
   ktracks,
   kfirsttrack,
   ksecondtrack,
   ktracksplus,
   kfirsttrackplus,
   nCaptureWhats
};

static const wxString kCaptureWhatStrings[nCaptureWhats] =
{
   XO("Window"),
   XO("Full_Window"),
   XO("Window_Plus"),
   XO("Fullscreen"),
   XO("Toolbars"),
   XO("Menus"),
   XO("Effects"),
   XO("Preferences"),
   XO("Selectionbar"),
   XO("Spectral_Selection"),
   XO("Tools"),
   XO("Transport"),
   XO("Mixer"),
   XO("Meter"),
   XO("Play_Meter"),
   XO("Record_Meter"),
   XO("Edit"),
   XO("Device"),
   XO("Scrub"),
   XO("Transcription"),
   XO("Trackpanel"),
   XO("Ruler"),
   XO("Tracks"),
   XO("First_Track"),
   XO("Second_Track")
   XO("Tracks_Plus"),
   XO("First_Track_Plus"),
};


enum kBackgrounds
{
   kBlue,
   kWhite,
   kNone,
   nBackgrounds
};

static const wxString kBackgroundStrings[nBackgrounds] =
{
   XO("Blue"),
   XO("White"),
   XO("None")
};


bool ScreenshotCommand::DefineParams( ShuttleParams & S ){ 
   wxArrayString whats(nCaptureWhats, kCaptureWhatStrings);
   wxArrayString backs(nBackgrounds, kBackgroundStrings);
   S.Define(     mPath, wxT("Path"),         wxT(""),       wxT(""), wxT(""), wxT(""));
   S.DefineEnum( mWhat, wxT("CaptureWhat"),  wxT("Window"), whats );
   S.DefineEnum( mBack, wxT("Background"),   wxT("None"), backs );
   return true;
};

void ScreenshotCommand::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString whats(nCaptureWhats, kCaptureWhatStrings);
   wxArrayString backs(nBackgrounds, kBackgroundStrings);
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox( _("Path:"), mPath);
      S.TieChoice( _("Capture What:"), mWhat, &whats);
      S.TieChoice( _("Background:"), mBack, &backs);
   }
   S.EndMultiColumn();
}

// static member variable.
void (*ScreenshotCommand::mIdleHandler)(wxIdleEvent& event) = NULL;
// This static variable is used to get from an idle event to the screenshot
// command that caused the idle event interception to be set up.
ScreenshotCommand * ScreenshotCommand::mpShooter=NULL;

// IdleHandler is expected to be called from EVT_IDLE when a dialog has been 
// fully created.  Usually the dialog will have been created by invoking
// an effects gui.
void IdleHandler(wxIdleEvent& event){
   wxWindow * pWin = dynamic_cast<wxWindow*>(event.GetEventObject());
   wxASSERT( pWin );
   pWin->Unbind(wxEVT_IDLE, IdleHandler);
   CommandContext context( *GetActiveProject() );
   // We have the relevant window, so go and capture it.
   if( ScreenshotCommand::mpShooter )
      ScreenshotCommand::mpShooter->CaptureWindowOnIdle( context, pWin );
}


wxTopLevelWindow *ScreenshotCommand::GetFrontWindow(AudacityProject *project)
{
   wxWindow *front = NULL;
   wxWindow *proj = wxGetTopLevelParent(project);

   // This is kind of an odd hack.  There's no method to enumerate all
   // possible windows, so we search the whole screen for any windows
   // that are not this one and not the given Audacity project and
   // if we find anything, we assume that's the dialog the user wants
   // to capture.

   int width, height, x, y;
   wxDisplaySize(&width, &height);
   for (x = 0; x < width; x += 50) {
      for (y = 0; y < height; y += 50) {
         wxWindow *win = wxFindWindowAtPoint(wxPoint(x, y));
         if (win) {
            win = wxGetTopLevelParent(win);
            if (win != mIgnore && win != proj) {
               front = win;
               break;
            }
         }
      }
   }

   if (!front || !front->IsTopLevel()) {
      return (wxTopLevelWindow *)proj;
   }

   return (wxTopLevelWindow *)front;
}

wxRect ScreenshotCommand::GetBackgroundRect()
{
   wxRect r;

   r.x = 16;
   r.y = 16;
   r.width = r.x * 2;
   r.height = r.y * 2;

   return r;
}

static void Yield()
{
   int cnt;
   for (cnt = 10; cnt && !wxTheApp->Yield(true); cnt--) {
      wxMilliSleep(10);
   }
   wxMilliSleep(200);
   for (cnt = 10; cnt && !wxTheApp->Yield(true); cnt--) {
      wxMilliSleep(10);
   }
}

bool ScreenshotCommand::Capture(
   const CommandContext & context,
   const wxString &filename,
                          wxWindow *window, wxRect r,
                          bool bg)
{
   int width = r.width;
   int height = r.height;
   if( r.width == 0 )
      return false;
   if (window) {
      if (window->IsTopLevel()) {
         window->Raise();
      }
      else {
         wxGetTopLevelParent(window)->Raise();
      }
   }

   Yield();

   int screenW, screenH;
   wxDisplaySize(&screenW, &screenH);
   // Bug 1378 workaround.
   // wx 3.0.2 has a bug in Blit from ScreenDC where in default mode 
   // much is drawn transparent - including for example black text!
   // Forcing 24 bit here is a workaround.
   wxBitmap full(screenW, screenH, 24);

   wxScreenDC screenDC;
   wxMemoryDC fullDC;

#if defined(__WXMAC__) && !wxCHECK_VERSION(3, 0, 0)
   full = DoGetAsBitmap(NULL);
#else
   // We grab the whole screen image since there seems to be a problem with
   // using non-zero source coordinates on OSX.  (as of wx2.8.9)
   fullDC.SelectObject(full);
   fullDC.Blit(0, 0, screenW, screenH, &screenDC, 0, 0);
   fullDC.SelectObject(wxNullBitmap);
#endif

   //wxRect r(x, y, width, height);


   // Convert to screen coordinates if needed
   if (window && window->GetParent() && !window->IsTopLevel()) {
      r.SetPosition(window->GetParent()->ClientToScreen(r.GetPosition()));
   }

   // Ensure within bounds (x/y are negative on Windows when maximized)
   r.Intersect(wxRect(0, 0, screenW, screenH));

   // Extract the actual image
   wxBitmap part = full.GetSubBitmap(r);

   // Add a background
   if (bg && mBackground) {
      wxRect b = GetBackgroundRect();

      wxBitmap back(width + b.width, height + b.height);
      fullDC.SelectObject(back);

      fullDC.SetBackground(wxBrush(mBackColor, wxSOLID));
      fullDC.Clear();

      fullDC.DrawBitmap(part, b.x, b.y);
      fullDC.SelectObject(wxNullBitmap);

      part = back;
   }

   // Save the final image
   wxImage image = part.ConvertToImage();
   ::wxBell();

   if (image.SaveFile(filename)) {
      // flush
      context.Status( wxString::Format( _("Saved %s"), filename ), true );
   }
   else {
      context.Error(
         wxString::Format( _("Error trying to save file: %s"), filename ) );
      return false;
   }
   return true;
}

bool ScreenshotCommand::CaptureToolbar(
   const CommandContext & context,
   ToolManager *man, int type, const wxString &name)
{
   bool visible = man->IsVisible(type);
   if (!visible) {
      man->ShowHide(type);
      Yield();
   }

   wxWindow *w = man->GetToolBar(type);
   int x = 0, y = 0;
   int width, height;

   w->ClientToScreen(&x, &y);
   w->GetParent()->ScreenToClient(&x, &y);
   w->GetClientSize(&width, &height);
   
   bool result = Capture(context, name, w, wxRect(x, y, width, height));

   if (!visible) {
      man->ShowHide(type);
      if (mIgnore)
         mIgnore->Raise();
   }
   return result;
}

bool ScreenshotCommand::CaptureDock(
   const CommandContext & context,
   wxWindow *win, const wxString &mFileName)
{
   int x = 0, y = 0;
   int width, height;

   win->ClientToScreen(&x, &y);
   win->GetParent()->ScreenToClient(&x, &y);
   win->GetClientSize(&width, &height);

   return Capture(context, mFileName, win, wxRect(x, y, width, height));
}

void ExploreMenu( 
   const CommandContext & context,
   wxMenu * pMenu, int Id, int depth ){
   Id;//compiler food.
   if( !pMenu )
      return;

   wxMenuItemList list = pMenu->GetMenuItems();
   size_t lcnt = list.GetCount();
   wxMenuItem * item;
   wxString Label;
   wxString Accel;

   for (size_t lndx = 0; lndx < lcnt; lndx++) {
      item = list.Item(lndx)->GetData();
      Label = item->GetItemLabelText();
      Accel = item->GetItemLabel();
      if( Accel.Contains("\t") )
         Accel = Accel.AfterLast('\t');
      else
         Accel = "";
      if( item->IsSeparator() )
         Label = "----";
      int flags = 0;
      if (item->IsSubMenu())
         flags +=1;
      if (item->IsCheck() && item->IsChecked())
         flags +=2;

      wxLogDebug("T.Add( %2i, %2i,  0, \"%s¬%s\" );", depth, flags, Label,Accel ); 
      if (item->IsSubMenu()) {
         pMenu = item->GetSubMenu();
         ExploreMenu( context, pMenu, item->GetId(), depth+1 );
      }
   }
}

void ScreenshotCommand::CaptureMenus(
   const CommandContext & context,
   wxMenuBar*pBar, const wxString &mFileName)
{
   mFileName;//compiler food.
   if(!pBar ){
      wxLogDebug("No menus");
      return;
   }

   size_t cnt = pBar->GetMenuCount();
   size_t i;
   wxString Label;
   for(i=0;i<cnt;i++)
   {
      Label = pBar->GetMenuLabelText( i );
      wxLogDebug( "\nT.Add(  0,  0,  0, \"%s¬\" );", Label);
      ExploreMenu( context, pBar->GetMenu( i ), pBar->GetId(), 1 );
   }

#if 0
   int x = 0, y = 0;
   int width, height;

   win->ClientToScreen(&x, &y);
   win->GetParent()->ScreenToClient(&x, &y);
   win->GetClientSize(&width, &height);

   Capture(mFileName, win, wxRect(x, y, width, height));
#endif
}

// Handed a dialog, which it is given the option to capture.
bool ScreenshotCommand::MayCapture( wxDialog * pDlg )
{
   if( mIdleHandler == NULL )
      return false;
   pDlg->Bind( wxEVT_IDLE, mIdleHandler );
   mIdleHandler = NULL;
   pDlg->ShowModal();
   return true;
}

void ScreenshotCommand::CaptureWindowOnIdle( 
   const CommandContext & context,
   wxWindow * pWin )
{
   wxDialog * pDlg = dynamic_cast<wxDialog*>(pWin);
   if( !pDlg ){
      wxLogDebug("Event from bogus dlg" );
      return;
   }

   wxPoint Pos = pDlg->GetScreenPosition();
   wxSize Siz = pDlg->GetSize();
   wxString Title = pDlg->GetTitle();

   // Remove '/' from "Sliding Time Scale/Pitch Shift..."
   // and any other effects that have illegal filename chanracters.
   Title.Replace( "/", "" );
   Title.Replace( ":", "" );
   wxString Name = mDirToWriteTo + Title + ".png";

   wxLogDebug("Taking screenshot of window %s (%i,%i,%i,%i)", Name, 
         Pos.x, Pos.y, Siz.x, Siz.y );
   // This delay is needed, as dialogs take a moment or two to fade in.
   wxMilliSleep( 200 );
   // JKC: The border of 7 pixels was determined from a trial capture and then measuring
   // in the GIMP.  I'm unsure where the border comes from.
   Capture( context, Name, pDlg, wxRect((int)Pos.x+7, (int)Pos.y, (int)Siz.x-14, (int)Siz.y-7) );

   // We've captured the dialog, so now dismiss the dialog.
   wxCommandEvent Evt( wxEVT_BUTTON, wxID_CANCEL );
   pDlg->GetEventHandler()->AddPendingEvent( Evt );
}

void ScreenshotCommand::CapturePreferences( 
   const CommandContext & context,
   AudacityProject * pProject, const wxString &mFileName ){
   mFileName;//compiler food.
   CommandManager * pMan = pProject->GetCommandManager();

   // Yucky static variables.  Is there a better way?  The problem is that we need the
   // idle callback to know more about what to do.
   mDirToWriteTo = mFileName.BeforeLast('\\') + "\\";
   mpShooter = this;
   const int nPrefsPages = 19;

   for( int i=0;i<nPrefsPages;i++){
      // The handler is cleared each time it is used.
      SetIdleHandler( IdleHandler );
      gPrefs->Write(wxT("/Prefs/PrefsCategory"), (long)i);
      gPrefs->Flush();
      wxString Command = "Preferences";
      const CommandContext context( *pProject );
      if( !pMan->HandleTextualCommand( Command, context, AlwaysEnabledFlag, AlwaysEnabledFlag ) )
      {
         wxLogDebug("Command %s not found", Command );
      }
      // This sleep is not needed, but gives user a chance to see the
      // dialogs as they whizz by.
      wxMilliSleep( 200 );
   }
}

void ScreenshotCommand::CaptureEffects( 
   const CommandContext & context,
   AudacityProject * pProject, const wxString &mFileName ){
   mFileName;//compiler food.
   CommandManager * pMan = pProject->GetCommandManager();
   wxString Str;
   // Yucky static variables.  Is there a better way?  The problem is that we need the
   // idle callback to know more about what to do.
   mDirToWriteTo = mFileName.BeforeLast('\\') + "\\";
   mpShooter = this;

#define TRICKY_CAPTURE
#define CAPTURE_NYQUIST_TOO
   // Commented out the effects that don't have dialogs.
   // Also any problematic ones, 
   const wxString EffectNames[] = {

#ifdef TRICKY_CAPTURE
      //"Contrast...", // renamed
      "ContrastAnalyser",
      //"Plot Spectrum...", // renamed
      "PlotSpectrum",

      "Auto Duck...",  // needs a track below.
      //"Spectral edit multi tool",
      "Spectral edit parametric EQ...", // Needs a spectral selection.
      "Spectral edit shelves...",

      //"Noise Reduction...", // Exits twice...
      //"SC4...", //Has 'Close' rather than 'Cancel'.
#endif
      "Amplify...",
      "Bass and Treble...",
      "Change Pitch...",
      "Change Speed...",
      "Change Tempo...",
      "Click Removal...",
      "Compressor...",
      "Distortion...",
      "Echo...",
      "Equalization...",
      //"Fade In",
      //"Fade Out",
      //"Invert",
      "Normalize...",
      "Paulstretch...",
      "Phaser...",
      //"Repair",
      "Repeat...",
      "Reverb...",
      //"Reverse",
      "Sliding Time Scale/Pitch Shift...",
      "Truncate Silence...",
      "Wahwah...",
      // Sole LADSPA effect...
#ifdef CAPTURE_NYQUIST_TOO
      "Adjustable Fade...",
      "Clip Fix...",
      //"Crossfade Clips",
      "Crossfade Tracks...",
      "Delay...",
      "High Pass Filter...",
      "Limiter...",
      "Low Pass Filter...",
      "Notch Filter...",
      "Nyquist Prompt...",
      //"Studio Fade Out",
      "Tremolo...",
      "Vocal Reduction and Isolation...",
      "Vocal Remover...",
      "Vocoder...",
#endif
      // Generators.....
      "Chirp...",
      "DTMF Tones...",
      "Noise...",
      "Silence...",
      "Tone...",
#ifdef CAPTURE_NYQUIST_TOO
      "Pluck...",
      "Rhythm Track...",
      "Risset Drum...",
      "Sample Data Import...",
#endif
      // Analyzers...
      "Find Clipping...",
#ifdef CAPTURE_NYQUIST_TOO
      "Beat Finder...",
      "Regular Interval Labels...",
      "Sample Data Export...",
      "Silence Finder...",
      "Sound Finder...",
#endif
   };

   for( int i=0;i<sizeof(EffectNames)/sizeof(EffectNames[0]);i++){
      // The handler is cleared each time it is used.
      SetIdleHandler( IdleHandler );
      Str = EffectNames[i];
      const CommandContext context( *pProject );
      if( !pMan->HandleTextualCommand( Str, context, AlwaysEnabledFlag, AlwaysEnabledFlag ) )
      {
         wxLogDebug("Command %s not found", Str);
      }
      // This sleep is not needed, but gives user a chance to see the
      // dialogs as they whizz by.
      wxMilliSleep( 200 );
   }
}

wxString ScreenshotCommand::MakeFileName(const wxString &path, const wxString &basename)
{
   wxFileName prefixPath;
   prefixPath.AssignDir(path);
   wxString prefix = prefixPath.GetPath
      (wxPATH_GET_VOLUME|wxPATH_GET_SEPARATOR);

   wxString filename;
   int i = 0;
   do {
      filename.Printf(wxT("%s%s%03d.png"),
                      prefix, basename, i);
      i++;
   } while (::wxFileExists(filename));

   return filename;
}

void ScreenshotCommand::GetDerivedParams()
{
      // Read the parameters that were passed in
   mFilePath    = mPath;
   mCaptureMode = mWhat;
   wxString background  = mBack;

   // Build a suitable filename
   mFileName = MakeFileName(mFilePath, mCaptureMode);

   if (background.IsSameAs(wxT("Blue")))
   {
      mBackground = true;
      mBackColor = wxColour(51, 102, 153);
   }
   else if (background.IsSameAs(wxT("White")))
   {
      mBackground = true;
      mBackColor = wxColour(255, 255, 255);
   }
   else
   {
      mBackground = false;
   }

}

wxRect ScreenshotCommand::GetWindowRect(wxTopLevelWindow *w){
   int x = 0, y = 0;
   int width, height;

   w->ClientToScreen(&x, &y);
   w->GetClientSize(&width, &height);
   return wxRect( x,y,width,height);
}

wxRect ScreenshotCommand::GetFullWindowRect(wxTopLevelWindow *w){

   wxRect r = w->GetRect();
   r.SetPosition(w->GetScreenPosition());
   r = w->GetScreenRect();

#if defined(__WXGTK__)
   // In wxGTK, we need to include decoration sizes
   r.width += (wxSystemSettings::GetMetric(wxSYS_BORDER_X, w) * 2);
   r.height += wxSystemSettings::GetMetric(wxSYS_CAPTION_Y, w) +
      wxSystemSettings::GetMetric(wxSYS_BORDER_Y, w);
#endif
   if (!mBackground && mCaptureMode.IsSameAs(wxT("Window_Plus")))
   {
      // background colour not selected but we want a background
      wxRect b = GetBackgroundRect();
      r.x = (r.x - b.x) >= 0 ? (r.x - b.x): 0;
      r.y = (r.y - b.y) >= 0 ? (r.y - b.y): 0;
      r.width += b.width;
      r.height += b.height;
   }

   return r;
}

wxRect ScreenshotCommand::GetScreenRect(){
   int width, height;
   wxDisplaySize(&width, &height);

   return wxRect( 0,0,width,height);
}

wxRect ScreenshotCommand::GetPanelRect(TrackPanel * panel){
   //AdornedRulerPanel *ruler = panel->mRuler;

   int h = panel->mRuler->GetRulerHeight();
   int x = 0, y = -h;
   int width, height;

   panel->ClientToScreen(&x, &y);
   panel->GetParent()->ScreenToClient(&x, &y);
   panel->GetClientSize(&width, &height);
   return wxRect(x, y, width, height + h);
}

wxRect ScreenshotCommand::GetRulerRect(AdornedRulerPanel *ruler){
   int x = 0, y = 0;
   int width, height;

   ruler->ClientToScreen(&x, &y);
   ruler->GetParent()->ScreenToClient(&x, &y);
   ruler->GetClientSize(&width, &height);
   height = ruler->GetRulerHeight();
   return wxRect( x, y, width, height);
}

wxRect ScreenshotCommand::GetTracksRect(TrackPanel * panel){
   int x = 0, y = 0;
   int width, height;

   panel->ClientToScreen(&x, &y);
   panel->GetParent()->ScreenToClient(&x, &y);
   panel->GetClientSize(&width, &height);

   return wxRect( x, y, width, height);
}

wxRect ScreenshotCommand::GetTrackRect( AudacityProject * pProj, TrackPanel * panel, int n){
   auto FindRectangle = []( TrackPanel &panel, Track &t )
   {
      // This rectangle omits the focus ring about the track, and
      // also within that, a narrow black border with a "shadow" below and
      // to the right
      wxRect rect = panel.FindTrackRect( &t, false );

      // Enlarge horizontally.
      // PRL:  perhaps it's one pixel too much each side, including some gray
      // beyond the yellow?
      rect.x = 0;
      panel.GetClientSize(&rect.width, nullptr);

      // Enlarge vertically, enough to enclose the yellow focus border pixels
      // Omit the outermost ring of gray pixels

      // (Note that TrackPanel paints its focus over the "top margin" of the
      // rectangle allotted to the track, according to Track::GetY() and
      // Track::GetHeight(), but also over the margin of the next track.)

      rect.height += kBottomMargin;
      int dy = kTopMargin - 1;
      rect.Inflate( 0, dy );

      // Reposition it relative to parent of panel
      rect.SetPosition(
         panel.GetParent()->ScreenToClient(
            panel.ClientToScreen(
               rect.GetPosition())));

      return rect;
   };

   TrackListIterator iter(pProj->GetTracks());
   int count = 0;
   for (auto t = iter.First(); t; t = iter.Next()) {
      count +=  1;
      if( count > n )
      {
         wxRect r =  FindRectangle( *panel, *t );
         return r;
      }
      if( t->GetLinked() ){
         t = iter.Next();
         if( !t )
            break;
      }
   }
   return wxRect( 0,0,0,0);
}

wxString ScreenshotCommand::WindowFileName(AudacityProject * proj, wxTopLevelWindow *w){
   if (w != proj && w->GetTitle() != wxT("")) {
      mFileName = MakeFileName(mFilePath,
            mCaptureMode + (wxT("-") + w->GetTitle() + wxT("-")));
   }
   return mFileName;
}

bool ScreenshotCommand::Apply(const CommandContext & context)
{
   GetDerivedParams();
   //Don't reset the toolbars to a known state.
   //We will be capturing variations of them.
   //context.GetProject()->GetToolManager()->Reset();

   wxTopLevelWindow *w = GetFrontWindow(context.GetProject());
   if (!w)
      return false;

   TrackPanel *panel = context.GetProject()->GetTrackPanel();
   AdornedRulerPanel *ruler = panel->mRuler;

   int x1,y1,x2,y2;
   w->ClientToScreen(&x1, &y1);
   panel->ClientToScreen(&x2, &y2);

   wxPoint p( x2-x1, y2-y1);

   if (mCaptureMode.IsSameAs(wxT("Window")))
      return Capture(context,  WindowFileName( context.GetProject(), w ) , w, GetWindowRect(w));
   else if (mCaptureMode.IsSameAs(wxT("Fullwindow"))
         || mCaptureMode.IsSameAs(wxT("Window_Plus")))
      return Capture(context,  WindowFileName( context.GetProject(), w ) , w, GetFullWindowRect(w));
   else if (mCaptureMode.IsSameAs(wxT("Fullscreen")))
      return Capture(context, mFileName, w,GetScreenRect());
   else if (mCaptureMode.IsSameAs(wxT("Toolbars")))
      return CaptureDock(context, context.GetProject()->GetToolManager()->GetTopDock(), mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Menus")))
      CaptureMenus(context, context.GetProject()->GetMenuBar(), mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Effects")))
      CaptureEffects(context, context.GetProject(), mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Preferences")))
      CapturePreferences(context, context.GetProject(), mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Selectionbar")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), SelectionBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Spectral_Selection")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), SpectralSelectionBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Tools")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), ToolsBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Transport")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), TransportBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Mixer")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), MixerBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Meter")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), MeterBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Recordmeter")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), RecordMeterBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Playmeter")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), PlayMeterBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Edit")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), EditBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Device")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), DeviceBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Transcription")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), TranscriptionBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Scrub")))
      return CaptureToolbar(context, context.GetProject()->GetToolManager(), ScrubbingBarID, mFileName);
   else if (mCaptureMode.IsSameAs(wxT("Trackpanel")))
      return Capture(context, mFileName, panel, GetPanelRect(panel));
   else if (mCaptureMode.IsSameAs(wxT("Ruler")))
      return Capture(context, mFileName, ruler, GetRulerRect(ruler) );
   else if (mCaptureMode.IsSameAs(wxT("Tracks")))
      return Capture(context, mFileName, panel, GetTracksRect(panel));
   else if (mCaptureMode.IsSameAs(wxT("First_Track")))
      return Capture(context, mFileName, panel, GetTrackRect( context.GetProject(), panel, 0 ) );
   else if (mCaptureMode.IsSameAs(wxT("Second_Track")))
      return Capture(context, mFileName, panel, GetTrackRect( context.GetProject(), panel, 1 ) );
   else if (mCaptureMode.IsSameAs(wxT("Tracks_Plus")))
   {  wxRect r = GetTracksRect(panel);
      r.SetTop( r.GetTop() - ruler->GetRulerHeight() );
      r.SetHeight( r.GetHeight() + ruler->GetRulerHeight() );
      return Capture(context, mFileName, panel, r);
   }
   else if (mCaptureMode.IsSameAs(wxT("First_Track_Plus")))
   {  wxRect r = GetTrackRect(context.GetProject(), panel, 0 );
      r.SetTop( r.GetTop() - ruler->GetRulerHeight() );
      r.SetHeight( r.GetHeight() + ruler->GetRulerHeight() );
      return Capture(context, mFileName, panel, r );
   }
   else
      return false;

   return true;
}
