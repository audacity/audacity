/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dominic Mazzoni
   Dan Horgan

******************************************************************//**

\class ScreenshotCommand
\brief Implements a command for capturing various areas of the screen or
project window.

*//*******************************************************************/

/* TODO: JKC: The screenshot code is very verbose and should be made 
   much shorter.  It could work from a single list of function 
   names and functors.
  */

#include "ScreenshotCommand.h"
#include "CommandTargets.h"
#include "../Project.h"
#include <wx/toplevel.h>
#include <wx/dcscreen.h>
#include <wx/dcmemory.h>
#include <wx/settings.h>
#include <wx/bitmap.h>

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

#if defined(__WXMAC__) && !wxCHECK_VERSION(3, 0, 0)
//
// This is a temporary solution for capturing screenshots on
// OS X 10.6 or greater.  This needs to go away once we move
// to wx3.
//
// (This was copied from wx3.1.0 source and hacked up a bit.)
//
#include <wx/mac/private.h>
#include <dlfcn.h>

typedef CGImageRef (*CGDisplayCreateImageFunc)(CGDirectDisplayID displayID);

static wxBitmap DoGetAsBitmap(const wxRect *subrect)
{
    CGRect cgbounds = CGDisplayBounds(CGMainDisplayID());

    wxRect rect = subrect ? *subrect : wxRect(0, 0, cgbounds.size.width, cgbounds.size.height);

    wxBitmap bmp(rect.GetSize().GetWidth(), rect.GetSize().GetHeight(), 32);

    CGDisplayCreateImageFunc createImage =
        (CGDisplayCreateImageFunc) dlsym(RTLD_NEXT, "CGDisplayCreateImage");
    if (createImage == NULL)
    {
        return bmp;
    }

    CGRect srcRect = CGRectMake(rect.x, rect.y, rect.width, rect.height);

    CGContextRef context = (CGContextRef)bmp.GetHBITMAP();

    CGContextSaveGState(context);

    CGContextTranslateCTM( context, 0,  cgbounds.size.height );
    CGContextScaleCTM( context, 1, -1 );

    if ( subrect )
        srcRect = CGRectOffset( srcRect, -subrect->x, -subrect->y ) ;

    CGImageRef image = NULL;

    image = createImage(kCGDirectMainDisplay);

    wxASSERT_MSG(image, wxT("wxScreenDC::GetAsBitmap - unable to get screenshot."));

    CGContextDrawImage(context, srcRect, image);

    CGImageRelease(image);

    CGContextRestoreGState(context);

    return bmp;
}
#endif

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
   // We have the relevant window, so go and capture it.
   if( ScreenshotCommand::mpShooter )
      ScreenshotCommand::mpShooter->CaptureWindowOnIdle( pWin );
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

void ScreenshotCommand::Capture(const wxString &filename,
                          wxWindow *window,
                          int x, int y, int width, int height,
                          bool bg)
{
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

   wxRect r(x, y, width, height);

   // Ensure within bounds (x/y are negative on Windows when maximized)
   r.Intersect(wxRect(0, 0, screenW, screenH));

   // Convert to screen coordinates if needed
   if (window && window->GetParent() && !window->IsTopLevel()) {
      r.SetPosition(window->GetParent()->ClientToScreen(r.GetPosition()));
   }

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
   if (image.SaveFile(filename)) {
      mOutput->Status(_("Saved ") + filename);
   }
   else {
      mOutput->Error(_("Error trying to save file: ") + filename);
   }

   ::wxBell();
}

void ScreenshotCommand::CaptureToolbar(ToolManager *man, int type, const wxString &name)
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

   Capture(name, w, x, y, width, height);

   if (!visible) {
      man->ShowHide(type);
      if (mIgnore)
         mIgnore->Raise();
   }
}

void ScreenshotCommand::CaptureDock(wxWindow *win, const wxString &fileName)
{
   int x = 0, y = 0;
   int width, height;

   win->ClientToScreen(&x, &y);
   win->GetParent()->ScreenToClient(&x, &y);
   win->GetClientSize(&width, &height);

   Capture(fileName, win, x, y, width, height);
}

void ExploreMenu( wxMenu * pMenu, int Id, int depth ){
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
         ExploreMenu( pMenu, item->GetId(), depth+1 );
      }
   }
}

void ScreenshotCommand::CaptureMenus(wxMenuBar*pBar, const wxString &fileName)
{
   fileName;//compiler food.
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
      ExploreMenu( pBar->GetMenu( i ), pBar->GetId(), 1 );
   }

#if 0
   int x = 0, y = 0;
   int width, height;

   win->ClientToScreen(&x, &y);
   win->GetParent()->ScreenToClient(&x, &y);
   win->GetClientSize(&width, &height);

   Capture(fileName, win, x, y, width, height);
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

void ScreenshotCommand::CaptureWindowOnIdle( wxWindow * pWin )
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
   Capture( Name, pDlg, (int)Pos.x+7, (int)Pos.y, (int)Siz.x-14, (int)Siz.y-7 );

   // We've captured the dialog, so now dismiss the dialog.
   wxCommandEvent Evt( wxEVT_BUTTON, wxID_CANCEL );
   pDlg->GetEventHandler()->AddPendingEvent( Evt );
}

void ScreenshotCommand::CapturePreferences( AudacityProject * pProject, const wxString &fileName ){
   fileName;//compiler food.
   CommandManager * pMan = pProject->GetCommandManager();

   // Yucky static variables.  Is there a better way?  The problem is that we need the
   // idle callback to know more about what to do.
   mDirToWriteTo = fileName.BeforeLast('\\') + "\\";
   mpShooter = this;
   const int nPrefsPages = 19;

   for( int i=0;i<nPrefsPages;i++){
      // The handler is cleared each time it is used.
      SetIdleHandler( IdleHandler );
      gPrefs->Write(wxT("/Prefs/PrefsCategory"), (long)i);
      gPrefs->Flush();
      wxString Command = "Preferences";
      if( !pMan->HandleTextualCommand( Command, AlwaysEnabledFlag, AlwaysEnabledFlag ) )
      {
         wxLogDebug("Command %s not found", Command );
      }
      // This sleep is not needed, but gives user a chance to see the
      // dialogs as they whizz by.
      wxMilliSleep( 200 );
   }
}

void ScreenshotCommand::CaptureEffects( AudacityProject * pProject, const wxString &fileName ){
   fileName;//compiler food.
   CommandManager * pMan = pProject->GetCommandManager();
   wxString Str;
   // Yucky static variables.  Is there a better way?  The problem is that we need the
   // idle callback to know more about what to do.
   mDirToWriteTo = fileName.BeforeLast('\\') + "\\";
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
      if( !pMan->HandleTextualCommand( Str, AlwaysEnabledFlag, AlwaysEnabledFlag ) )
      {
         wxLogDebug("Command %s not found", Str);
      }
      // This sleep is not needed, but gives user a chance to see the
      // dialogs as they whizz by.
      wxMilliSleep( 200 );
   }
}


wxString ScreenshotCommandType::BuildName()
{
   return wxT("Screenshot");
}

void ScreenshotCommandType::BuildSignature(CommandSignature &signature)
{
   auto captureModeValidator = make_movable<OptionValidator>();
   captureModeValidator->AddOption(wxT("window"));
   captureModeValidator->AddOption(wxT("fullwindow"));
   captureModeValidator->AddOption(wxT("windowplus"));
   captureModeValidator->AddOption(wxT("fullscreen"));
   captureModeValidator->AddOption(wxT("toolbars"));
   captureModeValidator->AddOption(wxT("menus"));
   captureModeValidator->AddOption(wxT("effects"));
   captureModeValidator->AddOption(wxT("preferences"));
   captureModeValidator->AddOption(wxT("selectionbar"));
   captureModeValidator->AddOption(wxT("spectralselection"));
   captureModeValidator->AddOption(wxT("tools"));
   captureModeValidator->AddOption(wxT("transport"));
   captureModeValidator->AddOption(wxT("mixer"));
   captureModeValidator->AddOption(wxT("meter"));
   captureModeValidator->AddOption(wxT("playmeter"));
   captureModeValidator->AddOption(wxT("recordmeter"));
   captureModeValidator->AddOption(wxT("edit"));
   captureModeValidator->AddOption(wxT("device"));
   captureModeValidator->AddOption(wxT("scrub"));
   captureModeValidator->AddOption(wxT("transcription"));
   captureModeValidator->AddOption(wxT("trackpanel"));
   captureModeValidator->AddOption(wxT("ruler"));
   captureModeValidator->AddOption(wxT("tracks"));
   captureModeValidator->AddOption(wxT("firsttrack"));
   captureModeValidator->AddOption(wxT("secondtrack"));

   auto backgroundValidator = make_movable<OptionValidator>();
   backgroundValidator->AddOption(wxT("Blue"));
   backgroundValidator->AddOption(wxT("White"));
   backgroundValidator->AddOption(wxT("None"));

   auto filePathValidator = make_movable<DefaultValidator>();

   signature.AddParameter(wxT("CaptureMode"),
                          wxT("fullscreen"),
                          std::move(captureModeValidator));
   signature.AddParameter(wxT("Background"),
                          wxT("None"),
                          std::move(backgroundValidator));
   signature.AddParameter(wxT("FilePath"), wxT(""), std::move(filePathValidator));
}

CommandHolder ScreenshotCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<ScreenshotCommand>(*this, std::move(target));
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
                      prefix.c_str(), basename.c_str(), i);
      i++;
   } while (::wxFileExists(filename));

   return filename;
}

bool ScreenshotCommand::Apply(CommandExecutionContext context)
{
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

   // Read the parameters that were passed in
   wxString filePath    = GetString(wxT("FilePath"));
   wxString captureMode = GetString(wxT("CaptureMode"));
   wxString background  = GetString(wxT("Background"));

   // Build a suitable filename
   wxString fileName = MakeFileName(filePath, captureMode);

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

   //Don't reset the toolbars to a known state.
   //We wil lbe capturing variations of them.
   //context.GetProject()->GetToolManager()->Reset();

   wxTopLevelWindow *w = GetFrontWindow(context.GetProject());
   if (!w)
   {
      return false;
   }

   if (captureMode.IsSameAs(wxT("window")))
   {
      int x = 0, y = 0;
      int width, height;

      w->ClientToScreen(&x, &y);
      w->GetClientSize(&width, &height);

      if (w != context.GetProject() && w->GetTitle() != wxT("")) {
         fileName = MakeFileName(filePath,
               captureMode + (wxT("-") + w->GetTitle() + wxT("-")));
      }

      Capture(fileName, w, x, y, width, height);
   }
   else if (captureMode.IsSameAs(wxT("fullwindow"))
         || captureMode.IsSameAs(wxT("windowplus")))
   {

      wxRect r = w->GetRect();
      r.SetPosition(w->GetScreenPosition());
      r = w->GetScreenRect();

      if (w != context.GetProject() && w->GetTitle() != wxT("")) {
         fileName = MakeFileName(filePath,
               captureMode + (wxT("-") + w->GetTitle() + wxT("-")));
      }

#if defined(__WXGTK__)
      // In wxGTK, we need to include decoration sizes
      r.width += (wxSystemSettings::GetMetric(wxSYS_BORDER_X, w) * 2);
      r.height += wxSystemSettings::GetMetric(wxSYS_CAPTION_Y, w) +
         wxSystemSettings::GetMetric(wxSYS_BORDER_Y, w);
#endif
      if (!mBackground && captureMode.IsSameAs(wxT("windowplus")))
      {
         // background colour not selected but we want a background
         wxRect b = GetBackgroundRect();
         r.x = (r.x - b.x) >= 0 ? (r.x - b.x): 0;
         r.y = (r.y - b.y) >= 0 ? (r.y - b.y): 0;
         r.width += b.width;
         r.height += b.height;
      }

      Capture(fileName, w, r.x, r.y, r.width, r.height, true);
   }
   else if (captureMode.IsSameAs(wxT("fullscreen")))
   {
      int width, height;
      wxDisplaySize(&width, &height);

      Capture(fileName, w, 0, 0, width, height);
   }
   else if (captureMode.IsSameAs(wxT("toolbars")))
   {
      CaptureDock(context.GetProject()->GetToolManager()->GetTopDock(), fileName);
   }
   else if (captureMode.IsSameAs(wxT("menus")))
   {
      CaptureMenus(context.GetProject()->GetMenuBar(), fileName);
   }
   else if (captureMode.IsSameAs(wxT("effects")))
   {
      CaptureEffects(context.GetProject(), fileName);
   }
   else if (captureMode.IsSameAs(wxT("preferences")))
   {
      CapturePreferences(context.GetProject(), fileName);
   }
   else if (captureMode.IsSameAs(wxT("selectionbar")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), SelectionBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("spectralselection")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), SpectralSelectionBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("tools")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), ToolsBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("transport")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), TransportBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("mixer")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), MixerBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("meter")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), MeterBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("recordmeter")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), RecordMeterBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("playmeter")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), PlayMeterBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("edit")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), EditBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("device")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), DeviceBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("transcription")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), TranscriptionBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("scrub")))
   {
      CaptureToolbar(context.GetProject()->GetToolManager(), ScrubbingBarID, fileName);
   }
   else if (captureMode.IsSameAs(wxT("trackpanel")))
   {
      TrackPanel *panel = context.GetProject()->GetTrackPanel();
      //AdornedRulerPanel *ruler = panel->mRuler;

      int h = panel->mRuler->GetRulerHeight();
      int x = 0, y = -h;
      int width, height;

      panel->ClientToScreen(&x, &y);
      panel->GetParent()->ScreenToClient(&x, &y);
      panel->GetClientSize(&width, &height);

      Capture(fileName, panel, x, y, width, height + h);
   }
   else if (captureMode.IsSameAs(wxT("ruler")))
   {
      TrackPanel *panel = context.GetProject()->GetTrackPanel();
      AdornedRulerPanel *ruler = panel->mRuler;

      int x = 0, y = 0;
      int width, height;

      ruler->ClientToScreen(&x, &y);
      ruler->GetParent()->ScreenToClient(&x, &y);
      ruler->GetClientSize(&width, &height);
      height = ruler->GetRulerHeight();

      Capture(fileName, ruler, x, y, width, height);
   }
   else if (captureMode.IsSameAs(wxT("tracks")))
   {
      TrackPanel *panel = context.GetProject()->GetTrackPanel();

      int x = 0, y = 0;
      int width, height;

      panel->ClientToScreen(&x, &y);
      panel->GetParent()->ScreenToClient(&x, &y);
      panel->GetClientSize(&width, &height);

      Capture(fileName, panel, x, y, width, height);
   }
   else if (captureMode.IsSameAs(wxT("firsttrack")))
   {
      TrackPanel *panel = context.GetProject()->GetTrackPanel();
      TrackListIterator iter(context.GetProject()->GetTracks());
      Track * t = iter.First();
      if (!t) {
         return false;
      }
      wxRect r = FindRectangle( *panel, *t );
      Capture(fileName, panel, r.x, r.y, r.width, r.height);
   }
   else if (captureMode.IsSameAs(wxT("secondtrack")))
   {
      TrackPanel *panel = context.GetProject()->GetTrackPanel();
      TrackListIterator iter(context.GetProject()->GetTracks());
      Track * t = iter.First();
      if (!t) {
         return false;
      }
      if (t->GetLinked()) {
         t = iter.Next();
      }
      t = iter.Next();
      if (!t) {
         return false;
      }
      wxRect r = FindRectangle( *panel, *t );
      Capture(fileName, panel, r.x, r.y, r.width, r.height);
   }
   else
   {
      // Invalid capture mode!
      return false;
   }

   return true;
}
