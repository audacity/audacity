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

#include "ScreenshotCommand.h"
#include "CommandTargets.h"
#include "../AudacityApp.h"
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
   for (cnt = 10; cnt && !wxGetApp().Yield(true); cnt--) {
      wxMilliSleep(10);
   }
   wxMilliSleep(200);
   for (cnt = 10; cnt && !wxGetApp().Yield(true); cnt--) {
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
   captureModeValidator->AddOption(wxT("selectionbar"));
   captureModeValidator->AddOption(wxT("tools"));
   captureModeValidator->AddOption(wxT("transport"));
   captureModeValidator->AddOption(wxT("mixer"));
   captureModeValidator->AddOption(wxT("meter"));
   captureModeValidator->AddOption(wxT("edit"));
   captureModeValidator->AddOption(wxT("device"));
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

   // Reset the toolbars to a known state
   context.GetProject()->GetToolManager()->Reset();

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
   else if (captureMode.IsSameAs(wxT("selectionbar")))
   {
      CaptureDock(context.GetProject()->GetToolManager()->GetBotDock(), fileName);
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
   else if (captureMode.IsSameAs(wxT("scrubbing")))
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
      wxRect r = panel->FindTrackRect(t, true);

      int x = 0, y = r.y - 3;
      int width, height;

      panel->ClientToScreen(&x, &y);
      panel->GetParent()->ScreenToClient(&x, &y);
      panel->GetClientSize(&width, &height);

      Capture(fileName, panel, x, y, width, r.height + 6);

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
      wxRect r = panel->FindTrackRect(t, true);

      int x = 0, y = r.y - 3;
      int width, height;

      panel->ClientToScreen(&x, &y);
      panel->GetParent()->ScreenToClient(&x, &y);
      panel->GetClientSize(&width, &height);

      Capture(fileName, panel, x, y, width, r.height + 6);
   }
   else
   {
      // Invalid capture mode!
      return false;
   }

   return true;
}
