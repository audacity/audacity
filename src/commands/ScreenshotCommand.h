/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dominic Mazzoni
   Dan Horgan

**********************************************************************/

#ifndef __SCREENSHOTCOMMAND__
#define __SCREENSHOTCOMMAND__

#include "Command.h"
#include "CommandType.h"

#include <wx/colour.h>
class wxWindow;
class wxTopLevelWindow;
class wxCommandEvent;
class wxRect;
class ToolManager;
class CommandOutputTarget;

class ScreenshotCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class ScreenshotCommand : public CommandImplementation
{
private:
   // May need to ignore the screenshot dialog
   wxWindow *mIgnore;

   bool mBackground;
   wxColour mBackColor;

   wxString MakeFileName(wxString path, wxString basename);

   wxRect GetBackgroundRect();
   void Capture(wxString basename,
         wxWindow *window,
         int x, int y, int width, int height,
         bool bg = false);
   void CaptureToolbar(ToolManager *man, int type, wxString name);
   void CaptureDock(wxWindow *win, wxString fileName);

public:
   wxTopLevelWindow *GetFrontWindow(AudacityProject *project);
   ScreenshotCommand(CommandType &type,
                     CommandOutputTarget *output,
                     wxWindow *ignore = NULL)
      : CommandImplementation(type, output),
        mIgnore(ignore),
        mBackground(false)
   { }
   bool Apply(CommandExecutionContext context);
};

#endif /* End of include guard: __SCREENSHOTCOMMAND__ */

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: TBD
