/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 or later - see LICENSE.txt

   Dan Horgan

   Paul Licameli split from CommandTargets.h

******************************************************************//**

\file wxCommandTargets
\brief Contains classes for 'command output targets' - objects which can
receive output from a command. For instance, a progress target might pass the
information to a GUI ProgressDialog. Using abstract targets means the command
objects needn't be concerned with what happens to the information.

Note: currently, reusing target objects is not generally safe - perhaps they
should be reference-counted.

\class MessageDialogTarget
\brief MessageDialogTarget is a CommandOutputTarget that sends its status
to the LongMessageDialog.

*//*******************************************************************/
#ifndef __AUDACITY_WX_COMMANDTARGETS__
#define __AUDACITY_WX_COMMANDTARGETS__

#include "CommandTargets.h"

#include <memory>
#include <vector>
#include <wx/string.h>
#include <wx/thread.h>

class wxStatusBar;

#if 0

//#include "ProgressDialog.h" // Member variable

/// Sends command progress information to a ProgressDialog
class GUIProgressTarget final : public CommandProgressTarget
{
private:
    ProgressDialog& mProgress;
public:
    GUIProgressTarget(ProgressDialog& pd)
        : mProgress(pd)
    {}
    ~GUIProgressTarget() override;
    void Update(double completed) override
    {
        mProgress.Update(completed);
    }
};
#endif

/// Displays messages from a command in a wxStatusBar
class AUDACITY_DLL_API StatusBarTarget final : public CommandMessageTarget
{
private:
    wxStatusBar& mStatus;
public:
    StatusBarTarget(wxStatusBar& sb)
        : mStatus(sb)
    {}
    ~StatusBarTarget() override;
    void Update(const wxString& message) override;
};

#endif /* End of include guard: __COMMANDTARGETS__ */
