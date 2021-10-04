/**********************************************************************
  Audacity: A Digital Audio Editor
  ResetConfig.h
  Jithin John
**********************************************************************/

#ifndef __AUDACITY_RESETCONFIG__
#define __AUDACITY_RESETCONFIG__

class wxWindow;
class AudacityProject;

AUDACITY_DLL_API
void RunResetConfig( wxWindow *parent, AudacityProject &project, const CommandContext &context);

#endif // define __AUDACITY_RESETCONFIG__