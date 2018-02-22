/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file AutomationCommands.h
\brief Contains declaration of AutomationCommands class.

\class AutomationCommands
\brief Command which outputs a list of available menu commands on the status
channel.

*//*******************************************************************/

#ifndef __GETALLMENUCOMMANDS__
#define __GETALLMENUCOMMANDS__

#include "Command.h"
#include "CommandType.h"

class wxMenuBar;
class wxPoint;

class AutomationCommandsType final : public CommandType
{
public:
   AutomationCommandsType( const wxString & Name ) { mCustomName = Name;};
   wxString mCustomName;
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class AutomationCommands final : public CommandImplementation
{
public:
   AutomationCommands(CommandType &type,
                      std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { mMode = type.BuildName();}

   virtual ~AutomationCommands()
   { }
   bool Apply(CommandExecutionContext context) override;

private:
   wxString mMode;
   bool SendMenus(CommandExecutionContext context);
   bool SendMenusPlus(CommandExecutionContext context);

   bool SendClips(CommandExecutionContext context);
   bool SendKeycodes(CommandExecutionContext context);
   bool SendBoxes(CommandExecutionContext context);
   void ExploreMenu( wxMenu * pMenu, int Id, int depth );
   void ExploreTrackPanel( CommandExecutionContext context,
      wxPoint P, wxWindow * pWin, int Id, int depth );
   void ExploreAdornments( CommandExecutionContext context,
      wxPoint P, wxWindow * pWin, int Id, int depth );

   void ExploreWindows( CommandExecutionContext context,
      wxPoint P, wxWindow * pWin, int Id, int depth );

};

#endif /* End of include guard: __GETALLMENUCOMMANDS__ */
