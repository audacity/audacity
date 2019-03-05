/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file DragCommand.h
\brief Declarations of DragCommand and DragCommandType classes

*//*******************************************************************/

#ifndef __DRAG_COMMAND__
#define __DRAG_COMMAND__

#include "Command.h"
#include "CommandType.h"

#define DRAG_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Drag") }

class DragCommand : public AudacityCommand
{
public:
   DragCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return DRAG_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Drags mouse from one place to another.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#move_mouse");};

   bool Apply(const CommandContext & context) override;

public:
   double mFromX;
   double mFromY;
   double mToX;
   double mToY;
   int mRelativeTo;
   int mId;
   wxString mWinName;

   bool bHasFromX;
   bool bHasFromY;
   bool bHasToX;
   bool bHasToY;
   bool bHasRelativeTo;
   bool bHasId;
   bool bHasWinName;

};


#endif /* End of include guard: __DRAG_COMMAND__ */
