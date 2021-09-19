/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file SetClipCommand.h
\brief Declarations of SetClipCommand and SetClipCommandType classes

*//*******************************************************************/

#ifndef __SET_CLIP_COMMAND__
#define __SET_CLIP_COMMAND__

#include "SetTrackInfoCommand.h"

class SetClipCommand : public SetTrackBase
{
public:
   static const ComponentInterfaceSymbol Symbol;

   SetClipCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;};
   TranslatableString GetDescription() override {return XO("Sets various values for a clip.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   ManualPageID ManualPage() override {return L"Extra_Menu:_Scriptables_I#set_clip";}
   bool ApplyInner( const CommandContext & context, Track * t ) override;

public:
   double mContainsTime;
   int mColour;
   double mT0;
   wxString mName;

// For tracking optional parameters.
   bool bHasContainsTime;
   bool bHasColour;
   bool bHasT0;
   bool bHasName;
};


#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
