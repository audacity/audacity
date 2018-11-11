/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SetProjectCommand.cpp
\brief Definitions for SetProjectCommand

\class SetProjectCommand
\brief Command that sets project information

*//*******************************************************************/

#include "../Audacity.h"
#include "SetProjectCommand.h"

#include "../Project.h"
#include "../Track.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"
#include "../toolbars/SelectionBar.h"

SetProjectCommand::SetProjectCommand()
{
}


bool SetProjectCommand::DefineParams( ShuttleParams & S ){ 
   S.OptionalN( bHasName        ).Define(     mName,        wxT("Name"),       _("Project") );
   S.OptionalN( bHasRate        ).Define(     mRate,        wxT("Rate"),       44100.0, 1.0, 1000000.0);
   S.OptionalY( bHasSizing      ).Define(     mPosX,        wxT("X"),          10.0, 0.0, 2000.0);
   S.OptionalY( bHasSizing      ).Define(     mPosY,        wxT("Y"),          10.0, 0.0, 2000.0);
   S.OptionalY( bHasSizing      ).Define(     mWidth,       wxT("Width"),      1000.0, 200.0, 4000.0);
   S.OptionalY( bHasSizing      ).Define(     mHeight,      wxT("Height"),      900.0, 200.0, 4000.0);
   return true;
};

void SetProjectCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);
   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasName      ).TieTextBox(         _("Name:"),     mName );
      S.Optional( bHasRate      ).TieTextBox(         _("Rate:"),     mRate );
      S.TieCheckBox( _("Resize:"), bHasSizing    );
      S.AddSpace(0,0);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieNumericTextBox(  _("X:"),        mPosX );
      S.TieNumericTextBox(  _("Y:"),        mPosY );
      S.TieNumericTextBox(  _("Width:"),    mWidth );
      S.TieNumericTextBox(  _("Height:"),   mHeight );
   }
   S.EndMultiColumn();
}

bool SetProjectCommand::Apply(const CommandContext & context)
{
   AudacityProject * pProj = context.GetProject();
   if( bHasName )
      pProj->SetLabel(mName);

   if( bHasRate && mRate >= 1 && mRate <= 1000000 )
   {
      auto *bar = pProj->GetSelectionBar();
      if( bar ){
         bar->SetRate( mRate );
      }
   }

   if( bHasSizing )
   {
      pProj->SetPosition( wxPoint( mPosX, mPosY));
      pProj->SetSize( wxSize( mWidth, mHeight ));
   }
   return true;
}
