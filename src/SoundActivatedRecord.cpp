/**********************************************************************

  Audacity: A Digital Audio Editor

  SoundActivatedRecord.cpp

  Martyn Shaw

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

********************************************************************//**

\class SoundActivatedRecord
\brief Configures sound activated recording.

*//********************************************************************/

#include "Audacity.h"
#include "SoundActivatedRecord.h"

#include "ShuttleGui.h"
#include "ShuttlePrefs.h"
#include "Prefs.h"
#include "prefs/GUISettings.h"

BEGIN_EVENT_TABLE(SoundActivatedRecord, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, SoundActivatedRecord::OnOK)
END_EVENT_TABLE()

SoundActivatedRecord::SoundActivatedRecord(wxWindow* parent)
: wxDialogWrapper(parent, -1, _("Sound Activated Record"), wxDefaultPosition,
           wxDefaultSize, wxCAPTION )
//           wxDefaultSize, wxCAPTION | wxTHICK_FRAME)
{
   SetName(GetTitle());
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   Fit();
   Center();
}

SoundActivatedRecord::~SoundActivatedRecord()
{
}

void SoundActivatedRecord::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   int dBRange;

   S.StartVerticalLay();
   {
      S.StartMultiColumn(2, wxEXPAND);
         S.SetStretchyCol(1);
         dBRange = gPrefs->Read(ENV_DB_KEY, ENV_DB_RANGE);
         S.TieSlider(_("Activation level (dB):"), wxT("/AudioIO/SilenceLevel"), -50, 0, -dBRange);
      S.EndMultiColumn();
   }
   S.EndVerticalLay();
   S.AddStandardButtons();
}

void SoundActivatedRecord::OnOK(wxCommandEvent & WXUNUSED(event))
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   gPrefs->Flush();

   EndModal(0);
}

