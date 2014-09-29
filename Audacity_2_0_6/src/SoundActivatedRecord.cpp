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

#include <wx/dialog.h>

#include "Envelope.h"
#include "ShuttleGui.h"
#include "ShuttlePrefs.h"
#include "Prefs.h"
#include "SoundActivatedRecord.h"

BEGIN_EVENT_TABLE(SoundActivatedRecord, wxDialog)
   EVT_BUTTON(wxID_OK, SoundActivatedRecord::OnOK)
END_EVENT_TABLE()

SoundActivatedRecord::SoundActivatedRecord(wxWindow* parent)
: wxDialog(parent, -1, _("Sound Activated Record"), wxDefaultPosition,
           wxDefaultSize, wxCAPTION )
//           wxDefaultSize, wxCAPTION | wxTHICK_FRAME)
{
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
         dBRange = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);
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
   EndModal(0);
}

