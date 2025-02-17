/**********************************************************************

  Audacity: A Digital Audio Editor

  SoundActivatedRecord.cpp

  Martyn Shaw

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

********************************************************************/

#ifndef __AUDACITY_SOUND_ACTIVATED_RECORD__
#define __AUDACITY_SOUND_ACTIVATED_RECORD__

#include "wxPanelWrapper.h" // to inherit

class ShuttleGui;

class SoundActivatedRecordDialog final : public wxDialogWrapper
{
public:
    SoundActivatedRecordDialog(wxWindow* parent);
    ~SoundActivatedRecordDialog();

private:
    void OnOK(wxCommandEvent& event);

    void PopulateOrExchange(ShuttleGui& S);

    DECLARE_EVENT_TABLE()
};

#endif
