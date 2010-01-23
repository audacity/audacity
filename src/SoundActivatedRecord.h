/**********************************************************************

  Audacity: A Digital Audio Editor

  SoundActivatedRecord.cpp

  Martyn Shaw

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

********************************************************************/

class SoundActivatedRecord : public wxDialog
{
public:
   SoundActivatedRecord(wxWindow* parent);
   ~SoundActivatedRecord();

private:
   void OnOK(wxCommandEvent& event);

   void PopulateOrExchange(ShuttleGui& S);

   DECLARE_EVENT_TABLE();
};

