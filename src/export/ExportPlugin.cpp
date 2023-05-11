/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPlugin.cpp

  Dominic Mazzoni

**********************************************************************/

#include "ExportPlugin.h"
#include "ShuttleGui.h"

ExportPlugin::ExportPlugin() = default;
ExportPlugin::~ExportPlugin() = default;

bool ExportPlugin::CheckFileName(wxFileName&, int)
{
  return true;
}

void ExportPlugin::OptionsCreate(ShuttleGui &S, int WXUNUSED(format))
{
   S.StartHorizontalLay(wxCENTER);
   {
      S.StartHorizontalLay(wxCENTER, 0);
      {
         S.Prop(1).AddTitle(XO("No format specific options"));
      }
      S.EndHorizontalLay();
   }
   S.EndHorizontalLay();
}
