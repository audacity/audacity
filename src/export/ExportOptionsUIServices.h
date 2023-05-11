/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOptionsUIServices.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

class ExportPlugin;
class ShuttleGui;

class ExportOptionsUIServices
{
public:
   
   virtual ~ExportOptionsUIServices();

   virtual void PopulateUI(ShuttleGui&) = 0;
   virtual void TransferDataFromWindow() = 0;
};
