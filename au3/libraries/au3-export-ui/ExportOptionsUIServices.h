/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOptionsUIServices.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

class ShuttleGui;

class EXPORT_UI_API ExportOptionsUIServices
{
public:

    virtual ~ExportOptionsUIServices();

    virtual void PopulateUI(ShuttleGui&) = 0;
    virtual bool TransferDataFromWindow() = 0;
};
