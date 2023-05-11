/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOptionsEditor.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include "ExportTypes.h"

class wxConfigBase;

class ExportOptionsEditor
{
public:
   
   class Listener
   {
   public:
      virtual ~Listener();
      virtual void OnExportOptionChangeBegin() = 0;
      virtual void OnExportOptionChangeEnd() = 0;
      virtual void OnExportOptionChange(const ExportOption& option) = 0;

      virtual void OnExtensionChange(const wxString& extension) = 0;
   };

   virtual ~ExportOptionsEditor();

   virtual int GetOptionsCount() const = 0;
   virtual bool GetOption(int index, ExportOption& option) const = 0;
   
   virtual bool GetValue(ExportOptionID id, ExportValue& value) const = 0;
   virtual bool SetValue(ExportOptionID id, const ExportValue& value) = 0;

   virtual void Store(wxConfigBase& config) const = 0;
   virtual void Load(const wxConfigBase& config) = 0;
};
