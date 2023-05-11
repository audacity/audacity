/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportProgressListener.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

///\brief Used by `ExportPlugin` to report on export progress and status changes
class AUDACITY_DLL_API ExportProgressListener
{
public:
   enum class ExportResult
   {
      Success,
      Error,
      Cancelled,
      Stopped
   };
   
   virtual ~ExportProgressListener();
   
   ///\brief Called when progress advances [optional]
   ///\param value progress value in range [0, 1]]
   virtual void OnExportProgress(double value) = 0;
   ///\brief Reports export status change, called only once
   virtual void OnExportResult(ExportResult result) = 0;
};
