/**********************************************************************

  Audacity: A Digital Audio Editor

  GenericExportProgressListener.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <memory>
#include "ExportProgressListener.h"

namespace BasicUI
{
class ProgressDialog;
}

class ExportPlugin;

class GenericExportProgressListener final : public ExportProgressListener
{
public:
   
   GenericExportProgressListener(ExportPlugin& plugin);
   ~GenericExportProgressListener() override;
   
   void OnExportProgress(double value) override;
   
   void OnExportResult(ExportResult result) override;
   
   ExportResult ConsumeResult() noexcept;
   
private:
   ExportPlugin& mPlugin;
   std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;
   ExportResult mResult{ExportResult::Error};
};
