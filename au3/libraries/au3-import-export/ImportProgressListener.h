/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportProgressListener.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

class TranslatableString;

class ImportFileHandle;

///\brief Interface used to report on import state and progress
class IMPORT_EXPORT_API ImportProgressListener
{
public:
    enum class ImportResult {
        Success,
        Error,
        Cancelled,
        Stopped
    };

    virtual ~ImportProgressListener();

    ///Called by Importer when it attempts to import file using registered ImportPlugin instance
    ///Could be called more than once, but for each call there will be a complementary call to OnImportResult
    ///\param importFileHandle file handle created by ImportPlugin instance
    ///\return Implementation may return false to abort import process
    virtual bool OnImportFileOpened(ImportFileHandle& importFileHandle) = 0;

    ///Used to report on import progress [optional]
    ///\param progress import progress in range [0, 1]
    virtual void OnImportProgress(double progress) = 0;

    ///Used to report on import result for file handle passed as argument to OnImportFileOpened
    virtual void OnImportResult(ImportResult result) = 0;
};
