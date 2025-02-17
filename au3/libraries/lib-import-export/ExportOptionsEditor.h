/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOptionsEditor.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include "ExportTypes.h"

namespace audacity {
class BasicSettings;
}

///\brief Editor objects are used to retrieve a set of export options,
///and configure exporting parameters according the plugin internal logic.
///Each option is assigned with internal index that should not change.
class IMPORT_EXPORT_API ExportOptionsEditor
{
public:
    using SampleRateList = std::vector<int>;

    ///\brief Listener object that is used to report on option changes.
    class IMPORT_EXPORT_API Listener
    {
    public:
        virtual ~Listener();

        ///\brief Called before `OnExportOptionChange`
        virtual void OnExportOptionChangeBegin() = 0;
        ///\brief Called after `OnExportOptionChange`
        virtual void OnExportOptionChangeEnd() = 0;
        ///\brief Called when option change
        virtual void OnExportOptionChange(const ExportOption& option) = 0;
        ///\brief Called when format extension change (usually in response parameter change)
        virtual void OnFormatInfoChange() = 0;

        virtual void OnSampleRateListChange() = 0;
    };

    virtual ~ExportOptionsEditor();

    virtual int GetOptionsCount() const = 0;
    virtual bool GetOption(int index, ExportOption& option) const = 0;

    virtual bool GetValue(ExportOptionID id, ExportValue& value) const = 0;
    virtual bool SetValue(ExportOptionID id, const ExportValue& value) = 0;

    virtual SampleRateList GetSampleRateList() const = 0;

    virtual void Store(audacity::BasicSettings& settings) const = 0;
    virtual void Load(const audacity::BasicSettings& config) = 0;
};
