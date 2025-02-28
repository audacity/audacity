/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPlugin.h

  Dominic Mazzoni

**********************************************************************/
#pragma once

#include <memory>

#include <rapidjson/fwd.h>

#include <wx/string.h>

#include "ExportOptionsEditor.h"
#include "wxArrayStringEx.h"
#include "Identifier.h"
#include "TranslatableString.h"
#include "wxFileNameWrapper.h"

class wxFileName;
class wxString;

class AudacityProject;
class Tags;

namespace MixerOptions {
class Downmix;
}

struct IMPORT_EXPORT_API FormatInfo final
{
    wxString format;
    TranslatableString description;
    FileExtensions extensions;
    unsigned maxChannels;
    bool canMetaData;
};

class IMPORT_EXPORT_API ExportException
{
    const wxString mMessage;
public:

    ExportException(const wxString& msg);

    const wxString& What() const noexcept;
};

class IMPORT_EXPORT_API ExportErrorException
{
    TranslatableString mMessage;
    wxString mHelpPageId;
public:
    /// We have many Export errors that are essentially anonymous
    /// and are distinguished only by an error code number.
    /// Rather than repeat the code, we have it just once.
    ExportErrorException(const wxString& errorCode);
    //
    ExportErrorException(TranslatableString message, const wxString& helpPage);

    const TranslatableString& GetMessage() const noexcept;
    const wxString& GetHelpPageId() const noexcept;
};

class IMPORT_EXPORT_API ExportDiskFullError
{
    wxFileNameWrapper mFileName;
public:
    ExportDiskFullError(const wxFileNameWrapper& fileName);

    const wxFileNameWrapper& GetFileName() const noexcept;
};

class IMPORT_EXPORT_API ExportProcessorDelegate
{
public:
    virtual ~ExportProcessorDelegate();

    virtual bool IsCancelled() const = 0;
    virtual bool IsStopped() const = 0;
    virtual void SetStatusString(const TranslatableString& str) = 0;
    virtual void OnProgress(double progress) = 0;
};

class IMPORT_EXPORT_API ExportProcessor
{
public:
    using Parameters = std::vector<std::tuple<ExportOptionID, ExportValue> >;

    ExportProcessor(const ExportProcessor&) = delete;
    ExportProcessor& operator=(const ExportProcessor&) = delete;

    ExportProcessor() = default;
    virtual ~ExportProcessor();

    /**
     * @brief Called before start processing.
     *
     * @param project Processor may access project data, take care to exclude any data race
     * @param parameters A format-dependent set of parameters used in exporting
     * @param selectedOnly Set to true if all tracks should be mixed, to false
     * if only the selected tracks should be mixed and exported.
     * @param tags A Tags object that will over-ride the one in *project and
     * be used to tag the file that is exported.
     * @retern Implementations may simply return false without any error reporting.
     * This is to temporarily preserve old behavior, which is to be removed in the
     * nearest future.
     */
    virtual bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                            bool selectedOnly, double rate, unsigned channels, MixerOptions::Downmix* mixerSpec = nullptr,
                            const Tags* tags = nullptr) = 0;

    virtual ExportResult Process(ExportProcessorDelegate& delegate) = 0;
};

//----------------------------------------------------------------------------
// ExportPlugin
//----------------------------------------------------------------------------
class IMPORT_EXPORT_API ExportPlugin /* not final */
{
public:

    ExportPlugin();
    virtual ~ExportPlugin();

    virtual int GetFormatCount() const = 0;
    /**
     * \brief Returns FormatInfo structure for given index if it's valid,
     * or a default one.
     * FormatInfo::format isn't guaranteed to be unique.
     * \param index Should not exceed the number of formats
     * provided by GetFormatCount()
     */
    virtual FormatInfo GetFormatInfo(int index) const = 0;

    /**
     * @brief Creates format-dependent options editor, that is used to create
     * a valid set of parameters to be used in exporting.
     *
     * @param listener Option listener object that could be used by the editor
     * to report on option changes.
     */
    virtual std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int formatIndex, ExportOptionsEditor::Listener* listener) const = 0;

    /// \return Mime type(s) supported by the format.
    virtual std::vector<std::string> GetMimeTypes(int formatIndex) const;

    /**
     * @brief Attempt to parse configuration JSON object and produce
     * a suitable set of parameters. Configuration is format dependent.
     *
     * @param formatIndex Internal format index
     * @param config Configuration JSON object
     * @param parameters Where to put parameters
     * @return Whether the parsing was successful
     **/
    virtual bool ParseConfig(int formatIndex, const rapidjson::Value& config, ExportProcessor::Parameters& parameters) const;

    virtual bool CheckFileName(wxFileName& filename, int format = 0) const;

    /**
     * @param format Control which of the multiple formats this exporter is
     * capable of exporting should be used. Used where a single export plug-in
     * handles a number of related formats, but they have separate
     * entries in the Format drop-down list box. For example, the options to
     * export to "Other PCM", "AIFF 16 Bit" and "WAV 16 Bit" are all the same
     * libsndfile export plug-in, but with subformat set to 0, 1, and 2
     * respectively.
     **/
    virtual std::unique_ptr<ExportProcessor> CreateProcessor(int format) const = 0;
};
