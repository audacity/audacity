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

class wxFileName;
class wxString;
class wxFileNameWrapper;

class AudacityProject;
class Tags;

namespace MixerOptions{ class Downmix; }


struct IMPORT_EXPORT_API FormatInfo
{
   wxString mFormat;
   TranslatableString mDescription;
   FileExtensions mExtensions;
   unsigned mMaxChannels;
   bool mCanMetaData;
};


class IMPORT_EXPORT_API ExportException
{
   const wxString mMessage;
public:
   
   ExportException(const wxString& msg);

   const wxString& What() const noexcept;
};

class IMPORT_EXPORT_API ExportErrorCodeException : public ExportException
{
public:
   using ExportException::ExportException;
};

class IMPORT_EXPORT_API ExportDiskFullError : public ExportException
{
public:
   using ExportException::ExportException;
};

class IMPORT_EXPORT_API ExportErrorException : public ExportException
{
   const wxString mHelpPage;
public:
   ExportErrorException(const wxString& message, const wxString& helpPage);
   
   const wxString& GetHelpPage() const noexcept;

};

class IMPORT_EXPORT_API ExportPluginDelegate
{
public:
   virtual ~ExportPluginDelegate();

   virtual bool IsCancelled() const = 0;
   virtual bool IsStopped() const = 0;
   virtual void SetErrorString(const TranslatableString& str) = 0;
   virtual void SetStatusString(const TranslatableString& str) = 0;
   virtual void OnProgress(double progress) = 0;
};

class IMPORT_EXPORT_API ExportProcessor
{
public:
   using Parameters = std::vector<std::tuple<ExportOptionID, ExportValue>>;

   ExportProcessor(const ExportProcessor&) = delete;
   ExportProcessor& operator=(const ExportProcessor&) = delete;

   ExportProcessor() = default;
   virtual ~ExportProcessor();
   
   virtual void Initialize(AudacityProject& project,
      const Parameters& parameters,
      const wxFileNameWrapper& filename,
      double t0, double t1, bool selectedOnly,
      unsigned channels,
      MixerOptions::Downmix* mixerSpec = nullptr,
      const Tags* tags = nullptr) = 0;

   virtual ExportResult Process(ExportPluginDelegate& delegate) = 0;
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
   virtual FormatInfo GetFormatInfo(int index) const = 0;
   
   /** \brief Creates format-dependent options editor, that is used to create
    * a valid set of parameters to be used in exporting.
    *
    * \param listener Option listener object that could be used by the editor
    * to report on option changes.
    */
   virtual std::unique_ptr<ExportOptionsEditor>
   CreateOptionsEditor(int formatIndex, ExportOptionsEditor::Listener* listener) const = 0;
 
   /// \return Mime type(s) supported by the format.
   virtual std::vector<std::string> GetMimeTypes(int formatIndex) const;

   ///\brief Attempt to parse configuration JSON object and produce
   ///a suitable set of parameters. Configuration is format dependent.
   ///\param formatIndex Internal format index
   ///\param config Configuration JSON object
   ///\param parameters Where to put parameters
   ///\return Whether the parsing was successful
   virtual bool ParseConfig(int formatIndex, const rapidjson::Value& config, ExportProcessor::Parameters& parameters) const;

   virtual bool CheckFileName(wxFileName &filename, int format = 0) const;

   virtual std::unique_ptr<ExportProcessor> CreateProcessor(int format) const = 0;
};
