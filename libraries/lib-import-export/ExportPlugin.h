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

//----------------------------------------------------------------------------
// ExportPlugin
//----------------------------------------------------------------------------
class IMPORT_EXPORT_API ExportPlugin /* not final */
{
public:

   using Parameters = std::vector<std::tuple<ExportOptionID, ExportValue>>;

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
   virtual bool ParseConfig(int formatIndex, const rapidjson::Value& config, Parameters& parameters) const;

   virtual bool CheckFileName(wxFileName &filename, int format = 0);

   /** \brief called to export audio into a file.
    *
    * @param delegate Used to report on export progress
    * @param parameters A format-dependent set of parameters used in exporting
    * @param selectedOnly Set to true if all tracks should be mixed, to false
    * if only the selected tracks should be mixed and exported.
    * @param metadata A Tags object that will over-ride the one in *project and
    * be used to tag the file that is exported.
    * @param subformat Control which of the multiple formats this exporter is
    * capable of exporting should be used. Used where a single export plug-in
    * handles a number of related formats, but they have separate
    * entries in the Format drop-down list box. For example, the options to
    * export to "Other PCM", "AIFF 16 Bit" and "WAV 16 Bit" are all the same
    * libsndfile export plug-in, but with subformat set to 0, 1, and 2
    * respectively.
    */
   virtual ExportResult Export(AudacityProject *project,
                       ExportPluginDelegate& delegate,
                       const Parameters& parameters,
                       unsigned channels,
                       const wxFileNameWrapper &fName,
                       bool selectedOnly, double t0, double t1,
                       MixerOptions::Downmix *mixerSpec = nullptr,
                       const Tags *metadata = nullptr,
                       int subformat = 0) = 0;
};
