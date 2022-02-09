/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VSTUtils.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <string>
#include <pluginterfaces/vst/vsttypes.h>

class wxString;
class wxWindow;

namespace Steinberg
{
   namespace Vst
   {
      class IEditController;
      class IComponentHandler;
      struct ParameterInfo;
   }
}

/**
 * \brief Provides a set of useful functions, used across the Audacity VST3 module
 */
class VST3Utils final
{
public:

   //Creates a plugin path string, which can be used to uniquely identify the effect.
   //modulePath - path to the vst3 file/catalog
   //effectUIDString - the effect's VST3::UID converted to a string
   static wxString MakePluginPathString(const wxString& modulePath, const std::string& effectUIDString);

   //Attempts to parse plugin path string, returns true if string is considered
   //to be a valid plugin path. modulePath and effectUIDString are written if
   //provided.
   static bool ParsePluginPath(const wxString& pluginPath, wxString* modulePath, std::string* effectUIDString);

   /*! Builds a "plain" plugin interface with wx components.
    * editController is used to extract current values from
    * the parameters, and handler is used to update them
    * \param parent Where all parameter controls will be placed (not null)
    * \param editController Effect controller (not null)
    * \param handler Where to report parameter changes (not null) 
    */
   static void BuildPlainUI(
      wxWindow* parent,
      Steinberg::Vst::IEditController* editController,
      Steinberg::Vst::IComponentHandler* handler);

   static wxString ToWxString(const Steinberg::Vst::TChar* str);

};
