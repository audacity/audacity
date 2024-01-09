/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AcidizerTagSerialization.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AcidizerTagSerialization.h"

#include <rapidjson/document.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/writer.h>

namespace LibImportExport
{
using namespace LibFileFormats;

std::string AcidizerTagsToString(const AcidizerTags& tags)
{
   rapidjson::StringBuffer buffer;
   rapidjson::Writer<rapidjson::StringBuffer> writer { buffer };
   writer.StartObject();
   writer.Key("bpm");
   writer.Double(tags.bpm.value_or(0.));
   writer.Key("isOneShot");
   writer.Bool(tags.isOneShot);
   writer.EndObject();
   return buffer.GetString();
}

std::optional<AcidizerTags> StringToAcidizerTags(const std::string& str)
{
   rapidjson::Document document;
   document.Parse(str.c_str());
   if (
      !document.IsObject() || !document.HasMember("bpm") ||
      !document.HasMember("isOneShot") || !document["bpm"].IsDouble() ||
      document["bpm"].GetDouble() < 0 || !document["isOneShot"].IsBool())
      return {};
   if (document["isOneShot"].GetBool())
      return AcidizerTags::OneShot {};
   else
      return AcidizerTags::Loop { document["bpm"].GetDouble() };
}
} // namespace LibImportExport
