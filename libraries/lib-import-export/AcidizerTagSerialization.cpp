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
std::string AcidizerTagsToString(const LibFileFormats::AcidizerTags& tags)
{
   rapidjson::StringBuffer buffer;
   rapidjson::Writer<rapidjson::StringBuffer> writer { buffer };
   writer.StartObject();
   writer.Key("bpm");
   writer.Double(tags.bpm);
   writer.Key("isOneShot");
   writer.Bool(tags.isOneShot);
   writer.EndObject();
   return buffer.GetString();
}

std::optional<LibFileFormats::AcidizerTags>
StringToAcidizerTags(const std::string& str)
{
   rapidjson::Document document;
   document.Parse(str.c_str());
   if (
      !document.IsObject() || !document.HasMember("bpm") ||
      !document.HasMember("isOneShot") || !document["bpm"].IsDouble() ||
      !document["isOneShot"].IsBool())
      return {};
   return LibFileFormats::AcidizerTags { document["bpm"].GetDouble(),
                                         document["isOneShot"].GetBool() };
}
} // namespace LibImportExport
