/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   ModelCard.cpp
   Hugo Flores Garcia

******************************************************************/

#include "ModelCard.h"

#include <optional> 

#include <wx/string.h>
#include <wx/datetime.h>
#include <wx/file.h>
#include <wx/log.h>

#include <rapidjson/document.h>
#include <rapidjson/ostreamwrapper.h>
#include <rapidjson/filewritestream.h>
#include <rapidjson/writer.h>
#include <rapidjson/filereadstream.h>
#include <rapidjson/error/en.h>

#include "FileException.h"
#include "CodeConversions.h"

namespace validators 
{
   bool validateExists(const std::string &key, const Doc& doc, bool required)
   {
      if(!doc.IsObject())
      {
         if (required)
            throw InvalidModelCardDocument(XO("The provided JSON document is not an object."));

         return false;
      }
      if(!doc.HasMember(key.c_str()))
      {
         if (required)
            throw InvalidModelCardDocument(
               XO("JSON document missing field: %s").Format(wxString(key)));
         
         return false;
      }

      return true;
   }

   void throwTypeError(const std::string &key, const char *type, const Doc& doc)
   {
      throw InvalidModelCardDocument(
         XO("JSON field %s is not of type %s").Format(wxString(key), wxString(type)));
   }

   std::string toLowerCase(const std::string &string)
   {
      return audacity::ToUTF8(wxString(string).Lower());
   }

   std::optional<std::string> tryGetString(const std::string &key, const Doc& doc, bool required)
   {
      if (!validateExists(key, doc, required))
         return std::nullopt;

      if (!doc[key.c_str()].IsString())
      {
         if (required)
            throwTypeError(key, "string", doc);
         else
            return std::nullopt;
      }

      return doc[key.c_str()].GetString();
   }

   std::optional<int> tryGetInt(const std::string &key, const Doc& doc, bool required)
   {
      if (!validateExists(key, doc, required))
         return std::nullopt;
      if (!doc[key.c_str()].IsInt())
      {
         if (required)
            throwTypeError(key, "int", doc);
         else
            return std::nullopt;
      }

      return doc[key.c_str()].GetInt();
   }

   std::optional<bool> tryGetBool(const std::string &key, const Doc& doc, bool required)
   {
      if (!validateExists(key, doc, required))
         return std::nullopt;
         
      if (!doc[key.c_str()].IsBool())
      {
         if (required)
            throwTypeError(key, "bool", doc);
         else
            return std::nullopt;
      }

      return doc[key.c_str()].GetBool();
   }

   std::vector<std::string> tryGetStringArray(const std::string &key, const Doc& doc, bool lower)
   {
      validateExists(key, doc, true);
      if(!doc[key.c_str()].IsArray())
         throwTypeError(key, "array", doc);
      
      std::vector<std::string> labels;
      for (auto itr = doc[key.c_str()].Begin(); itr != doc[key.c_str()].End();  ++itr)
      {
         if (lower)
            labels.emplace_back(toLowerCase(itr->GetString()));
         else
            labels.emplace_back(itr->GetString());
      }

      return labels;
   }

   std::optional<uint64_t> tryGetUint64(const std::string &key, const Doc& doc, bool required)
   {
      if (!validateExists(key, doc, required))
         return std::nullopt;

      if(!doc[key.c_str()].IsUint64())
         throwTypeError(key, "uint64", doc);
      
      return doc[key.c_str()].GetUint64();
   }
}

namespace parsers 
{
   Doc ParseString(const std::string &data)
   {
      Doc d;
      // parse the data
      if (d.Parse(data.c_str()).HasParseError()) 
      {
         TranslatableString msg = XO("Error parsing JSON from string:\n%s\nDocument: %s ")
                                       .Format(wxString(rapidjson::GetParseError_En(d.GetParseError())), 
                                                wxString(data));
         throw InvalidModelCardDocument(msg);
      }
      
      return d;
   }

   Doc ParseFile(const std::string &path)
   {
      wxString docStr;
      wxFile file = wxFile(path);

      if(!file.ReadAll(&docStr))
         throw InvalidModelCardDocument(XO("Could not read file."));

      return ParseString(audacity::ToUTF8(docStr));
   }
}

// ModelCard Implementation

void ModelCard::Validate(const Doc& doc, const Doc& schema) const
{
   rapidjson::SchemaDocument schemaDoc(schema);
   rapidjson::SchemaValidator validator(schemaDoc);

   if (!doc.Accept(validator)) 
   {
      // Input JSON is invalid according to the schema
      // Output diagnostic information
      TranslatableString msg = XO(
         "A schema violation was found in the Model Card.\n "
         "- violation found in URI: %s\n"
         "- schema field violated: %s\n"
         "- invalid document: %s\n"
         "- schema document: %s\n"
      );

      rapidjson::StringBuffer sb;

      validator.GetInvalidSchemaPointer().StringifyUriFragment(sb);
      wxString uri(sb.GetString());
      sb.Clear();

      wxString invalidSchemaField(validator.GetInvalidSchemaKeyword());

      rapidjson::Writer<rapidjson::StringBuffer> writer(sb);
      doc.Accept(writer);
      wxString invalidDocument(sb.GetString());
      sb.Clear();
   
      rapidjson::Writer<rapidjson::StringBuffer> swriter(sb);
      schema.Accept(swriter);
      wxString schemaDocument(sb.GetString());

      throw InvalidModelCardDocument(msg.Format(uri, invalidSchemaField, 
                                                invalidDocument, schemaDocument));
   }
}

void ModelCard::SerializeToFile(const std::string &path) const
{
   rapidjson::StringBuffer sb;
   rapidjson::Writer<rapidjson::StringBuffer> writer(sb);

   writer.StartObject();
   Serialize(writer);
   writer.EndObject();

   wxFile file(path, wxFile::write);

   TranslatableString msg = FileException::WriteFailureMessage(wxFileName(path));

   if (!file.Write(wxString(sb.GetString())))
      throw InvalidModelCardDocument(XO("Could not serialize ModelCard to file. \n Message: %s").Format(wxString(path), msg));
}

void ModelCard::DeserializeFromFile(const std::string &path, const Doc& schema)
{
   Doc d = parsers::ParseFile(path);
   Deserialize(d, schema);
}

template < typename Writer >
void ModelCard::Serialize(Writer &writer) const
{
   // note: the fields "is_local" and "local_path"
   // are not serialized because they are filled out
   // by the ModelManager when the ModelCard is loaded from disk. 

   // name
   writer.String("name");
   writer.String(m_name.c_str());

   // author
   writer.String("author");
   writer.String(m_author.c_str());
   
   // long description
   writer.String("long_description");
   writer.String(m_long_description.c_str());

   // short description
   writer.String("short_description");
   writer.String(m_short_description.c_str());

   // sample rate
   writer.String("sample_rate");
   writer.Int(m_sample_rate);

   // multichannel
   writer.String("multichannel");
   writer.Bool(m_multichannel);

   // effect type
   writer.String("effect_type");
   writer.String(m_effect_type.c_str());

   // domain tags
   writer.String("domain_tags");
   writer.StartArray();
   for (auto tag : m_domain_tags)
      writer.String(tag.c_str());
   writer.EndArray();

   // tags
   writer.String("tags");
   writer.StartArray();
   for (auto tag : m_tags)
      writer.String(tag.c_str());
   writer.EndArray();

   // labels
   writer.String("labels");
   writer.StartArray();
   for(auto label : m_labels)
      writer.String(label.c_str());
   writer.EndArray();

   // model size
   writer.String("model_size");
   writer.Uint64((uint64_t)m_model_size);

}

void ModelCard::Deserialize(const Doc& doc, const Doc& schema)
{
   using namespace validators;
   Validate(doc, schema);

   // note: the fields "is_local" and "local_path"
   // are not deseralized because they are filled out
   // by the ModelManager when the ModelCard is saved to / loaded from disk. 

   // these first three fields are not in HF metadata but rather added later,
   // so don't throw if they are not present (empty default values are given)
   m_name              = tryGetString( "name",              doc, false).value_or("");
   m_author            = tryGetString( "author",            doc, false).value_or("");

   m_long_description  = tryGetString( "long_description",  doc, false).value_or("no long description available");
   m_short_description = tryGetString( "short_description", doc, false).value_or("no short description available");

   // these default values are here so we can use value_or() to get the value
   // but the following fields (sample rate, multichannel, effect type) are 
   // considered as required, so they will throw if not present in the doc
   m_sample_rate       = tryGetInt(    "sample_rate",       doc, true).value_or(16000);
   m_multichannel      = tryGetBool(   "multichannel",      doc, true).value_or(false);
   m_effect_type       = toLowerCase(
                          tryGetString( "effect_type",      doc, true).value_or("waveform-to-waveform"));

   m_domain_tags       = tryGetStringArray("domain_tags",   doc, true);
   m_tags              = tryGetStringArray("tags",          doc, false);
   m_labels            = tryGetStringArray("labels",        doc, false);
   m_model_size        = (size_t)tryGetUint64("model_size", doc, false).value_or(0);
}

std::string ModelCard::GetRepoID() const
{
   return this->author() + '/' + this->name();
}

// ModelCardCollection implementation

bool ModelCardCollection::Find(ModelCardHolder card) const
{
   auto predicate = [&](const ModelCardHolder &a){
      return (*card) == (*a);
   };

   return std::find_if(this->begin(), this->end(), predicate) != this->end();
}

bool ModelCardCollection::Insert(ModelCardHolder card)
{  
   // only add it if its not already there
   if (!Find(card))
   {
      mCards.push_back(std::move(card));
      return true;
   }
   else
      return false;
}

ModelCardCollection ModelCardCollection::Filter(ModelCardFilter filter) const
{
   ModelCardCollection that;
   for (auto card : mCards)
   {
      if (card) // check for nullptr
      {
         // only insert if filter returns true
         if (filter(*card))
            that.Insert(card);
      }
   }
   return that;
}
