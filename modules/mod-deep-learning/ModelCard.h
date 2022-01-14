/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   ModelCard.h
   Hugo Flores Garcia

******************************************************************/
/**

\file ModelCard.h
\brief ModelCard provides a class and utilities for deep learning model metadata.

*/
/*******************************************************************/

#pragma once

#include <torch/script.h>
#include <torch/torch.h>

#include <rapidjson/document.h>
#include <rapidjson/schema.h>
#include "rapidjson/prettywriter.h" 
#include <rapidjson/writer.h>
#include <wx/string.h>
#include <wx/log.h>

#include "MemoryX.h"
#include "AudacityException.h"

using Doc = rapidjson::Document;

// this exception should be caught internally, but we 
// derive from MessageBoxException just in case it needs to 
// get handled by Audacity
class InvalidModelCardDocument final : public MessageBoxException
{
public:
   InvalidModelCardDocument(const TranslatableString msg): 
                           m_msg(std::move(msg)),
                            MessageBoxException {
                               ExceptionType::Internal,
                               XO("Invalid Model Card Document")
                            }
   { 
   }

   // detailed internal error message
   virtual const char* what() const throw () 
   {
      wxLogError(m_msg.Translation());
      // TODO: also print the document
      return m_msg.Translation().c_str();
   }

   // user facing message
   virtual TranslatableString ErrorMessage() const
      { return XO("Model Card Error: %s")
               .Format(m_msg);}

   const TranslatableString m_msg;
};

namespace parsers 
{
   Doc ParseString(const std::string &json);
   Doc ParseFile(const std::string &path);
}

class DeepModelManager;

class ModelCard final
{
public: 
   ModelCard() = default;
   ModelCard(const ModelCard&) = default;

   //! returns {author}/{name}
   std::string GetRepoID() const;

   //! check if two ModelCards have the same RepoID
   //! only GetRepoID() is meant to be significant in comparison, 
   //! as this function is used to determine if two ModelCards point
   //! to the same repository. TODO: add versioning
   bool operator==(const ModelCard& that) const { return GetRepoID() == that.GetRepoID(); }
   bool operator!=(const ModelCard& that) const { return !((*this) == that); }
   bool operator<(const ModelCard& that) const { return GetRepoID() < that.GetRepoID(); }
   bool operator>(const ModelCard& that) const { return GetRepoID() > that.GetRepoID(); }

   //! same as operator==
   bool IsSame(const ModelCard& other) const { return (*this) == other; }

   //! throws InvalidModelCardDocument if the given json is not valid. 
   void DeserializeFromFile(const std::string& path, const Doc& schema);
   void SerializeToFile(const std::string& path) const;

   template < typename Writer >
   void Serialize(Writer &writer) const;
   //! throws InvalidModelCardDocument if the given json is not valid. 
   void Deserialize(const Doc& doc, const Doc& schema);

private:
   void Validate(const Doc& doc, const Doc& schema) const;

public:

   bool local() const { return m_is_local; }
   ModelCard &local(bool local) { m_is_local = local; return *this;}

   std::string local_path() const { return m_local_path; }
   ModelCard &local_path(const std::string& path) { m_local_path = path; return *this;}

   std::string name() const { return m_name; }
   ModelCard &name(const std::string &name) { m_name = name; return *this;}

   std::string author() const { return m_author; }
   ModelCard &author(const std::string &author) { m_author = author; return *this;}

   std::string long_description() const { return m_long_description; }
   ModelCard &long_description(const std::string &long_description) { m_long_description = long_description; return *this;}

   std::string short_description() const { return m_short_description; }
   ModelCard &short_description(const std::string &short_description) { m_short_description = short_description; return *this;}

   int sample_rate() const { return m_sample_rate; }
   ModelCard &sample_rate(int rate) { m_sample_rate = rate; return *this;}

   bool multichannel() const { return m_multichannel; }
   ModelCard &multichannel(bool multichannel) { m_multichannel = multichannel; return *this;}

   std::string effect_type() const { return m_effect_type; }
   ModelCard &effect_type(const std::string &type) { m_effect_type = type; return *this;}

   const std::vector<std::string> &domain_tags() const { return m_domain_tags; }
   ModelCard &domain_tags(const std::vector<std::string> tags) { m_domain_tags = tags; return *this;}

   const std::vector<std::string> &tags() const { return m_tags; }
   ModelCard &tags(const std::vector<std::string> tags) { m_tags = tags; return *this;}

   const std::vector<std::string> &labels() const { return m_labels; }
   ModelCard &labels(std::vector<std::string> labels) { m_labels = labels; return *this;} // use std::move to not copy

   size_t model_size() const { return m_model_size; }
   ModelCard &model_size(size_t model_size) { m_model_size = model_size; return *this;}

private:
   std::string m_name {""};
   std::string m_author {""};
   std::string m_long_description {""};
   std::string m_short_description {""};
   int m_sample_rate {0};
   bool m_multichannel  {false};
   std::string m_effect_type {""};
   std::vector<std::string> m_domain_tags;
   std::vector<std::string> m_tags;
   std::vector<std::string> m_labels;
   size_t m_model_size {0};

   bool m_is_local {false};
   std::string m_local_path {""};

};

using ModelCardHolder = std::shared_ptr<ModelCard>;
using ModelCardFilter = std::function<bool(const ModelCard& card)>;
using ModelCardComparison = std::function<bool(const ModelCard &a, const ModelCard &b)>;

class ModelCardCollection final
{
public:
   ModelCardCollection() = default;

   //! only inserts if the card isn't already there (doesn't throw)
   bool Insert(ModelCardHolder card);

   //! returns a view of a subset as dictated by the filter
   ModelCardCollection Filter(ModelCardFilter filter) const;

   //! returns a sorted view of the cards
   ModelCardCollection Sort(ModelCardComparison cmp) const;

   //! iterators 
   ModelCardHolder &operator[](size_t i) {return mCards[i];}
   std::vector<ModelCardHolder>::iterator begin() {return mCards.begin();}
   std::vector<ModelCardHolder>::iterator end() {return mCards.end();}
   size_t Size() const {return mCards.size();}

   //! determine whether a card is already in the collection
   bool Find(ModelCardHolder card) const;

private:
   std::vector<ModelCardHolder> mCards;

};
