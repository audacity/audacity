/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MuseHubService.cpp

  Dmitry Makarenko

**********************************************************************/
#include "MuseHubService.h"

#include <optional>

#include <rapidjson/rapidjson.h>
#include <rapidjson/document.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/writer.h>

#include "NetworkManager.h"
#include "Request.h"
#include "IResponse.h"


namespace audacity::musehub
{

static const std::string becomeAPartnerUrl = "https://developer.musehub.com/muse-partners-help/introduction/becoming-a-muse-partner";
static const std::string musehubAPIEndpointUrl = "https://cosmos-customer-webservice.azurewebsites.net/graphql";
static const std::string musehubAPIDevEndpointUrl = "https://cosmos-customer-webservice-dev.azurewebsites.net/graphql";
static const std::string musehubEffectUrl = "https://www.musehub.com/plugin/";

static const std::string musehubEffectUtmSource = "utm_source=au-app-get-fx-panel";
static const std::string musehubEffectUtmMediumPrefix = "utm_medium=";
static const std::string musehubEffectUtmCampaignPrefix = "utm_campaign=au-app-get-fx-mh-";

static const std::string getEffectsQuery = R"(
   query EffectsQuery($locale: String!) {
      product_pages_configuration {
         audacityPageSections {
            ... on ProductPageSectionDynamic {
               title(locale: { locale: $locale })
               productCards {
                  ... on ProductCardRegular {
                     iconImageUrl
                     product(locale: { locale: $locale }) {
                        ... on ProductPlugin {
                           code
                           title
                           subtitle
                           category
                        }
                     }
                  }
               }
            }
         }
      }
   }
)";

static std::optional<EffectInfo> parseEffect(const rapidjson::Value& effectObj) {
   if (!effectObj.HasMember("product") ||
       !effectObj.HasMember("iconImageUrl") ||
       !effectObj["product"].HasMember("code") ||
       !effectObj["product"].HasMember("title") ||
       !effectObj["product"].HasMember("subtitle") ||
       !effectObj["product"].HasMember("category")) {
      assert(false);
      return std::nullopt;
   }

   return EffectInfo {
      effectObj["iconImageUrl"].GetString(),
      effectObj["product"]["code"].GetString(),
      effectObj["product"]["title"].GetString(),
      effectObj["product"]["subtitle"].GetString(),
      effectObj["product"]["category"].GetString(),
   };
}

static std::optional<EffectsGroup> parseEffectGroup(const rapidjson::Value& groupObj) {
   if (!groupObj.HasMember("title") ||
       !groupObj.HasMember("productCards") ||
       !groupObj["productCards"].IsArray()) {
      assert(false);
      return std::nullopt;
   }


   EffectsGroup group;
   group.title = groupObj["title"].GetString();

   const rapidjson::Value& cards = groupObj["productCards"];
   for (rapidjson::SizeType i = 0; i < cards.Size(); i++) {
      auto effect = parseEffect(cards[i]);
      if (effect)
         group.effects.push_back(*effect);
   }

   return group;
}

static std::vector<EffectsGroup> parseProductPages(const rapidjson::Document& doc) {
   if (!doc.HasMember("data") ||
       !doc["data"].HasMember("product_pages_configuration") ||
       !doc["data"]["product_pages_configuration"].HasMember("audacityPageSections")) {
      assert(false);
      return {};
   }

   std::vector<EffectsGroup> pages;

   const rapidjson::Value& sections = doc["data"]["product_pages_configuration"]["audacityPageSections"];

   for (rapidjson::SizeType i = 0; i < sections.Size(); i++) {
      auto group = parseEffectGroup(sections[i]);
      if (group && !group->effects.empty())
         pages.push_back(*group);
   }

   return pages;
}

void GetEffects(std::function<void(std::vector<EffectsGroup>)> callback)
{
   using namespace audacity::network_manager;
   using rapidjson::StringRef;

   // TODO Get locale
   const std::string locale = "en-EN";

   rapidjson::Document jsonDoc;
   jsonDoc.SetObject();
   auto& allocator = jsonDoc.GetAllocator();
   jsonDoc.AddMember("query", rapidjson::Value(getEffectsQuery.c_str(), allocator), allocator);

   rapidjson::Value variables(rapidjson::kObjectType);
   variables.AddMember("locale", rapidjson::Value(locale.c_str(), allocator), allocator);
   jsonDoc.AddMember("variables", variables, allocator);

   rapidjson::StringBuffer buffer;
   rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
   jsonDoc.Accept(writer);

   Request request(GetMusehubAPIEndpoint());
   request.setHeader(common_headers::ContentType, common_content_types::ApplicationJson);
   request.setHeader(common_headers::Accept, common_content_types::ApplicationJson);

   auto response = NetworkManager::GetInstance().doPost(request, buffer.GetString(), buffer.GetSize());

   response->setRequestFinishedCallback([response, callback](auto) {
      const auto httpCode = response->getHTTPCode();
      const auto body = response->readAll<std::string>();

      if(httpCode != HttpCode::OK) {
         callback({});
         return;
      }

      rapidjson::Document document;
      document.Parse(body.c_str(), body.size());

      if (document.HasParseError() || !document.IsObject()) {
         callback({});
         return;
      }

      auto groups = parseProductPages(document);
      callback(groups);
   });
}

std::string GetBecomeAPartnerUrl()
{
   return becomeAPartnerUrl;
}

std::string GetMusehubAPIEndpoint()
{
   return musehubAPIEndpointUrl;
}

std::string GetEffectUrl(const std::string& effectCode)
{
   return musehubEffectUrl + effectCode +
         "?" + musehubEffectUtmSource +
         "&" + musehubEffectUtmMediumPrefix + effectCode +
         "&" + musehubEffectUtmCampaignPrefix + effectCode;
}

}
