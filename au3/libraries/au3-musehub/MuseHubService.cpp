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

#include "au3-strings/Languages.h"
#include "au3-string-utils/UrlEncode.h"
#include "au3-network-manager/NetworkManager.h"
#include "au3-network-manager/Request.h"
#include "au3-network-manager/IResponse.h"

namespace audacity::musehub {
static const std::string becomeAPartnerUrl = "https://developer.musehub.com/muse-partners-help/introduction/becoming-a-muse-partner";
static const std::string musehubAPIEndpointUrl = "https://customers-api.musehub.com/graphql/v3";
static const std::string musehubAPIDevEndpointUrl = "https://customers-api-dev.musehub.com/graphql/v3";
static const std::string musehubEffectUrl = "https://www.musehub.com/plugin/";
static const std::string musehubEffectsQueryId = "audacity-effects-page-v1";
static const std::string fallbackLocale = "en-US";

static const std::string musehubEffectUtmSource = "utm_source=au-app-get-fx-panel";
static const std::string musehubEffectUtmMediumPrefix = "utm_medium=";
static const std::string musehubEffectUtmCampaignPrefix = "utm_campaign=au-app-get-fx-mh-";

// Not sent, the server resolves musehubEffectsQueryId to this query
// Kept as the reference for the shape of the response we parse
static const std::string getEffectsQuery
    =
        R"(
    query EffectsQuery($locale: String) {
      product_pages_configuration {
        audacityPageSections {
          ... on ProductPageSectionDynamic {
            title(locale: { locale: $locale })
            productCards {
              ...CardFields
            }
          }
          ... on ProductPageSectionRegular {
            title(locale: { locale: $locale })
            productCards {
              ...CardFields
            }
          }
        }
      }
    }

    fragment CardFields on ProductCardRegular {
      iconImageUrl
      product(locale: { locale: $locale }) {
        ... on ProductBase {
          id
          title
          subtitle
          iconImageUrl
          code
        }
      }
    }
)";

static std::optional<EffectInfo> parseEffect(const rapidjson::Value& effectObj)
{
    if (!effectObj.HasMember("product")
        || !effectObj.HasMember("iconImageUrl")
        || !effectObj["product"].HasMember("code")
        || !effectObj["product"].HasMember("title")
        || !effectObj["product"].HasMember("subtitle")) {
        assert(false);
        return std::nullopt;
    }

    return EffectInfo {
        effectObj["iconImageUrl"].GetString(),
        effectObj["product"]["code"].GetString(),
        effectObj["product"]["title"].GetString(),
        effectObj["product"]["subtitle"].GetString(),
    };
}

static std::optional<EffectsGroup> parseEffectGroup(const rapidjson::Value& groupObj)
{
    if (!groupObj.HasMember("title")
        || !groupObj.HasMember("productCards")
        || !groupObj["productCards"].IsArray()) {
        assert(false);
        return std::nullopt;
    }

    EffectsGroup group;
    group.title = groupObj["title"].GetString();

    const rapidjson::Value& cards = groupObj["productCards"];
    for (rapidjson::SizeType i = 0; i < cards.Size(); i++) {
        auto effect = parseEffect(cards[i]);
        if (effect) {
            group.effects.push_back(*effect);
        }
    }

    return group;
}

static std::vector<EffectsGroup> parseProductPages(const rapidjson::Document& doc)
{
    if (!doc.HasMember("data")
        || !doc["data"].HasMember("product_pages_configuration")
        || !doc["data"]["product_pages_configuration"].HasMember("audacityPageSections")) {
        assert(false);
        return {};
    }

    std::vector<EffectsGroup> pages;

    const rapidjson::Value& sections = doc["data"]["product_pages_configuration"]["audacityPageSections"];

    for (rapidjson::SizeType i = 0; i < sections.Size(); i++) {
        auto group = parseEffectGroup(sections[i]);
        if (group && !group->effects.empty()) {
            pages.push_back(*group);
        }
    }

    return pages;
}

// Converts internal locale tag [ca_ES@valencia] to [ca-ES] accepted by MuseHub
static std::string GetLocale()
{
    wxString lang = Languages::GetLangShort().BeforeFirst(L'@');
    lang.Replace(L"_", L"-");

    return lang.empty() ? fallbackLocale : lang.ToStdString();
}

void GetEffects(std::function<void(std::vector<EffectsGroup>)> callback)
{
    using namespace audacity::network_manager;

    const std::string variables = UrlEncode(R"({"locale":")" + GetLocale() + R"("})");
    const std::string url = GetMusehubAPIEndpoint()
                            + "?pqId=" + musehubEffectsQueryId
                            + "&variables=" + variables;

    Request request(url);
    request.setHeader(common_headers::Accept, common_content_types::ApplicationJson);

    auto response = NetworkManager::GetInstance().doGet(request);

    response->setRequestFinishedCallback([response, callback](auto) {
        const auto httpCode = response->getHTTPCode();
        const auto body = response->readAll<std::string>();

        if (httpCode != HttpCode::OK) {
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
    return musehubEffectUrl + effectCode
           + "?" + musehubEffectUtmSource
           + "&" + musehubEffectUtmMediumPrefix + effectCode
           + "&" + musehubEffectUtmCampaignPrefix + effectCode;
}
}
