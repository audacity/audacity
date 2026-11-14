/*!********************************************************************s
 Audacity: A Digital Audio Editor
 **********************************************************************/

#pragma once

#include <memory>

#include <QObject>

#include "../IResponseFactory.h"

class QNetworkAccessManager;
class QThread;

namespace audacity {
namespace network_manager {
class ResponseFactory final : public QObject, public IResponseFactory
{
    Q_OBJECT
public:
    ResponseFactory();
    ~ResponseFactory() override;

    void setProxy(const std::string& proxy) override;

    ResponsePtr performRequest(RequestVerb verb, const Request& request) override;
    ResponsePtr performRequest(
        RequestVerb verb, const Request& request, RequestPayloadStreamPtr payloadStream) override;
    ResponsePtr performRequest(
        RequestVerb verb, const Request& request, std::unique_ptr<MultipartData> form) override;

    void terminate() override;

private:
    QNetworkAccessManager* mNetworkManager { nullptr };
    QThread* mNetworkThread { nullptr };
    bool mTerminated { false };
};
}
}
