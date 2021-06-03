#include "ServerCommunication.h"

#include "NetworkManager.h"
#include "IResponse.h"
#include "Request.h"

namespace audacity
{
	namespace update_manager
	{
		ServerCommunication::ServerCommunication()
		{}

		ServerCommunication::~ServerCommunication()
		{}

		bool ServerCommunication::getUpdateData (UpdateDataFormat* receivedData)
		{
			audacity::network_manager::Request request(mUrl);
			auto response = audacity::network_manager::NetworkManager::GetInstance().doGet(request);

			response->setRequestFinishedCallback([response, &receivedData](audacity::network_manager::IResponse*) {
				*receivedData = response->readAll<UpdateDataFormat>();
				}
			);
		}
	}
}
