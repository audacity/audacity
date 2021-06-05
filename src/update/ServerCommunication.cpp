#include "ServerCommunication.h"

#include "NetworkManager.h"
#include "IResponse.h"
#include "Request.h"

ServerCommunication::ServerCommunication()
{}

ServerCommunication::~ServerCommunication()
{}

bool ServerCommunication::getUpdateData(UpdateDataFormat& receivedData)
{
	audacity::network_manager::Request request(mUrl);
	auto response = audacity::network_manager::NetworkManager::GetInstance().doGet(request);

	response->setRequestFinishedCallback([response, &receivedData, this](audacity::network_manager::IResponse*) {
		std::lock_guard<std::mutex> lock(mResponseMutex);

		if (response->getError() == audacity::network_manager::NetworkError::NoError)
			receivedData = response->readAll<UpdateDataFormat>();

		mResponseCondition.notify_one();
		}
	);

	std::unique_lock<std::mutex> lock{ mResponseMutex };
	mResponseCondition.wait_for(lock, std::chrono::seconds(60));

	if (!response->isFinished())
	{
		response->abort();
		return false;
	}

	return response->getError() == audacity::network_manager::NetworkError::NoError;
}
