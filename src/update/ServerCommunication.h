#pragma once

#include "VersionPatch.h"

class ServerCommunication final
{
public:
	ServerCommunication();
	~ServerCommunication();

	using UpdateDataFormat = std::string;

	bool getUpdateData(UpdateDataFormat& receivedData);

private:
	const std::string mUrl{ "https://updates.audacityteam.org/feed/latest.xml" };

	std::mutex mResponseMutex;
	std::condition_variable mResponseCondition;
};
