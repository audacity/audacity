#pragma once

#include "UpdateDataParser.h"

namespace audacity
{
	namespace update_manager
	{
		UpdateDataParser::UpdateDataParser()
		{}

		UpdateDataParser::~UpdateDataParser()
		{}

		bool UpdateDataParser::Parse(ServerCommunication::UpdateDataFormat* updateData)
		{
			;
		}

		void UpdateDataParser::startElement(void* userData, const char* name, const char** atts)
		{
			;
		}

		void UpdateDataParser::endElement(void* userData, const char* name)
		{
			;
		}

		void UpdateDataParser::contentHandler(void* userData, const char* s, int len)
		{
			;
		}
	}
}
