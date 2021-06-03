#pragma once

#include "ServerCommunication.h"

#include "expat.h"

namespace audacity
{
	namespace update_manager
	{
		class UpdateDataParser final
		{
		public:
			UpdateDataParser();
			~UpdateDataParser();

			bool Parse(ServerCommunication::UpdateDataFormat* updateData);

			// TODO: check for private this.
			// Callback functions for expat
			static void startElement(void* userData, const char* name, const char** atts);
			static void endElement(void* userData, const char* name);
			static void contentHandler(void* userData, const char* s, int len);

		private:
		};
	}
}
