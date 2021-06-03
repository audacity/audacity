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
		};
	}
}
