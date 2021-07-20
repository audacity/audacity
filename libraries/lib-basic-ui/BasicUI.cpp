/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicUI.cpp

Paul Licameli

**********************************************************************/
#include "BasicUI.h"

#include <mutex>
#include <vector>

namespace  BasicUI {
WindowPlacement::~WindowPlacement() = default;

Services::~Services() = default;

ProgressDialog::~ProgressDialog() = default;

GenericProgressDialog::~GenericProgressDialog() = default;

static Services *theInstance = nullptr;

Services *Get() { return theInstance; }

Services *Install(Services *pInstance)
{
   auto result = theInstance;
   theInstance = pInstance;
   return result;
}

static std::recursive_mutex sActionsMutex;
static std::vector<Action> sActions;

void CallAfter(Action action)
{
   if (auto p = Get())
      p->DoCallAfter(action);
   else {
      // No services yet -- but don't lose the action.  Put it in a queue
      auto guard = std::lock_guard{ sActionsMutex };
      sActions.emplace_back(std::move(action));
   }
}

void Yield()
{
   do {
      // Dispatch anything in the queue, added while there were no Services
      {
         auto guard = std::lock_guard{ sActionsMutex };
         std::vector<Action> actions;
         actions.swap(sActions);
         for (auto &action : actions)
            action();
      }

      // Dispatch according to Services, if present
      if (auto p = Get())
         p->DoYield();
   }
   // Re-test for more actions that might have been enqueued by actions just
   // dispatched
   while ( !sActions.empty() );
}

TranslatableString DefaultCaption()
{
   return XO("Message");
}
}
