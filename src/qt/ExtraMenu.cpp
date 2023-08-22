#include "ExtraMenu.h"

#include "BasicUI.h"
#include "CodeConversions.h"

class MenuItem final : ClientData::Base
{
public:
   MenuItem(TranslatableString name, std::function<void()> action)
      : name(std::move(name)), action(std::move(action))
   { }

   TranslatableString name;
   std::function<void()> action { nullptr };
};

ExtraMenu::Item::Item(TranslatableString name, std::function<void()> action)
   : RegisteredFactory([item = MenuItem { std::move(name), std::move(action) } ](auto&) {
      return std::make_unique<MenuItem>(item);
   })
{
}

ExtraMenu::ExtraMenu(QObject* parent)
   : QObject(parent)
{
   BuildAll();
}

ExtraMenu::~ExtraMenu() = default;

QStringList ExtraMenu::items() const
{
   QStringList result;
   ForEach([&](const MenuItem& item){
      result.push_back(audacity::ToQString(item.name.Translation()));
   });
   return result;
}

void ExtraMenu::activate(int index)
{
   int counter { 0 };
   ForEach([&](MenuItem& item)
   {
      if(counter == index)
         item.action();
      ++counter;
   });
}
