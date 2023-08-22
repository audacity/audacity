#pragma once

#include <QObject>

#include "ClientData.h"

class MenuItem;

// Playground to invoke and test C++ objects from QML with few lines
// of boilerplate code.
class ExtraMenu
   : public QObject
   , public ClientData::Site<ExtraMenu, MenuItem>
{
   Q_OBJECT

   Q_PROPERTY(QStringList items READ items FINAL)
public:

   //Initialization helper
   struct Item final : RegisteredFactory
   {
      explicit Item(TranslatableString name, std::function<void()> action);
   };

   ExtraMenu(QObject* parent = nullptr);
   ~ExtraMenu() override;

   QStringList items() const;

   Q_INVOKABLE void activate(int index);
};
