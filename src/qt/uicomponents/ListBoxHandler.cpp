#include <cassert>
#include <utility>
#include <QtCore/QDebug>
#include <QtGui/QScreen>
#include <QtWidgets/QApplication>
#include "ListBoxHandler.h"

ListBoxItem::ListBoxItem(QString description, std::function<void()> onClickHandler)
   : description(std::move(description))
   , callback(std::move(onClickHandler))
{
}

ListBoxHandler::ListBoxHandler(QObject* parent)
   : QAbstractListModel(parent)
{
}

QVariant ListBoxHandler::data(const QModelIndex &index, int role) const
{
   if (!index.isValid() || index.row() >= rowCount())
      return QVariant();

   const auto& item = m_items[index.row()];

   switch (role)
   {
      case Roles::DescriptionRole:
         return item.description;
      case Roles::ClickCallbackRole:
         return QVariant::fromValue(item.callback);
   }

   return QVariant();
}

bool ListBoxHandler::setData(const QModelIndex &index, const QVariant &value, int role)
{
   return index.isValid();
}

int ListBoxHandler::rowCount(const QModelIndex &index) const
{
   if (!index.isValid())
      return 0;

   return static_cast<int>(m_items.count());
}

QHash<int, QByteArray> ListBoxHandler::roleNames() const
{
   static QHash<int, QByteArray> mapping
   {
      { DescriptionRole,   "description" },
      { ClickCallbackRole, "callback"    }
   };

   return mapping;
}

QRect ListBoxHandler::GetAvailableGeometry() const
{
   auto availableGeometry = QApplication::primaryScreen()->availableGeometry();

   availableGeometry.setX(padding);
   availableGeometry.setY(padding);
   availableGeometry.setHeight(availableGeometry.height() - padding);
   availableGeometry.setWidth(availableGeometry.width() - padding);

   return availableGeometry;
}
