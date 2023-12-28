#include <cassert>
#include <functional>
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

std::unique_ptr<ListBoxHandler> ListBoxHandler::Create()
{
   return std::make_unique<ListBoxHandler>();
}

ListBoxHandler::ListBoxHandler(QObject* parent)
   : QAbstractListModel(parent)
{
   m_items << ListBoxItem("Item 1", [this]() { qDebug() << "Item 1 selected"; });
   m_items << ListBoxItem("Item 2", [this]() { qDebug() << "Item 2 selected"; });
   m_items << ListBoxItem("Item 3", [this]() { qDebug() << "Item 3 selected"; });
   m_items << ListBoxItem("Item 4", [this]() { qDebug() << "Item 4 selected"; });
   m_items << ListBoxItem("Item 5", [this]() { qDebug() << "Item 5 selected"; });
   m_items << ListBoxItem("Item 6", [this]() { qDebug() << "Item 6 selected"; });
   m_items << ListBoxItem("Item 7", [this]() { qDebug() << "Item 7 selected"; });
   m_items << ListBoxItem("Item 8", [this]() { qDebug() << "Item 8 selected"; });
   m_items << ListBoxItem("Item 9", [this]() { qDebug() << "Item 9 selected"; });
   m_items << ListBoxItem("Item 10", [this]() { qDebug() << "Item 10 selected"; });
}

QVariant ListBoxHandler::data(const QModelIndex &index, int role) const
{
   if (!index.isValid() || index.row() >= rowCount())
      return QVariant();

   if (role == Roles::DescriptionRole)
   {
      const auto& item = m_items[index.row()];
      return item.description;
   }

   return QVariant();
}

bool ListBoxHandler::setData(const QModelIndex &index, const QVariant &value, int role)
{
   if (!index.isValid())
      return false;

   auto& item = m_items[index.row()];

   if (role == Roles::DescriptionRole)
   {
      emit dataChanged(index, index, { Roles::DescriptionRole });
      return true;
   }

   return false;
}

int ListBoxHandler::rowCount(const QModelIndex &index) const
{
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

QString ListBoxHandler::GetDescription(int index) const
{
   if (index < 0 || index >= m_items.count())
      return {};

   return m_items[index].description;
}

Q_INVOKABLE void ListBoxHandler::HandleClickEvent(int index)
{
   if (index < 0 || index >= m_items.count())
      return;

   const auto& callback = m_items[index].callback;

   if (callback)
      callback();
}
