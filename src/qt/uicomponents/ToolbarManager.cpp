#include "ToolbarManager.h"

ToolbarItem::ToolbarItem(QString type, QString id, QString description, IconCode::Code icon, QColor iconColor, bool visible)
   : type{ type }
   , id{ id }
   , description { description }
   , icon{ icon }
   , iconColor{ iconColor }
   , visible{ visible }
{
}

ToolbarManager::ToolbarManager(QObject *parent)
   : QAbstractListModel(parent)
{
}

ToolbarManager::~ToolbarManager()
{
}

QVariant ToolbarManager::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= rowCount())
      return QVariant();

   const auto& item = m_items[index.row()];

   switch (role)
   {
      case Roles::TypeRole:
         return item.type;
      case Roles::DescriptionRole:
         return item.description;
      case Roles::IconRole:
         return static_cast<char16_t>(item.icon);
      case Roles::IconColorRole:
         return item.iconColor;
      case Roles::VisibilityRole:
         return item.visible;
   }

   return QVariant();
}

bool ToolbarManager::setData(const QModelIndex& index, const QVariant& value, int role)
{
   if (!index.isValid())
      return false;

   if (role == Roles::VisibilityRole)
   {
      auto& item = m_items[index.row()];
      auto visible = value.toBool();

      if (item.visible != visible)
      {
         item.visible = visible;

         emit visibleChanged(item.id, visible);
         emit dataChanged(index, index, { Roles::VisibilityRole });

         return true;
      }
   }

   return false;
}

int ToolbarManager::rowCount(const QModelIndex& index) const
{
   if (index.isValid())
      return 0;

   return static_cast<int>(m_items.count());
}

QHash<int, QByteArray> ToolbarManager::roleNames() const
{
   static QHash<int, QByteArray> mapping
   {
      { TypeRole,        "type"        },
      { DescriptionRole, "description" },
      { IconRole,        "icon"        },
      { IconColorRole,   "iconColor"   },
      { VisibilityRole,  "visible"     }
   };

   return mapping;
}

void ToolbarManager::Reset()
{
   beginResetModel();
   m_items.clear();
   endResetModel();
}

void ToolbarManager::AddButtonConfiguration(QString id, QString description, IconCode::Code icon, QColor iconColor, bool visible)
{
   int row = rowCount();

   beginInsertRows(QModelIndex(), row, row);
   m_items << ToolbarItem{ "configuration", id, description, icon, iconColor, visible };
   endInsertRows();
}

void ToolbarManager::AddSeparator()
{
   int row = rowCount();

   beginInsertRows(QModelIndex(), row, row);
   m_items << ToolbarItem{ "separator", "", "", IconCode::Code::NONE, QColor(), true };
   endInsertRows();
}
