#pragma once

#include <QtCore/QAbstractListModel>
#include <QtCore/QByteArray>
#include <QtCore/QHash>
#include <QtCore/QString>
#include <QtCore/QVariant>
#include <QtCore/QVector>
#include <QtGui/QColor>
#include <QtQml/qqml.h>
#include "IconCode.h"

struct ToolbarItem
{
   ToolbarItem(QString type, QString id, QString description, IconCode::Code icon, QColor iconColor, bool visible);

   QString type;
   QString id;
   QString description;
   IconCode::Code icon;
   QColor iconColor;
   bool visible;
};

class ToolbarManager : public QAbstractListModel
{
   Q_OBJECT
   QML_SINGLETON
   QML_ELEMENT

signals:
   void visibleChanged(QString id, bool isVisible);

public:
   explicit ToolbarManager(QObject *parent=nullptr);
   ~ToolbarManager() override;

   QVariant data(const QModelIndex& parent, int role) const override;
   bool setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole) override;
   int rowCount(const QModelIndex& parent = QModelIndex()) const override;

   QHash<int, QByteArray> roleNames() const override;

   Q_INVOKABLE void Reset();

   void AddButtonConfiguration(QString id, QString description, IconCode::Code icon, QColor iconColor, bool visible);
   void AddSeparator();

private:
   enum Roles
   {
      TypeRole = Qt::UserRole + 1,
      IdRole,
      DescriptionRole,
      IconRole,
      IconColorRole,
      VisibilityRole
   };

   QVector<ToolbarItem> m_items;
};
