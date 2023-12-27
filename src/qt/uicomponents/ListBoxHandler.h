#pragma once

#include <algorithm>
#include <memory>
#include <QtCore/QAbstractListModel>
#include <QtCore/QByteArray>
#include <QtCore/QHash>
#include <QtCore/QString>
#include <QtCore/QVariant>
#include <QtCore/QVector>
#include <QtQml/qqmlregistration.h>

struct ListBoxItem
{
   ListBoxItem(QString description, std::function<void()> onClickHandler);

   QString description;
   std::function<void()> callback;
};

class ListBoxHandler : public QAbstractListModel
{
   Q_OBJECT
   QML_ELEMENT

public:
   static std::unique_ptr<ListBoxHandler> Create();

   explicit ListBoxHandler(QObject *parent = nullptr);
   virtual ~ListBoxHandler() = default;

   QVariant data(const QModelIndex &index, int role) const override;
   bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole) override;
   int rowCount(const QModelIndex &index = QModelIndex()) const override;

   QHash<int, QByteArray> roleNames() const override;

   Q_INVOKABLE QRect GetAvailableGeometry() const;
   Q_INVOKABLE QString GetDescription(int index) const;
   Q_INVOKABLE void HandleClickEvent(int index);

private:
   enum Roles
   {
      DescriptionRole = Qt::UserRole + 1,
      ClickCallbackRole
   };

   int padding{ 8 };
   QVector<ListBoxItem> m_items;
};
