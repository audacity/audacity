#pragma once

#include <functional>
#include <QtCore/QMap>
#include <QtCore/QObject>
#include <QtCore/QString>
#include <QtQml/qqmlregistration.h>
#include "ToolbarManager.h"

class ToolbarHandler : public QObject
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(bool toolbarVisible READ ToolbarVisible WRITE SetToolbarVisible NOTIFY toolbarVisibleChanged)

signals:
   void toolbarVisibleChanged(bool isVisible);

public:
   explicit ToolbarHandler(QObject *parent = nullptr);
   virtual ~ToolbarHandler() = default;

   Q_INVOKABLE bool StoreToolbarManager(ToolbarManager *toolbarManager);
   Q_INVOKABLE virtual void RegisterToolbarConfiguration() = 0;

   virtual void MonitorForConfigurationChanges() =  0;

   bool ToolbarVisible() const;
   void SetToolbarVisible(bool isVisible);

public slots:
   void ToolbarButtonVisibilityHandler(QString id, bool isVisible);

protected:
   void AddToolbarButtonConfiguration(QString id, QString description);
   void UpdateToolbarVisibility();

   struct ToolbarButtonState
   {
      std::function<bool()> isVisible;
      std::function<void(bool)> setVisible;
   };

   bool m_toolbarVisible{ true };
   ToolbarManager* m_toolbarManager{ nullptr };
   QMap<QString, ToolbarButtonState> m_toolbarButtons;
};
