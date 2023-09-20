#include <cassert>
#include <QtCore/QMapIterator>
#include <QtGui/QColor>
#include <QtQml/QQmlContext>
#include "ToolbarHandler.h"

ToolbarHandler::ToolbarHandler(QObject *parent)
   : QObject(parent)
{
}

bool ToolbarHandler::StoreToolbarManager(ToolbarManager *toolbarManager)
{
   assert(toolbarManager != nullptr);

   if (toolbarManager == nullptr)
      return false;

   if (m_toolbarManager == nullptr)
   {
      m_toolbarManager = toolbarManager;
      MonitorForConfigurationChanges();
   }

   return true;
}

void ToolbarHandler::AddToolbarButtonConfiguration(QString id, QString description)
{
   auto object = qmlContext(this)->objectForName(id);
   if (object)
   {
      auto icon = static_cast<IconCode::Code>(object->property("icon").toInt());
      auto iconColor = object->property("iconColor").value<QColor>();
      auto visible = object->property("visible").toBool();

      m_toolbarManager->AddButtonConfiguration(id, description, icon, iconColor, visible);
   }
}

void ToolbarHandler::ToolbarButtonVisibilityHandler(QString id, bool isVisible)
{
   auto it = m_toolbarButtons.find(id);
   if (it != m_toolbarButtons.end())
   {
      it.value().setVisible(isVisible);
   }
}

void ToolbarHandler::UpdateToolbarVisibility()
{
   bool hideToolbar = true;

   QMapIterator<QString, ToolbarButtonState> it{m_toolbarButtons};
   while (it.hasNext())
   {
      it.next();

      if (it.value().isVisible())
      {
         hideToolbar = false;
         break;
      }
   }

   SetToolbarVisible(!hideToolbar);
}

bool ToolbarHandler::ToolbarVisible() const
{
   return m_toolbarVisible;
}

void ToolbarHandler::SetToolbarVisible(bool isVisible)
{
   if (m_toolbarVisible != isVisible)
   {
      m_toolbarVisible = isVisible;
      emit toolbarVisibleChanged(isVisible);
   }
}

