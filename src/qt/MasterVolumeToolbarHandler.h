#pragma once

#include <QtCore/QBindable>
#include <QtCore/QObject>
#include <QtQml/qqmlregistration.h>
#include "uicomponents/ToolbarHandler.h"

class MasterVolumeToolbarHandler : public ToolbarHandler
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(bool masterVolumeVisible READ MasterVolumeVisible WRITE SetMasterVolumeVisible BINDABLE BindableMasterVolumeVisible)

signals:
   void masterVolumeVisibleChanged(bool isVisible);

public:
   explicit MasterVolumeToolbarHandler(QObject *parent = nullptr);
   virtual ~MasterVolumeToolbarHandler() = default;

   Q_INVOKABLE void RegisterToolbarConfiguration() override;
   void MonitorForConfigurationChanges() override;

   bool MasterVolumeVisible() const;
   void SetMasterVolumeVisible(bool isVisible);
   QBindable<bool> BindableMasterVolumeVisible();

private:
   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(MasterVolumeToolbarHandler,
                                        bool, m_masterVolumeVisible, true,
                                        &MasterVolumeToolbarHandler::masterVolumeVisibleChanged);
};
