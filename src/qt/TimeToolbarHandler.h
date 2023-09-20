#pragma once

#include <QtCore/QBindable>
#include <QtCore/QObject>
#include <QtQml/qqmlregistration.h>
#include "uicomponents/ToolbarHandler.h"

class TimeToolbarHandler : public ToolbarHandler
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(bool timeControlVisible READ TimeControlVisible WRITE SetTimeControlVisible BINDABLE BindableTimeControlVisible)

signals:
   void timeControlVisibleChanged(bool isVisible);

public:
   explicit TimeToolbarHandler(QObject *parent = nullptr);
   virtual ~TimeToolbarHandler() = default;

   Q_INVOKABLE void RegisterToolbarConfiguration() override;
   void MonitorForConfigurationChanges() override;

   bool TimeControlVisible() const;
   void SetTimeControlVisible(bool isVisible);
   QBindable<bool> BindableTimeControlVisible();

private:
   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(TimeToolbarHandler,
                                        bool, m_timeControlVisible, true,
                                        &TimeToolbarHandler::timeControlVisibleChanged);

};
