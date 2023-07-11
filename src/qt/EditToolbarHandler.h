#pragma once

#include <QtCore/QBindable>
#include <QtCore/QObject>
#include <QtQml/qqmlregistration.h>
#include "uicomponents/ToolbarHandler.h"

class EditToolbarHandler : public ToolbarHandler
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(bool automationVisible READ AutomationVisible WRITE SetAutomationVisible BINDABLE BindableAutomationVisible)
   Q_PROPERTY(bool zoomInVisible READ ZoomInVisible WRITE SetZoomInVisible BINDABLE BindableZoomInVisible)
   Q_PROPERTY(bool zoomOutVisible READ ZoomOutVisible WRITE SetZoomOutVisible BINDABLE BindableZoomOutVisible)
   Q_PROPERTY(bool fitSelectionVisible READ FitSelectionVisible WRITE SetFitSelectionVisible BINDABLE BindableFitSelectionVisible)
   Q_PROPERTY(bool fitProjectVisible READ FitProjectVisible WRITE SetFitProjectVisible BINDABLE BindableFitProjectVisible)
   Q_PROPERTY(bool zoomToggleVisible READ ZoomToggleVisible WRITE SetZoomToggleVisible BINDABLE BindableZoomToggleVisible)
   Q_PROPERTY(bool trimVisible READ TrimVisible WRITE SetTrimVisible BINDABLE BindableTrimVisible)
   Q_PROPERTY(bool silenceVisible READ SilenceVisible WRITE SetZoomInVisible BINDABLE BindableSilenceVisible)

signals:
   void automationVisibleChanged(bool isVisible);
   void zoomInVisibleChanged(bool isVisible);
   void zoomOutVisibleChanged(bool isVisible);
   void fitSelectionVisibleChanged(bool isVisible);
   void fitProjectVisibleChanged(bool isVisible);
   void zoomToggleVisibleChanged(bool isVisible);
   void trimVisibleChanged(bool isVisible);
   void silenceVisibleChanged(bool isVisible);
   void updateStatusBar(QString status);

public:
   explicit EditToolbarHandler(QObject *parent = nullptr);
   virtual ~EditToolbarHandler() = default;

   Q_INVOKABLE void RegisterToolbarConfiguration() override;
   void MonitorForConfigurationChanges() override;

   bool AutomationVisible() const;
   void SetAutomationVisible(bool isVisible);
   QBindable<bool> BindableAutomationVisible();

   bool ZoomInVisible() const;
   void SetZoomInVisible(bool isVisible);
   QBindable<bool> BindableZoomInVisible();

   bool ZoomOutVisible() const;
   void SetZoomOutVisible(bool isVisible);
   QBindable<bool> BindableZoomOutVisible();

   bool FitSelectionVisible() const;
   void SetFitSelectionVisible(bool isVisible);
   QBindable<bool> BindableFitSelectionVisible();

   bool FitProjectVisible() const;
   void SetFitProjectVisible(bool isVisible);
   QBindable<bool> BindableFitProjectVisible();

   bool ZoomToggleVisible() const;
   void SetZoomToggleVisible(bool isVisible);
   QBindable<bool> BindableZoomToggleVisible();

   bool TrimVisible() const;
   void SetTrimVisible(bool isVisible);
   QBindable<bool> BindableTrimVisible();

   bool SilenceVisible() const;
   void SetSilenceVisible(bool isVisible);
   QBindable<bool> BindableSilenceVisible();

public slots:
   void Automation();
   void ZoomIn();
   void ZoomOut();
   void FitSelection();
   void FitProject();
   void ZoomToggle();
   void Trim();
   void Silence();

private:
   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(EditToolbarHandler,
                                        bool, m_automationVisible, true,
                                        &EditToolbarHandler::automationVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(EditToolbarHandler,
                                        bool, m_zoomInVisible, true,
                                        &EditToolbarHandler::zoomInVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(EditToolbarHandler,
                                        bool, m_zoomOutVisible, true,
                                        &EditToolbarHandler::zoomOutVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(EditToolbarHandler,
                                        bool, m_fitSelectionVisible, true,
                                        &EditToolbarHandler::fitSelectionVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(EditToolbarHandler,
                                        bool, m_fitProjectVisible, true,
                                        &EditToolbarHandler::fitProjectVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(EditToolbarHandler,
                                        bool, m_zoomToggleVisible, true,
                                        &EditToolbarHandler::zoomToggleVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(EditToolbarHandler,
                                        bool, m_trimVisible, true,
                                        &EditToolbarHandler::trimVisibleChanged);

   Q_OBJECT_BINDABLE_PROPERTY_WITH_ARGS(EditToolbarHandler,
                                        bool, m_silenceVisible, true,
                                        &EditToolbarHandler::silenceVisibleChanged);
};
