#pragma once

#include <QtCore/QObject>
#include <QtQml/qqmlregistration.h>

class EditToolbarHandler : public QObject
{
   Q_OBJECT
   QML_ELEMENT

signals:
   void updateStatusBar(QString status);

public:
   explicit EditToolbarHandler(QObject *parent = nullptr);
   virtual ~EditToolbarHandler() = default;

public slots:
   void Automation();
   void ZoomIn();
   void ZoomOut();
   void FitSelection();
   void FitProject();
   void ZoomToggle();
   void Trim();
   void Silence();
};
