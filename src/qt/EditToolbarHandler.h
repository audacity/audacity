#ifndef EDIT_TOOLBAR_HANDLER_H
#define EDIT_TOOLBAR_HANDELR_H

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
   void automation();
   void zoomIn();
   void zoomOut();
   void fitSelection();
   void fitProject();
   void zoomToggle();
   void trim();
   void silence();
};

#endif
