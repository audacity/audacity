#ifndef TOOLS_TOOLBAR_HANDLER_H
#define TOOLS_TOOLBAR_HANDLER_H

#include <QtCore/QObject>
#include <QtQml/qqmlregistration.h>

class ToolsToolbarHandler : public QObject
{
   Q_OBJECT
   QML_ELEMENT

signals:
   void updateStatusBar(QString status);

public:
   explicit ToolsToolbarHandler(QObject* parent = nullptr);
   virtual ~ToolsToolbarHandler() = default;

public slots:
   void setup();
};

#endif
