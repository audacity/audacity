#include <QDebug>
#include "ToolsToolbarHandler.h"

ToolsToolbarHandler::ToolsToolbarHandler(QObject *parent)
   : QObject(parent)
{
}

void ToolsToolbarHandler::setup()
{
   emit updateStatusBar("Setup clicked");
}
