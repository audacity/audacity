/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include <QtCore/QDebug>
#include <QtQml/QQmlApplicationEngine>
#include <QtWidgets/QApplication>
#include "uicomponents/ApplicationConfiguration.h"

int main(int argc, char *argv[])
{
   QGuiApplication app(argc, argv);
   ApplicationConfiguration appConfig;

   QQmlApplicationEngine engine;
   engine.addImportPath(":/uicomponents");

   engine.setInitialProperties({
      { "appConfig", QVariant::fromValue(&appConfig) }
   });
   engine.load("qrc:/qml/main.qml");

   if (engine.rootObjects().isEmpty())
   {
      qDebug() << "Unable to load main.qml";
      return -1;
   }

   return app.exec();
}
