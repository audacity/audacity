/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include <QtQml>
#include <QtWidgets/QApplication>
#include <QFileInfo>

int main(int argc, char *argv[])
{
   QGuiApplication app(argc, argv);
   QQmlApplicationEngine engine;

   engine.load("qrc:/qml/main.qml");

   if (engine.rootObjects().isEmpty())
      return -1;

   return app.exec();
}
