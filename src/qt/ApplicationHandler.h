#pragma once

#include <QtCore/QObject>
#include <QtCore/QTranslator>
#include <QtQml/qqmlregistration.h>

class ApplicationHandler : public QObject
{
   Q_OBJECT
   QML_ELEMENT

public:
   explicit ApplicationHandler(QObject *parent = nullptr);
   virtual ~ApplicationHandler() = default;

   Q_INVOKABLE void ChangeToEnglish();
   Q_INVOKABLE void ChangeToGerman();

private:
   QTranslator translator;
};
