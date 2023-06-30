#pragma once

#include <QtCore/QObject>
#include <QtCore/QStringList>
#include <QtQml/qqml.h>

class UiThemeHandler : public QObject
{
   Q_OBJECT
   QML_SINGLETON
   QML_ELEMENT

public:
   UiThemeHandler() = default;
   virtual ~UiThemeHandler() = default;

   Q_INVOKABLE QStringList themeFiles();
};
