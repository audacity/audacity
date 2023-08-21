#pragma once

#include <QFont>
#include <QtCore/QObject>
#include <QtCore/QStringList>
#include <QtQml/qqml.h>

class UiThemeHandler : public QObject
{
   Q_OBJECT
   QML_SINGLETON
   QML_ELEMENT

   Q_PROPERTY(QFont iconFont READ IconFont FINAL)
   Q_PROPERTY(QFont bodyFont READ BodyFont FINAL)
   Q_PROPERTY(QFont timecodeFont READ TimecodeFont FINAL)

public:
   UiThemeHandler();
   ~UiThemeHandler() override = default;

   Q_INVOKABLE QStringList themeFiles();

   QFont IconFont() const;
   QFont BodyFont() const;
   QFont TimecodeFont() const;

private:

   //temporarily added here, this should be become a regular QML property
   //when theme definitions are moved out from the module
   QFont m_iconFont;
   QFont m_bodyFont;
   QFont m_timecodeFont;
};
