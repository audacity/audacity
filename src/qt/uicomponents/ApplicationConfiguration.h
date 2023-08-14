#ifndef APPLICATION_CONFIGURATION_H
#define APPLICATION_CONFIGURATION_H

#include <QtCore/QObject>
#include <QtGui/QFont>
#include <QtQml/qqml.h>

class ApplicationConfiguration : public QObject
{
   Q_OBJECT
   QML_ELEMENT

   Q_PROPERTY(QFont iconFont READ IconFont NOTIFY themeChanged FINAL)
   Q_PROPERTY(QFont bodyFont READ BodyFont NOTIFY themeChanged FINAL)
   Q_PROPERTY(QFont timecodeFont READ TimecodeFont NOTIFY themeChanged FINAL)

signals:
   void themeChanged();

public:
   ApplicationConfiguration();

   QFont IconFont() const;
   QFont BodyFont() const;
   QFont TimecodeFont() const;

private:
   QFont m_iconFont;
   QFont m_bodyFont;
   QFont m_timecodeFont;
};

#endif
