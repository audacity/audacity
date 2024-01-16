#pragma once

#include <QFont>
#include <QColor>
#include <QtCore/QObject>
#include <QtCore/QStringList>
#include <QtQml/qqml.h>

class UiTheme
   : public QObject
{
   Q_OBJECT

   Q_PROPERTY(QStringList themes READ themes)

   Q_PROPERTY(QFont iconFont READ iconFont CONSTANT FINAL)
   Q_PROPERTY(QFont bodyFont READ bodyFont CONSTANT FINAL)
   Q_PROPERTY(QFont timecodeFont READ timecodeFont CONSTANT FINAL)

   Q_PROPERTY(qreal itemOpacityDisabled READ itemOpacityDisabled CONSTANT FINAL)
   Q_PROPERTY(qreal buttonOpacityNormal READ buttonOpacityNormal CONSTANT FINAL)
   Q_PROPERTY(qreal buttonOpacityHit READ buttonOpacityHit CONSTANT FINAL)
   Q_PROPERTY(qreal buttonOpacityHover READ buttonOpacityHover CONSTANT FINAL)
   Q_PROPERTY(int borderWidth READ borderWidth CONSTANT FINAL)
   Q_PROPERTY(int defaultButtonSize READ defaultButtonSize CONSTANT FINAL)

   Q_PROPERTY(qreal opacityLight READ opacityLight CONSTANT FINAL)
   Q_PROPERTY(qreal opacityMedium READ opacityMedium CONSTANT FINAL)
   Q_PROPERTY(qreal opacityStrong READ opacityStrong CONSTANT FINAL)
   Q_PROPERTY(qreal opacityOpaque READ opacityOpaque CONSTANT FINAL)

   Q_PROPERTY(QColor backgroundColor1 MEMBER mBackgroundColor1 NOTIFY backgroundColor1Changed)
   Q_PROPERTY(QColor backgroundColor2 MEMBER mBackgroundColor2 NOTIFY backgroundColor2Changed)
   Q_PROPERTY(QColor backgroundColor3 MEMBER mBackgroundColor3 NOTIFY backgroundColor3Changed)
   Q_PROPERTY(QColor backgroundColor4 MEMBER mBackgroundColor4 NOTIFY backgroundColor4Changed)
   Q_PROPERTY(QColor fontColor1 MEMBER mFontColor1 NOTIFY fontColor1Changed)
   Q_PROPERTY(QColor fontColor2 MEMBER mFontColor2 NOTIFY fontColor2Changed)
   Q_PROPERTY(QColor buttonColor MEMBER mButtonColor NOTIFY buttonColorChanged)
   Q_PROPERTY(QColor brandColor MEMBER mBrandColor NOTIFY brandColorChanged)
   Q_PROPERTY(QColor textFieldColor MEMBER mTextFieldColor NOTIFY textFieldColorChanged)

   Q_PROPERTY(QColor successColor MEMBER mSuccessColor NOTIFY successColorChanged)
   Q_PROPERTY(QColor dangerColor MEMBER mDangerColor NOTIFY dangerColorChanged)

   Q_PROPERTY(QColor strokeColor1 MEMBER mStrokeColor1 NOTIFY strokeColor1Changed)
   Q_PROPERTY(QColor strokeColor2 MEMBER mStrokeColor2 NOTIFY strokeColor2Changed)
   Q_PROPERTY(QColor strokeColor3 MEMBER mStrokeColor3 NOTIFY strokeColor3Changed)

   Q_PROPERTY(QColor waveformRMSColor MEMBER mWaveformRMSColor NOTIFY waveformRMSColorChanged)
   Q_PROPERTY(QColor waveformHighlightColor MEMBER mWaveformHighlightColor NOTIFY waveformHighlightColorChanged)
   Q_PROPERTY(QColor waveformPeakColor MEMBER mWaveformPeakColor NOTIFY waveformPeakColorChanged)

   Q_PROPERTY(QColor clipStrokeColor MEMBER mClipStrokeColor NOTIFY clipStrokeColorChanged)
   Q_PROPERTY(QColor clipHeaderColor MEMBER mClipHeaderColor NOTIFY clipHeaderColorChanged)
   Q_PROPERTY(QColor textHighlightColor MEMBER mTextHighlightColor NOTIFY textHighlightColorChanged)
   Q_PROPERTY(QColor invalidInputColor MEMBER mInvalidInputColor NOTIFY invalidInputColorChanged)

   static int TypeID;

   UiTheme(QObject *parent = nullptr);

   void applyTheme(QQmlEngine& engine, const QString& name);

public:
   ~UiTheme() override = default;

   static void Register();
   static UiTheme* Get(QQmlEngine& engine);

   QStringList themes();

   Q_INVOKABLE void applyTheme(const QString& name);

   QFont iconFont() const;
   QFont bodyFont() const;
   QFont timecodeFont() const;

   qreal itemOpacityDisabled() const { return 0.3; }
   qreal buttonOpacityNormal() const { return 0.7; }
   qreal buttonOpacityHit() const { return 1.0; }
   qreal buttonOpacityHover() const { return 0.5; }
   int borderWidth() const { return 0; }
   int defaultButtonSize() const { return 32; }

   qreal opacityLight() const { return 0.25; }
   qreal opacityMedium() const { return 0.5; }
   qreal opacityStrong() const { return 0.75; }
   qreal opacityOpaque() const { return 1.0; }

   QColor backgroundColor1() const;
   QColor backgroundColor2() const;
   QColor backgroundColor3() const;
   QColor backgroundColor4() const;
   QColor fontColor1() const;
   QColor fontColor2() const;
   QColor buttonColor() const;
   QColor brandColor() const;
   QColor textFieldColor() const;
   QColor successColor() const;
   QColor dangerColor() const;
   QColor strokeColor1() const;
   QColor strokeColor2() const;
   QColor strokeColor3() const;
   QColor waveformRMSColor() const;
   QColor waveformHighlightColor() const;
   QColor waveformPeakColor() const;
   QColor clipStrokeColor() const;
   QColor clipHeaderColor() const;
   QColor textHighlightColor() const;
   QColor invalidInputColor() const;

signals:
   void backgroundColor1Changed();
   void backgroundColor2Changed();
   void backgroundColor3Changed();
   void backgroundColor4Changed();
   void fontColor1Changed();
   void fontColor2Changed();
   void buttonColorChanged();
   void brandColorChanged();
   void textFieldColorChanged();
   void successColorChanged();
   void dangerColorChanged();
   void strokeColor1Changed();
   void strokeColor2Changed();
   void strokeColor3Changed();
   void waveformRMSColorChanged();
   void waveformHighlightColorChanged();
   void waveformPeakColorChanged();
   void clipStrokeColorChanged();
   void clipHeaderColorChanged();
   void textHighlightColorChanged();
   void invalidInputColorChanged();

private:

   //temporarily added here, this should be become a regular QML property
   //when theme definitions are moved out from the module
   QFont mIconFont;
   QFont mBodyFont;
   QFont mTimecodeFont;

   QColor mBackgroundColor1;
   QColor mBackgroundColor2;
   QColor mBackgroundColor3;
   QColor mBackgroundColor4;
   QColor mFontColor1;
   QColor mFontColor2;
   QColor mButtonColor;
   QColor mBrandColor;
   QColor mTextFieldColor;
   QColor mSuccessColor;
   QColor mDangerColor;
   QColor mStrokeColor1;
   QColor mStrokeColor2;
   QColor mStrokeColor3;
   QColor mWaveformRMSColor;
   QColor mWaveformHighlightColor;
   QColor mWaveformPeakColor;
   QColor mClipStrokeColor;
   QColor mClipHeaderColor;
   QColor mTextHighlightColor;
   QColor mInvalidInputColor;
};
