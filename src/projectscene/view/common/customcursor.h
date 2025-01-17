/*
* Audacity: A Digital Audio Editor
*/

namespace au::projectscene {
class CustomCursor : public QObject
{
    Q_OBJECT

    Q_PROPERTY(bool active READ active WRITE setActive NOTIFY activeChanged FINAL)
    Q_PROPERTY(QString source READ source WRITE setSource NOTIFY sourceChanged FINAL)

public:
    explicit CustomCursor(QQuickItem* parent = nullptr);
    ~CustomCursor() = default;

    bool active() const;
    QString source() const;

    void setActive(bool active);
    void setSource(QString source);

signals:
    void activeChanged();
    void sourceChanged();

private:
    bool m_active = false;
    QString m_source;
    QCursor m_cursor;
};
}
