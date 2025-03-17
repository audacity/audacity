/*
* Audacity: A Digital Audio Editor
*/

constexpr static int DEFAULT_CURSOR_SIZE = 26;

namespace au::projectscene {
class CustomCursor : public QObject
{
    Q_OBJECT

    Q_PROPERTY(bool active READ active WRITE setActive NOTIFY activeChanged FINAL)
    Q_PROPERTY(QString source READ source WRITE setSource NOTIFY sourceChanged FINAL)
    Q_PROPERTY(int size READ size WRITE setSize NOTIFY sizeChanged FINAL)
    Q_PROPERTY(bool appActive READ appActive NOTIFY appActiveChanged FINAL)

public:
    explicit CustomCursor(QQuickItem* parent = nullptr);
    ~CustomCursor() = default;

    bool active() const;
    QString source() const;
    int size() const;
    bool appActive() const;

    void setActive(bool active);
    void setSource(QString source);
    void setSize(int size);

signals:
    void activeChanged();
    void sourceChanged();
    void sizeChanged();
    void appActiveChanged();

private:
    bool m_active = false;
    QString m_source;
    QCursor m_cursor;
    int m_size = DEFAULT_CURSOR_SIZE;
    bool m_appActive = true;
};
}
