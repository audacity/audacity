/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "private/multisplitter/Item_p.h"
#include "private/multisplitter/Separator_p.h"
#include "private/multisplitter/Widget_qwidget.h"
#include "private/multisplitter/MultiSplitterConfig.h"
#include "private/multisplitter/Separator_qwidget.h"

#include <QPainter>
#include <QtTest/QtTest>

#include <memory.h>


// TODO: namespace

using namespace Layouting;
using namespace KDDockWidgets;

static int st = Item::separatorThickness;

static QtMessageHandler s_original = nullptr;
static QString s_expectedWarning;

class TestMultiSplitter;
static TestMultiSplitter* s_testObject = nullptr;

class MyGuestWidget : public QWidget
                    , public Widget_qwidget
{
    Q_OBJECT
public:

    MyGuestWidget()
        : QWidget()
        , Widget_qwidget(this)
    {
    }

    void setLayoutItem(Item *) override {}

    QSize minimumSizeHint() const override
    {
        return m_minSize;
    }

    void setMinSize(QSize sz)
    {
        if (sz != m_minSize) {
            m_minSize = sz;
            Q_EMIT layoutInvalidated();
        }
    }

    void setMaxSize(QSize sz)
    {
        if (sz != m_maxSize) {
            m_maxSize = sz;
            Q_EMIT layoutInvalidated();
        }
    }

    QSize maxSizeHint() const override {
        return m_maxSize;
    }

    void resizeEvent(QResizeEvent *ev) override
    {
        QWidget::resizeEvent(ev);
        /*Debug helpers:
         * if (geometry() == QRect(800,0, 200,200)) {
            qDebug() << "HERE2";
        }*/
    }

    void moveEvent(QMoveEvent *ev) override
    {
        QWidget::moveEvent(ev);
        /*Debug helpers:
         * if (geometry() == QRect(800,0, 200,200)) {
            //qDebug() << "HERE1";
        }*/
    }

    void paintEvent(QPaintEvent *) override
    {
        QPainter p(this);
        p.fillRect(QWidget::rect(), Qt::green);
    }

Q_SIGNALS:
    void layoutInvalidated();
private:
    QSize m_minSize = QSize(200, 200);
    QSize m_maxSize = Layouting::Item::hardcodedMaximumSize;
};

static void fatalWarningsMessageHandler(QtMsgType t, const QMessageLogContext &context, const QString &msg)
{
    s_original(t, context, msg);
    if (t == QtWarningMsg) {
        if (msg.contains(QLatin1String("checkSanity"))) {
            // These will already fail in QVERIFY(checkSanity())
            return;
        }

        if (s_expectedWarning.isEmpty() ||!msg.contains(s_expectedWarning))
            qFatal("Got a warning, category=%s", context.category);
    }
}

class TestMultiSplitter : public QObject
{
    Q_OBJECT

public:
    QVector<QWidget*> m_hostWidgets; // for cleanup purposes

public Q_SLOTS:
    void initTestCase()
    {
        s_original = qInstallMessageHandler(fatalWarningsMessageHandler);
        s_testObject = this;

        Config::self().setSeparatorFactoryFunc([] (Layouting::Widget *parent) {
            //return new SeparatorWidget(parent);
            return static_cast<Separator*>(new SeparatorWidget(parent));
        });
    }

    void cleanupTestCase()
    {
        auto copy = m_hostWidgets;
        qDeleteAll(copy);
    }

private Q_SLOTS:
    void tst_createRoot();
    void tst_insertOne();
    void tst_insertThreeSideBySide();
    void tst_insertTwoHorizontal();
    void tst_insertTwoVertical();
    void tst_insertOnWidgetItem1();
    void tst_insertOnWidgetItem2();
    void tst_insertOnWidgetItem1DifferentOrientation();
    void tst_insertOnWidgetItem2DifferentOrientation();
    void tst_insertOnRootDifferentOrientation();
    void tst_removeItem1();
    void tst_removeItem2();
    void tst_minSize();
    void tst_resize();
    void tst_resizeWithConstraints();
    void tst_availableSize();
    void tst_missingSize();
    void tst_ensureEnoughSize();
    void tst_turnIntoPlaceholder();
    void tst_suggestedRect();
    void tst_suggestedRect2();
    void tst_suggestedRect3();
    void tst_suggestedRect4();
    void tst_insertAnotherRoot();
    void tst_misc1();
    void tst_misc2();
    void tst_misc3();
    void tst_containerGetsHidden();
    void tst_minSizeChanges();
    void tst_numSeparators();
    void tst_separatorMinMax();
    void tst_separatorRecreatedOnParentChange();
    void tst_containerReducesSize();
    void tst_insertHiddenContainer();
    void tst_availableOnSide();
    void tst_availableToGrowOnSide();
    void tst_resizeViaSeparator();
    void tst_resizeViaSeparator2();
    void tst_resizeViaSeparator3();
    void tst_mapToRoot();
    void tst_closeAndRestorePreservesPosition();
    void tst_minSizeChangedBeforeRestore();
    void tst_separatorMoveCrash();
    void tst_separatorMoveHonoursMax();
    void tst_maxSizeHonoured1();
    void tst_maxSizeHonoured2();
    void tst_maxSizeHonoured3();
    void tst_requestEqualSize();
    void tst_maxSizeHonouredWhenAnotherRemoved();
    void tst_simplify();
    void tst_adjacentLayoutBorders();
};

class MyHostWidget : public QWidget
                   , public Layouting::Widget_qwidget
{
  public:
    MyHostWidget()
        : QWidget()
        , Widget_qwidget(this)
    {
        s_testObject->m_hostWidgets << this;
    }

    ~MyHostWidget() override;

    void paintEvent(QPaintEvent *) override
    {
        QPainter p(this);
        p.fillRect(QWidget::rect(), Qt::yellow);
    }
};

MyHostWidget::~MyHostWidget() {
    s_testObject->m_hostWidgets.removeOne(this);
}

static bool serializeDeserializeTest(const std::unique_ptr<ItemBoxContainer> &root)
{
    // Serializes and deserializes a layout
    if (!root->checkSanity())
        return false;

    const QVariantMap serialized = root->toVariantMap();
    ItemBoxContainer root2(root->hostWidget());

    QHash<QString, Widget*> widgets;
    const Item::List originalItems = root->items_recursive();
    for (Item *item : originalItems)
        if (auto w = static_cast<MyGuestWidget*>(item->guestAsQObject()))
            widgets.insert(w->id(), w);

    root2.fillFromVariantMap(serialized, widgets);

    return root2.checkSanity();
}

static std::unique_ptr<ItemBoxContainer> createRoot()
{
    auto hostWidget = new MyHostWidget();
    hostWidget->setObjectName("HostWidget");
    hostWidget->QWidget::show();
    auto root = new ItemBoxContainer(hostWidget);
    root->setSize({ 1000, 1000 });
    return std::unique_ptr<ItemBoxContainer>(root);
}

static Item* createItem(QSize minSz = {}, QSize maxSz = {})
{
    static int count = 0;
    count++;
    auto hostWidget = new MyHostWidget();
    hostWidget->setObjectName("HostWidget");
    hostWidget->QWidget::show();
    auto item = new Item(hostWidget);
    item->setGeometry(QRect(0, 0, 200, 200));
    item->setObjectName(QStringLiteral("%1").arg(count));
    auto guest = new MyGuestWidget();
    if (minSz.isValid())
        guest->setMinSize(minSz);
    if (maxSz.isValid())
        guest->setMaxSize(maxSz);

    guest->setObjectName(item->objectName());
    item->setGuestWidget(guest);
    return item;
}

static ItemBoxContainer* createRootWithSingleItem()
{
    auto root = new ItemBoxContainer(new MyHostWidget());
    root->setSize({ 1000, 1000 });

    Item *item1 = createItem();
    root->insertItem(item1, Location_OnTop);

    return root;
}

void TestMultiSplitter::tst_createRoot()
{
    auto root = createRoot();
    QVERIFY(root->isRoot());
    QVERIFY(root->isContainer());
    QVERIFY(root->hasOrientation());
    QCOMPARE(root->size(), QSize(1000, 1000));
    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_insertOne()
{
    auto root = createRoot();
    auto item = createItem();
    root->insertItem(item, Location_OnTop);
    QVERIFY(root->checkSanity());
    QCOMPARE(root->numChildren(), 1);
    QVERIFY(!item->isContainer());
    QCOMPARE(root->size(), QSize(1000, 1000));
    QCOMPARE(item->size(), root->size());
    QCOMPARE(item->pos(), QPoint());
    QCOMPARE(item->pos(), root->pos());
    QVERIFY(root->hasChildren());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_insertThreeSideBySide()
{
    // Result is [1, 2, 3]
    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();

    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    root->insertItem(item3, Location_OnRight);

    QVERIFY(root->checkSanity());
    QCOMPARE(root->numChildren(), 3);
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_insertTwoHorizontal()
{
    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    root->insertItem(item1, Location_OnLeft);
    ItemBoxContainer::insertItemRelativeTo(item2, item1, Location_OnRight);
    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_insertTwoVertical()
{
    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    root->insertItem(item1, Location_OnTop);
    ItemBoxContainer::insertItemRelativeTo(item2, item1, Location_OnBottom);
    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_insertOnWidgetItem1()
{
    // We insert into a widget item instead of in a container. It will insert in the container still
    // Result is still [1, 2, 3]

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item3, item2, Location_OnRight);

    QVERIFY(item3->x() > item2->x());
    QCOMPARE(item3->y(), item2->y());

    QVERIFY(root->checkSanity());
    QCOMPARE(root->numChildren(), 3);
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_insertOnWidgetItem2()
{
    // Same, but result [1, 3, 2]

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item3, item2, Location_OnLeft);

    QVERIFY(item1->x() < item3->x());
    QVERIFY(item3->x() < item2->x());
    QCOMPARE(item3->y(), item2->y());

    QVERIFY(root->checkSanity());
    QCOMPARE(root->numChildren(), 3);
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_insertOnWidgetItem1DifferentOrientation()
{
    // Result [1, 2, |3  |]
    //               |3.1|

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item31 = createItem();
    root->insertItem(item1, Location_OnLeft);
    QVERIFY(root->checkSanity());

    root->insertItem(item2, Location_OnRight);
    QVERIFY(root->checkSanity());

    ItemBoxContainer::insertItemRelativeTo(item3, item2, Location_OnRight);
    QVERIFY(root->checkSanity());

    ItemBoxContainer::insertItemRelativeTo(item31, item3, Location_OnBottom);
    QVERIFY(root->checkSanity());

    auto container3 = item3->parentBoxContainer();
    QVERIFY(container3->isContainer());
    QVERIFY(container3 != root.get());
    QVERIFY(root->isHorizontal());
    QVERIFY(container3->isVertical());

    QCOMPARE(root->numChildren(), 3);
    QCOMPARE(container3->numChildren(), 2);

    QVERIFY(item1->x() < item2->x());
    QVERIFY(item3->parentBoxContainer()->x() > item2->x());
    QCOMPARE(item3->x(), 0);
    QCOMPARE(item3->y(), item2->y());
    QCOMPARE(item1->y(), item2->y());

    QVERIFY(item31->y() >= item3->y());
    QCOMPARE(item31->parentBoxContainer(), container3);
    QCOMPARE(item3->parentBoxContainer(), container3);
    QCOMPARE(container3->parentBoxContainer(), root.get());
    QCOMPARE(QPoint(0, 0), item3->pos());
    QCOMPARE(container3->width(), item3->width());
    QCOMPARE(container3->height(), item3->height() + st + item31->height());

    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_insertOnWidgetItem2DifferentOrientation()
{
    // Result [1, 2, |3 3.2|]
    //               |3.1  |

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item5 = createItem();
    auto item4 = createItem();
    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item3, item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item4, item3, Location_OnBottom);
    auto container3Parent = item3->parentBoxContainer();
    ItemBoxContainer::insertItemRelativeTo(item5, item3, Location_OnRight);
    QVERIFY(root->checkSanity());
    auto container3 = item3->parentBoxContainer();

    QCOMPARE(container3->parentBoxContainer(), container3Parent);

    QVERIFY(container3->isContainer());
    QVERIFY(container3 != root.get());
    QVERIFY(root->isHorizontal());
    QVERIFY(container3->isHorizontal());
    QVERIFY(container3Parent->isVertical());

    QCOMPARE(root->numChildren(), 3);
    QCOMPARE(container3->numChildren(), 2);
    QCOMPARE(container3Parent->numChildren(), 2);

    QVERIFY(item1->x() < item2->x());
    QCOMPARE(container3->pos(), QPoint(0, 0l));
    QCOMPARE(item3->pos(), container3->pos());
    QVERIFY(container3Parent->x() > item2->x());
    QCOMPARE(item3->y(), item2->y());
    QCOMPARE(item1->y(), item2->y());

    QVERIFY(item4->y() >= item3->y());
    QCOMPARE(item4->parentBoxContainer(), container3Parent);
    QCOMPARE(item3->parentBoxContainer(), container3);
    QCOMPARE(container3Parent->parentBoxContainer(), root.get());
    QCOMPARE(container3->pos(), item3->pos());
    QCOMPARE(container3->width(), item3->width() + item5->width() + st);
    QCOMPARE(container3->height(), item3->height());
    QCOMPARE(container3Parent->height(), item3->height() + st + item4->height());

    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_insertOnRootDifferentOrientation()
{
    //        [       4     ]
    // Result [1, 2, |3 3.2|]
    //               |3.1  |

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item31 = createItem();
    auto item32 = createItem();
    auto item4 = createItem();
    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item3, item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item31, item3, Location_OnBottom);
    ItemBoxContainer::insertItemRelativeTo(item32, item3, Location_OnRight);
    root->insertItem(item4, Location_OnTop);

    QCOMPARE(item4->parentBoxContainer(), root.get());
    QCOMPARE(item4->pos(), root->pos());
    QCOMPARE(item4->width(), root->width());

    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_removeItem1()
{
    //        [       4     ]
    // Result [1, 2, |3 3.2|]
    //               |3.1  |

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item31 = createItem();
    auto item32 = createItem();
    auto item4 = createItem();
    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item3, item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item31, item3, Location_OnBottom);
    ItemBoxContainer::insertItemRelativeTo(item32, item3, Location_OnRight);
    root->insertItem(item4, Location_OnTop);
    QVERIFY(root->checkSanity());
    QCOMPARE(root->numChildren(), 2);

    root->removeItem(item4);
    QVERIFY(root->checkSanity());
    QCOMPARE(root->numChildren(), 1);

    auto c1 = item1->parentBoxContainer();
    QCOMPARE(c1->pos(), QPoint(0, 0));
    QCOMPARE(c1->width(), root->width());
    QCOMPARE(c1->height(), item1->height());
    QCOMPARE(c1->height(), root->height());

    const int item3and32Width = item3->width() + item32->width() + st;
    root->removeItem(item32);

    QCOMPARE(item3->width(), item3and32Width);
    QVERIFY(root->checkSanity());

    root->removeItem(item31);
    QVERIFY(root->checkSanity());

    QCOMPARE(item2->height(), item3->height());

    QPointer<Item> c3 = item3->parentBoxContainer();
    root->removeItem(c3);
    QVERIFY(c3.isNull());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_removeItem2()
{
    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item31 = createItem();
    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item3, item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item31, item3, Location_OnBottom);
    item31->parentBoxContainer()->removeItem(item31);
    item3->parentBoxContainer()->removeItem(item3);
}

void TestMultiSplitter::tst_minSize()
{
    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item22 = createItem();

    item1->m_sizingInfo.minSize = {101, 150};
    item2->m_sizingInfo.minSize = {200, 300};
    item2->setSize(item2->m_sizingInfo.minSize);
    item22->m_sizingInfo.minSize = {100, 100};

    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item22, item2, Location_OnBottom);

    QCOMPARE(item2->minSize(), QSize(200, 300));
    QCOMPARE(item2->parentBoxContainer()->minSize(), QSize(200, 300+100+st));

    QCOMPARE(root->minSize(), QSize(101+200+st, 300 + 100 + st));
    QVERIFY(root->checkSanity());

    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_resize()
{
    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item31 = createItem();

    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    root->insertItem(item3, Location_OnRight);

    const int item1Percentage = item1->width() / root->width();
    const int item2Percentage = item1->width() / root->width();
    const int item3Percentage = item1->width() / root->width();

    // Now resize:
    root->setSize_recursive({2000, 505});
    QVERIFY(root->checkSanity());

    QVERIFY(item1Percentage - (1.0* item1->width() / root->width()) < 0.01);
    QVERIFY(item2Percentage - (1.0* item2->width() / root->width()) < 0.01);
    QVERIFY(item3Percentage - (1.0* item3->width() / root->width()) < 0.01);
    QCOMPARE(root->width(), 2000);
    QCOMPARE(root->height(), 505);
    QCOMPARE(item1->height(), 505);
    QCOMPARE(item2->height(), 505);
    QCOMPARE(item3->height(), 505);

    ItemBoxContainer::insertItemRelativeTo(item31, item3, Location_OnBottom);

    QVERIFY(root->checkSanity());
    root->setSize_recursive({2500, 505});
    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_resizeWithConstraints()
{
    s_expectedWarning = QStringLiteral("New size doesn't respect size constraints");

    {
        // Test that resizing below minSize isn't permitted.

        auto root = createRoot();
        auto item1 = createItem();
        item1->setMinSize(QSize(500, 500));
        root->insertItem(item1, Location_OnLeft);
        QVERIFY(root->checkSanity());

        root->setSize_recursive(item1->minSize()); // Still fits
        root->setSize_recursive(item1->minSize() - QSize(1, 0)); // wouldn't fit
        QCOMPARE(root->size(), item1->size()); // still has the old size
        QVERIFY(serializeDeserializeTest(root));
    }

    {
        // |1|2|3|

        auto root = createRoot();
        auto item1 = createItem();
        auto item2 = createItem();
        auto item3 = createItem();
        root->setSize_recursive(QSize(2000, 500));
        item1->setMinSize(QSize(500, 500));
        item2->setMinSize(QSize(500, 500));
        item3->setMinSize(QSize(500, 500));
        root->insertItem(item1, Location_OnLeft);
        root->insertItem(item2, Location_OnRight);
        root->insertItem(item3, Location_OnRight);
        QVERIFY(root->checkSanity());

        // TODO: Resize further
    }
    s_expectedWarning.clear();
}

void TestMultiSplitter::tst_availableSize()
{
    auto root = createRoot();
    QCOMPARE(root->availableSize(), QSize(1000, 1000));
    QCOMPARE(root->minSize(), QSize(0, 0));

    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    item1->m_sizingInfo.minSize = {100, 100};
    item2->m_sizingInfo.minSize = {100, 100};
    item3->m_sizingInfo.minSize = {100, 100};

    root->insertItem(item1, Location_OnLeft);
    QCOMPARE(root->availableSize(), QSize(900, 900));
    QCOMPARE(root->minSize(), QSize(100, 100));
    QCOMPARE(root->neighboursLengthFor(item1, Side1, Qt::Horizontal), 0);
    QCOMPARE(root->neighboursLengthFor(item1, Side2, Qt::Horizontal), 0);
    QCOMPARE(root->neighboursMinLengthFor(item1, Side1, Qt::Horizontal), 0);
    QCOMPARE(root->neighboursMinLengthFor(item1, Side2, Qt::Horizontal), 0);

    QCOMPARE(root->neighboursLengthFor_recursive(item1, Side1, Qt::Vertical), 0);
    QCOMPARE(root->neighboursLengthFor_recursive(item1, Side2, Qt::Vertical), 0);
    QCOMPARE(root->neighboursLengthFor_recursive(item1, Side1, Qt::Horizontal), 0);
    QCOMPARE(root->neighboursLengthFor_recursive(item1, Side2, Qt::Horizontal), 0);

    root->insertItem(item2, Location_OnLeft);
    QCOMPARE(root->availableSize(), QSize(800 - st, 900));
    QCOMPARE(root->minSize(), QSize(200 + st, 100));
    QCOMPARE(root->neighboursLengthFor(item1, Side1, Qt::Horizontal), item2->width());
    QCOMPARE(root->neighboursLengthFor(item1, Side2, Qt::Horizontal), 0);
    QCOMPARE(root->neighboursLengthFor(item2, Side1, Qt::Horizontal), 0);
    QCOMPARE(root->neighboursLengthFor(item2, Side2, Qt::Horizontal), item1->width());
    QCOMPARE(root->neighboursMinLengthFor(item1, Side1, Qt::Horizontal), item2->minSize().width());
    QCOMPARE(root->neighboursMinLengthFor(item1, Side2, Qt::Horizontal), 0);
    QCOMPARE(root->neighboursMinLengthFor(item2, Side1, Qt::Horizontal), 0);
    QCOMPARE(root->neighboursMinLengthFor(item2, Side2, Qt::Horizontal), item1->minSize().width());

    QCOMPARE(root->neighboursLengthFor_recursive(item1, Side1, Qt::Vertical), 0);
    QCOMPARE(root->neighboursLengthFor_recursive(item1, Side2, Qt::Vertical), 0);
    QCOMPARE(root->neighboursLengthFor_recursive(item1, Side1, Qt::Horizontal), item2->width());
    QCOMPARE(root->neighboursLengthFor_recursive(item1, Side2, Qt::Horizontal), 0);

    root->insertItem(item3, Location_OnBottom);
    QCOMPARE(root->availableSize(), QSize(800 - st, 800 - st));
    QCOMPARE(root->minSize(), QSize(200 + st, 100 + 100 + st));
    QCOMPARE(item3->parentBoxContainer()->neighboursMinLengthFor(item3, Side1, Qt::Vertical), item1->minSize().height());

    auto container2 = item2->parentBoxContainer();
    QCOMPARE(container2->neighboursLengthFor_recursive(item1, Side1, Qt::Vertical), 0);
    QCOMPARE(container2->neighboursLengthFor_recursive(item1, Side2, Qt::Vertical), item3->height());
    QCOMPARE(container2->neighboursLengthFor_recursive(item1, Side1, Qt::Horizontal), item2->width());
    QCOMPARE(container2->neighboursLengthFor_recursive(item1, Side2, Qt::Horizontal), 0);

    // More nesting
    auto item4 = createItem();
    auto item5 = createItem();
    ItemBoxContainer::insertItemRelativeTo(item4, item3, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item5, item4, Location_OnBottom);

    auto container4 = item4->parentBoxContainer();
    QCOMPARE(container4->neighboursLengthFor_recursive(item4, Side1, Qt::Vertical), item1->height());
    QCOMPARE(container4->neighboursLengthFor_recursive(item4, Side2, Qt::Vertical), item5->height());
    QCOMPARE(container4->neighboursLengthFor_recursive(item4, Side1, Qt::Horizontal), item3->width());
    QCOMPARE(container4->neighboursLengthFor_recursive(item4, Side2, Qt::Horizontal), 0);
    QCOMPARE(container4->neighboursLengthFor_recursive(item5, Side1, Qt::Vertical), item4->height() + item1->height());
    QCOMPARE(container4->neighboursLengthFor_recursive(item5, Side2, Qt::Vertical), 0);
    QCOMPARE(container4->neighboursLengthFor_recursive(item5, Side1, Qt::Horizontal), item3->width());
    QCOMPARE(container4->neighboursLengthFor_recursive(item5, Side2, Qt::Horizontal), 0);

    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_missingSize()
{
    auto root = createRoot();
    QCOMPARE(root->size(), QSize(1000, 1000));
    QCOMPARE(root->availableSize(), QSize(1000, 1000));

    Item *item1 = createItem();
    item1->setMinSize({100, 100});

    Item *item2 = createItem();
    item2->setMinSize(root->size());

    Item *item3 = createItem();
    item3->setMinSize(root->size() + QSize(100, 200));

    // Test with an existing item
    root->insertItem(item1, Location_OnTop);
    QVERIFY(serializeDeserializeTest(root));

    delete item2;
    delete item3;
}

void TestMultiSplitter::tst_ensureEnoughSize()
{
    // Tests that the layout's size grows when the item being inserted wouldn't have enough space

    auto root = createRoot(); /// 1000x1000
    Item *item1 = createItem();
    item1->setMinSize({2000, 500});

    // Insert to empty layout:

    root->insertItem(item1, Location_OnLeft);
    QCOMPARE(root->size(), QSize(2000, 1000));
    QCOMPARE(item1->size(), QSize(2000, 1000));
    QCOMPARE(item1->minSize(), root->minSize());
    QVERIFY(root->checkSanity());

    // Insert to non-empty layout
    Item *item2 = createItem();
    item2->setMinSize({2000, 2000});
    root->insertItem(item2, Location_OnRight);
    QVERIFY(root->checkSanity());
    QCOMPARE(root->size(), QSize(item1->minSize().width() + item2->minSize().width() + st, item2->minSize().height()));
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_turnIntoPlaceholder()
{
    auto root = createRoot();

    int numVisibleItems = 0;
    QObject::connect(root.get(), &ItemBoxContainer::numVisibleItemsChanged, this, [&numVisibleItems] (int count) {
        numVisibleItems = count;
    });

    Item *item1 = createItem();
    Item *item2 = createItem();
    Item *item3 = createItem();
    root->insertItem(item1, Location_OnLeft);
    QCOMPARE(numVisibleItems, 1);
    QVERIFY(item1->isVisible());
    item1->turnIntoPlaceholder();
    QCOMPARE(numVisibleItems, 0);
    QVERIFY(!item1->isVisible());
    QCOMPARE(root->visibleCount_recursive(), 0);
    QCOMPARE(root->count_recursive(), 1);
    QVERIFY(root->checkSanity());

    root->insertItem(item2, Location_OnLeft);
    QVERIFY(root->checkSanity());
    QCOMPARE(numVisibleItems, 1);

    root->insertItem(item3, Location_OnLeft);
    QCOMPARE(numVisibleItems, 2);
    QVERIFY(root->checkSanity());
    QCOMPARE(item2->width() + item3->width() + st, root->width());
    item2->turnIntoPlaceholder();
    QCOMPARE(numVisibleItems, 1);
    QVERIFY(root->checkSanity());
    QCOMPARE(item3->width(), root->width());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_suggestedRect()
{
    auto root = createRoot();
    root->setSize(QSize(2000, 1000));
    const QSize minSize(100, 100);
    Item itemBeingDropped(nullptr);
    itemBeingDropped.setMinSize(minSize);

    QRect leftRect = root->suggestedDropRect(&itemBeingDropped, nullptr, Location_OnLeft);
    QRect topRect = root->suggestedDropRect(&itemBeingDropped, nullptr, Location_OnTop);
    QRect bottomRect = root->suggestedDropRect(&itemBeingDropped, nullptr, Location_OnBottom);
    QRect rightRect = root->suggestedDropRect(&itemBeingDropped, nullptr, Location_OnRight);

    // Test relative to root:
    QVERIFY(leftRect.width() >= minSize.width());
    QVERIFY(topRect.height() >= minSize.height());
    QVERIFY(bottomRect.height() >= minSize.height());
    QVERIFY(rightRect.width() >= minSize.width());
    QCOMPARE(leftRect.topLeft(), QPoint(0, 0));
    QCOMPARE(leftRect.bottomLeft(), root->rect().bottomLeft());
    QCOMPARE(rightRect.topRight(), root->rect().topRight());
    QCOMPARE(rightRect.bottomRight(), root->rect().bottomRight());
    QCOMPARE(topRect.topLeft(), root->rect().topLeft());
    QCOMPARE(topRect.topRight(), root->rect().topRight());
    QCOMPARE(bottomRect.bottomLeft(), root->rect().bottomLeft());
    QCOMPARE(bottomRect.bottomRight(), root->rect().bottomRight());

    // Test relative to an item
    Item *item1 = createItem();
    item1->setMinSize(QSize(100, 100));
    root->insertItem(item1, Location_OnLeft);
    leftRect = root->suggestedDropRect(&itemBeingDropped, item1, Location_OnLeft);
    topRect = root->suggestedDropRect(&itemBeingDropped, item1, Location_OnTop);
    bottomRect = root->suggestedDropRect(&itemBeingDropped, item1, Location_OnBottom);
    rightRect = root->suggestedDropRect(&itemBeingDropped, item1, Location_OnRight);
    QVERIFY(leftRect.width() >= minSize.width());
    QVERIFY(topRect.height() >= minSize.height());
    QVERIFY(bottomRect.height() >= minSize.height());
    QVERIFY(rightRect.width() >= minSize.width());
    QCOMPARE(leftRect.topLeft(), QPoint(0, 0));
    QCOMPARE(leftRect.bottomLeft(), root->rect().bottomLeft());
    QCOMPARE(rightRect.topRight(), root->rect().topRight());
    QCOMPARE(rightRect.bottomRight(), root->rect().bottomRight());
    QCOMPARE(topRect.topLeft(), root->rect().topLeft());
    QCOMPARE(topRect.topRight(), root->rect().topRight());
    QCOMPARE(bottomRect.bottomLeft(), root->rect().bottomLeft());
    QCOMPARE(bottomRect.bottomRight(), root->rect().bottomRight());


    // Insert another item:
    Item *item2 = createItem();
    item1->setMinSize(QSize(100, 100));
    root->insertItem(item2, Location_OnRight);
    leftRect = root->suggestedDropRect(&itemBeingDropped, item2, Location_OnLeft);
    topRect = root->suggestedDropRect(&itemBeingDropped, item2, Location_OnTop);
    bottomRect = root->suggestedDropRect(&itemBeingDropped, item2, Location_OnBottom);
    rightRect = root->suggestedDropRect(&itemBeingDropped, item2, Location_OnRight);
    QCOMPARE(leftRect.y(), item2->geometry().y());
    QVERIFY(leftRect.x() < item2->geometry().x());
    QVERIFY(leftRect.x() > item1->geometry().x());
    QCOMPARE(rightRect.topRight(), root->rect().topRight());
    QCOMPARE(rightRect.bottomRight(), root->rect().bottomRight());
    QCOMPARE(topRect.topLeft(), item2->geometry().topLeft());
    QCOMPARE(topRect.topRight(), item2->geometry().topRight());
    QCOMPARE(bottomRect.bottomLeft(), item2->geometry().bottomLeft());
    QCOMPARE(bottomRect.bottomRight(), item2->geometry().bottomRight());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_suggestedRect2()
{
    // Tests a bug where the inner drop locations didn't work when there was a nested container
    // Like container >> container >> Item

    auto root1 = createRoot();
    auto root2 = createRoot();

    Item itemBeingDropped(nullptr);
    itemBeingDropped.setMinSize(QSize(100, 100));

    Item *item = createItem();

    root2->insertItem(item, Location_OnRight);
    root1->insertItem(root2.release(), Location_OnRight);

    QVERIFY(item->parentBoxContainer()->suggestedDropRect(&itemBeingDropped, item, Location_OnRight).isValid());
}

void TestMultiSplitter::tst_suggestedRect3()
{
    auto root1 = createRoot();
    Item *item1 = createItem();
    Item *item2 = createItem();
    Item *item3 = createItem();
    Item *itemToDrop = createItem();

    root1->insertItem(item1, Location_OnLeft);
    root1->insertItem(item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item3, item2, Location_OnBottom);

    QVERIFY(!item3->parentBoxContainer()->suggestedDropRect(itemToDrop, item3, Location_OnLeft).isEmpty());
    delete itemToDrop;
}

void TestMultiSplitter::tst_suggestedRect4()
{
    auto root = createRoot();

    auto root1 = createRoot();
    Item *item1 = createItem();
    root1->insertItem(item1, Location_OnLeft);

    root->insertItem(root1.release(), Location_OnLeft);

    auto root2 = createRoot();
    Item *item2 = createItem();
    root2->insertItem(item2, Location_OnLeft);

    auto root3 = createRoot();
    Item *item3 = createItem();
    root3->insertItem(item3, Location_OnLeft);

    ItemBoxContainer::insertItemRelativeTo(root2.release(), item1, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(root3.release(), item2, Location_OnBottom);

    Item *itemToDrop = createItem();

    QVERIFY(root->checkSanity());
    QVERIFY(!item3->parentBoxContainer()->suggestedDropRect(itemToDrop, item3, Location_OnLeft).isEmpty());

    delete itemToDrop;
}

void TestMultiSplitter::tst_insertAnotherRoot()
{
    {
        auto root1 = createRoot();
        Item *item1 = createItem();
        root1->insertItem(item1, Location_OnRight);
        QWidget *host1 = root1->hostWidget()->asQWidget();

        auto root2 = createRoot();
        Item *item2 = createItem();
        root2->insertItem(item2, Location_OnRight);

        root1->insertItem(root2.release(), Location_OnBottom);

        QCOMPARE(root1->hostWidget()->asQWidget(), host1);
        QCOMPARE(item2->hostWidget()->asQWidget(), host1);
        const auto &items = root1->items_recursive();
        for (Item *item : items) {
            QCOMPARE(item->hostWidget()->asQWidget(), host1);
            QVERIFY(item->isVisible());
        }
        QVERIFY(root1->checkSanity());
        QVERIFY(serializeDeserializeTest(root1));
    }

    {
        auto root1 = createRoot();
        Item *item1 = createItem();
        Item *item2 = createItem();
        root1->insertItem(item1, Location_OnLeft);
        root1->insertItem(item2, Location_OnRight);
        QWidget *host1 = root1->hostWidget()->asQWidget();

        auto root2 = createRoot();
        Item *item12 = createItem();
        root2->insertItem(item12, Location_OnRight);

        root1->insertItem(root2.release(), Location_OnTop);

        QCOMPARE(root1->hostWidget()->asQWidget(), host1);
        QCOMPARE(item2->hostWidget()->asQWidget(), host1);
        for (Item *item : root1->items_recursive()) {
            QCOMPARE(item->hostWidget()->asQWidget(), host1);
            QVERIFY(item->isVisible());
        }
        QVERIFY(root1->checkSanity());
        QVERIFY(serializeDeserializeTest(root1));
    }
}

void TestMultiSplitter::tst_misc1()
{
    // Random test1

    auto root = createRoot();
    Item *item1 = createItem();
    Item *item2 = createItem();
    Item *item3 = createItem();
    Item *item4 = createItem();
    Item *item5 = createItem();

    root->insertItem(item1, Location_OnTop);
    ItemBoxContainer::insertItemRelativeTo(item2, item1, Location_OnRight);
    root->insertItem(item3, Location_OnBottom);
    ItemBoxContainer::insertItemRelativeTo(item4, item3, Location_OnRight);
    root->insertItem(item5, Location_OnLeft);

    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_misc2()
{
    // Random test1
    // |5|1|2|
    // | |3|4|

    auto root = createRoot();
    ItemBoxContainer *root1 = createRootWithSingleItem();
    Item *item1 = root1->childItems().constFirst();
    ItemBoxContainer *root2 = createRootWithSingleItem();
    ItemBoxContainer *root3 = createRootWithSingleItem();
    Item *item3 = root3->childItems().constFirst();
    ItemBoxContainer *root4 = createRootWithSingleItem();
    ItemBoxContainer *root5 = createRootWithSingleItem();
    Item *item5 = root5->childItems().constFirst();

    root->insertItem(root1, Location_OnTop);
    QVERIFY(root->checkSanity());
    ItemBoxContainer::insertItemRelativeTo(root2, item1, Location_OnRight);
    QVERIFY(root->checkSanity());
    root->insertItem(item3, Location_OnBottom);
    QVERIFY(root->checkSanity());
    ItemBoxContainer::insertItemRelativeTo(root4, item3, Location_OnRight);
    QVERIFY(root->checkSanity());

    root->insertItem(root5, Location_OnLeft);
    QVERIFY(root->checkSanity());

    item5->parentBoxContainer()->removeItem(item5);
    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_misc3()
{
    // Random test1
    // |1|2|3|
    // | |3|4|

    auto root = createRoot();
    Item *item1 = createItem();
    Item *item2 = createItem();
    Item *root2 = createRootWithSingleItem();

    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    root->insertItem(root2, Location_OnRight);
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_containerGetsHidden()
{
    auto root = createRoot();
    Item *item1 = createItem();
    Item *item2 = createItem();
    Item *item3 = createItem();
    root->insertItem(item1, Location_OnLeft);
    QVERIFY(root->checkSanity());

    root->insertItem(item2, Location_OnRight);
    QVERIFY(root->checkSanity());

    ItemBoxContainer::insertItemRelativeTo(item3, item2, Location_OnBottom);
    QVERIFY(root->checkSanity());

    item2->turnIntoPlaceholder();
    QVERIFY(root->checkSanity());

    item3->turnIntoPlaceholder();
    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_minSizeChanges()
{
    auto root = createRoot();
    Item *item1 = createItem();
    root->insertItem(item1, Location_OnLeft);

    root->setSize_recursive(QSize(200, 200));
    QVERIFY(root->checkSanity());

    auto w1 = static_cast<MyGuestWidget*>(item1->guestAsQObject()); // TODO: Static cast not required ?
    w1->setMinSize(QSize(300, 300));
    QVERIFY(root->checkSanity());
    QCOMPARE(root->size(), QSize(300, 300));

    Item *item2 = createItem();
    root->insertItem(item2, Location_OnTop);
    QVERIFY(root->checkSanity());

    root->setSize_recursive(QSize(1000, 1000));
    QVERIFY(root->checkSanity());

    w1->setMinSize(QSize(700, 700));
    QVERIFY(root->checkSanity());
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_numSeparators()
{
    auto root = createRoot();
    Item *item1 = createItem();
    Item *item2 = createItem();
    Item *item3 = createItem();
    Item *item4 = createItem();

    QCOMPARE(root->separators_recursive().size(), 0);

    root->insertItem(item1, Location_OnLeft);
    QCOMPARE(root->separators_recursive().size(), 0);

    root->insertItem(item2, Location_OnLeft);
    QCOMPARE(root->separators_recursive().size(), 1);

    root->insertItem(item3, Location_OnTop);
    QCOMPARE(root->separators_recursive().size(), 2);
    ItemBoxContainer::insertItemRelativeTo(item4, item3, Location_OnRight);
    QCOMPARE(root->separators_recursive().size(), 3);

    root->removeItem(item3);
    QCOMPARE(root->separators_recursive().size(), 2);

    root->clear();
    QCOMPARE(root->separators_recursive().size(), 0);

    Item *item5 = createItem();
    Item *item6 = createItem();

    root->insertItem(item5, Location_OnLeft);
    QCOMPARE(root->separators_recursive().size(), 0);
    root->insertItem(item6, Location_OnLeft, KDDockWidgets::InitialVisibilityOption::StartHidden);
    QCOMPARE(root->separators_recursive().size(), 0);
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_separatorMinMax()
{
    auto root = createRoot();
    Item *item1 = createItem();
    Item *item2 = createItem();
    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnLeft);
    item1->setMinSize(QSize(200, 200));
    item2->setMinSize(QSize(200, 200));

    auto separator = root->separators_recursive().at(0);
    QCOMPARE(root->minPosForSeparator(separator), 200);
    QCOMPARE(root->minPosForSeparator_global(separator), 200); // same, since there's no nesting

    QCOMPARE(root->maxPosForSeparator(separator), root->width() - st - 200);
    QCOMPARE(root->maxPosForSeparator(separator), root->width() - st - 200);
    QVERIFY(serializeDeserializeTest(root));
}

void TestMultiSplitter::tst_separatorRecreatedOnParentChange()
{
    auto root1 = createRoot();
    Item *item1 = createItem();
    Item *item2 = createItem();
    root1->insertItem(item1, Location_OnLeft);
    root1->insertItem(item2, Location_OnLeft);

    auto root2 = createRoot();
    Item *item21 = createItem();
    Item *item22= createItem();
    root2->insertItem(item21, Location_OnLeft);
    root2->insertItem(item22, Location_OnLeft);

    root1->insertItem(root2.get(), Location_OnTop);
    QVERIFY(root1->checkSanity());
}

void TestMultiSplitter::tst_containerReducesSize()
{
    // Tests that the container reduces size when its children get hidden

    auto root = createRoot();
    Item *item1 = createItem();
    Item *item2 = createItem();
    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnLeft);
    Item *item21 = createItem();
    Item *item22= createItem();
    ItemBoxContainer::insertItemRelativeTo(item21, item2, Location_OnTop);
    ItemBoxContainer::insertItemRelativeTo(item22, item2, Location_OnTop);
    QVERIFY(root->checkSanity());

    item2->turnIntoPlaceholder();
    QVERIFY(root->checkSanity());

    item21->turnIntoPlaceholder();
    QVERIFY(root->checkSanity());

    item22->turnIntoPlaceholder();
    QVERIFY(root->checkSanity());
}

void TestMultiSplitter::tst_insertHiddenContainer()
{
    auto root1 = createRoot();
    auto root2 = createRoot();
    Item *item2 = createItem();
    root2->insertItem(item2, Location_OnLeft, KDDockWidgets::InitialVisibilityOption::StartHidden);

    QVERIFY(root1->checkSanity());
    QVERIFY(root2->checkSanity());

    root1->insertItem(root2.release(), Location_OnTop);
    QVERIFY(root1->checkSanity());

    auto anotherRoot = createRoot();
    anotherRoot->insertItem(root1.release(), Location_OnTop);
    QVERIFY(anotherRoot->checkSanity());
}

void TestMultiSplitter::tst_availableOnSide()
{
    // Tests that items are available to squeeze a certain amount (without violating their min-size)

    auto root = createRoot();
    Item *item1 = createItem(/*min=*/QSize(100, 100));
    root->setSize(QSize(1000, 1000));
    root->insertItem(item1, Location_OnLeft);

    QCOMPARE(root->availableToSqueezeOnSide(item1, Side1), 0);
    QCOMPARE(root->availableToSqueezeOnSide(item1, Side2), 0);

    Item *item2 = createItem(/*min=*/QSize(200, 200));
    root->insertItem(item2, Location_OnRight);
    auto separator = root->separators_recursive()[0];
    QCOMPARE(root->minPosForSeparator_global(separator), item1->minSize().width());
    QCOMPARE(root->maxPosForSeparator_global(separator), root->width() - item2->minSize().width() - Item::separatorThickness);

    QCOMPARE(root->availableToSqueezeOnSide(item1, Side1), 0);
    QCOMPARE(root->availableToSqueezeOnSide(item1, Side2), item2->width() - item2->minSize().width());
    QCOMPARE(root->availableToSqueezeOnSide(item2, Side1), item1->width() - item1->minSize().width());
    QCOMPARE(root->availableToSqueezeOnSide(item2, Side2), 0);

    Item *item3 = createItem(/*min=*/QSize(200, 200));
    root->insertItem(item3, Location_OnRight);
    QVERIFY(root->checkSanity());
    QCOMPARE(root->availableToSqueezeOnSide(item3, Side1), (item1->width() - item1->minSize().width()) + (item2->width() - item2->minSize().width()));
    QCOMPARE(root->availableToSqueezeOnSide(item3, Side2), 0);

    auto separator2 = root->separators_recursive()[1];
    QCOMPARE(root->minPosForSeparator_global(separator2), item1->minSize().width() + item2->minSize().width() + Item::separatorThickness);
    QCOMPARE(root->maxPosForSeparator_global(separator2), root->width() - item3->minSize().width() - Item::separatorThickness);

    Item *item4 = createItem(/*min=*/QSize(200, 200));
    ItemBoxContainer::insertItemRelativeTo(item4, item3, Location_OnBottom);

    auto c = item3->parentBoxContainer();
    QCOMPARE(c->availableToSqueezeOnSide_recursive(item3, Side1, Qt::Horizontal), (item1->width() - item1->minSize().width()) + (item2->width() - item2->minSize().width()));
    QCOMPARE(c->availableToSqueezeOnSide_recursive(item3, Side2, Qt::Horizontal), 0);
    QCOMPARE(c->availableToSqueezeOnSide_recursive(item4, Side1, Qt::Horizontal), (item1->width() - item1->minSize().width()) + (item2->width() - item2->minSize().width()));
    QCOMPARE(c->availableToSqueezeOnSide_recursive(item4, Side2, Qt::Horizontal), 0);
    QCOMPARE(c->availableToSqueezeOnSide_recursive(item4, Side1, Qt::Vertical), (item3->height() - item3->minSize().height()));
    QCOMPARE(c->availableToSqueezeOnSide_recursive(item4, Side2, Qt::Vertical), 0);

    Item *item31 = createItem(/*min=*/QSize(100, 100));
    ItemBoxContainer::insertItemRelativeTo(item31, item3, Location_OnRight);
    auto container31 = item31->parentBoxContainer();
    auto separator31 = container31->separators().at(0);

    // Since we don't have widgets with max-size, these two must be the same
    QCOMPARE(container31->minPosForSeparator_global(separator31, false), container31->minPosForSeparator_global(separator31, true));

    QCOMPARE(container31->minPosForSeparator_global(separator31), item1->minSize().width() + item2->minSize().width() + item3->minSize().width() + 2*Item::separatorThickness);
    QCOMPARE(container31->maxPosForSeparator_global(separator31), root->width() -item31->minSize().width() - Item::separatorThickness);
}

void TestMultiSplitter::tst_availableToGrowOnSide()
{
    // Tests that items are available to grow a certain amount (without violating their max-size)
    auto root = createRoot();
    Item *item1 = createItem(/*min=*/QSize(100, 100), /*max=*/QSize(230, 230));
    root->setSize(QSize(1000, 1000));
    root->insertItem(item1, Location_OnLeft);

    QCOMPARE(root->availableToGrowOnSide(item1, Side1), 0);
    QCOMPARE(root->availableToGrowOnSide(item1, Side2), 0);

    Item *item2 = createItem(/*min=*/QSize(200, 200));
    root->insertItem(item2, Location_OnRight);

    // give a resize, so item1 gets smaller than its max-size. Will be unneeded soon
    root->setSize_recursive(QSize(1001, 1001));

    QCOMPARE(root->availableToGrowOnSide(item1, Side1), 0);
    QCOMPARE(root->availableToGrowOnSide(item2, Side2), 0);
    QCOMPARE(root->availableToGrowOnSide(item1, Side2), root->length() - item2->width());
    QCOMPARE(root->availableToGrowOnSide(item2, Side1), item1->maxSizeHint().width() - item1->width());

    auto separator = root->separators_recursive()[0];
    QCOMPARE(root->minPosForSeparator_global(separator, true), item1->minSize().width());
    QCOMPARE(root->maxPosForSeparator_global(separator, true), item1->maxSizeHint().width());

    Item *item3 = createItem(/*min=*/QSize(200, 200), /*max=*/QSize(200, 200));
    root->insertItem(item3, Location_OnRight);
    QVERIFY(root->checkSanity());

    QCOMPARE(root->availableToGrowOnSide(item3, Side2), 0);
    QCOMPARE(root->availableToGrowOnSide(item3, Side1), root->length() - item2->width() - item1->width());
    QCOMPARE(root->availableToGrowOnSide(item2, Side1), item1->maxSizeHint().width() - item1->width());
    QCOMPARE(root->availableToGrowOnSide(item2, Side2), item3->maxSizeHint().width() - item3->width());
}

void TestMultiSplitter::tst_resizeViaSeparator()
{
    auto root = createRoot();
    Item *item1 = createItem(/*min=*/QSize(100, 100));
    Item *item2 = createItem(/*min=*/QSize(100, 100));
    root->setSize(QSize(1000, 1000));
    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);

    auto separator = root->separators_recursive().at(0);
    int oldPos = separator->position();
    const int delta = -50;

    root->requestSeparatorMove(separator, delta);
    QCOMPARE(separator->position(), oldPos + delta);

    // Now move right
    oldPos = separator->position();
    root->requestSeparatorMove(separator, -delta);
    QCOMPARE(separator->position(), oldPos -delta);


    Item *item3 = createItem(/*min=*/QSize(100, 100));
    Item *item4 = createItem(/*min=*/QSize(100, 100));
    root->insertItem(item4, Location_OnLeft);
    root->insertItem(item3, Location_OnRight);
    item2->turnIntoPlaceholder();
    item1->turnIntoPlaceholder();
    separator = root->separators_recursive().at(0);
    root->requestSeparatorMove(separator, delta);
}

void TestMultiSplitter::tst_resizeViaSeparator2()
{
    // Here we resize one of the separators and make sure only the items next to the separator move
    // propagation should only start when constraints have been met

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item4 = createItem();

    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    root->insertItem(item3, Location_OnRight);
    root->insertItem(item4, Location_OnRight);

    auto resizeChildrenTo1000px = [&root] {
        /// Make sure each item has 1000  of width. Cheating here as we don't have API to resize all.
        const int numChildren = root->numChildren();
        auto children = root->childItems();
        for (auto item : std::as_const(children)) {
            item->m_sizingInfo.percentageWithinParent = 1.0 / numChildren;
        }
        root->setSize_recursive(QSize(4000 + Item::separatorThickness*(numChildren-1), 1000));
    };

    const int delta = 100;
    const int originalChildWidth = 1000;
    resizeChildrenTo1000px();

    const auto separators = root->separators_recursive();
    QVERIFY(root->checkSanity());
    QCOMPARE(separators.size(), 3);

    root->requestSeparatorMove(separators[1], delta);

    QCOMPARE(item1->width(), originalChildWidth); // item1 didn't change when we moved the second separator, only item2 and 3 are supposed to move
    QCOMPARE(item2->width(), originalChildWidth + delta);
    QCOMPARE(item3->width(), originalChildWidth - delta);
    QCOMPARE(item4->width(), originalChildWidth);

    // And back
    root->requestSeparatorMove(separators[1], -delta);
    QCOMPARE(item1->width(), originalChildWidth); // item1 didn't change when we moved the second separator, only item2 and 3 are supposed to move
    QCOMPARE(item2->width(), originalChildWidth);
    QCOMPARE(item3->width(), originalChildWidth);
    QCOMPARE(item4->width(), originalChildWidth);
}

void TestMultiSplitter::tst_resizeViaSeparator3()
{
    // Like tst_resizeViaSeparator2 but we have nesting, when a container is shrunk, it too
    // should only shrink its children that are near the separator, instead of all of them equally

    // Layout: |1 | 3|
    //         |4 |  |
    //         -------
    //            2

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item4 = createItem();

    root->insertItem(item1, Location_OnTop);
    root->insertItem(item2, Location_OnBottom);
    ItemBoxContainer::insertItemRelativeTo(item3, item1, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item4, item1, Location_OnBottom);

    // Make some room, so each item has enough space to shrink without hitting constraints
    root->setSize_recursive(QSize(1000, 4000));

    // Our horizontal separator
    const auto separators = root->separators();
    const auto horizontalSeparator = separators[0];
    QCOMPARE(separators.size(), 1);

    const int delta = 10;
    const int oldH1 = item1->height();
    const int oldH2 = item2->height();
    const int oldH3 = item3->height();
    const int oldH4 = item4->height();

    // If the following ever fails, then make sure item4 has space before we move the separator
    QVERIFY(item4->availableLength(Qt::Vertical) > delta);

    // Move separator up:
    root->requestSeparatorMove(horizontalSeparator, -delta);

    QCOMPARE(item2->height(),  oldH2 + delta);
    QCOMPARE(item3->height(),  oldH3 - delta);
    QCOMPARE(item4->height(),  oldH4 - delta);
    QCOMPARE(item1->height(),  oldH1);

    // Move down again
    root->requestSeparatorMove(horizontalSeparator, delta);

    QCOMPARE(item2->height(),  oldH2);
    QCOMPARE(item3->height(),  oldH3);
    QCOMPARE(item4->height(),  oldH4);
    QCOMPARE(item1->height(),  oldH1);
}

void TestMultiSplitter::tst_mapToRoot()
{
    //   1
    // -----
    // 21|22

    auto root = createRoot();
    Item *item1 = createItem();
    root->insertItem(item1, Location_OnLeft);
    auto root2 = createRoot();
    Item *item21 = createItem();
    Item *item22 = createItem();
    root2->insertItem(item21, Location_OnLeft);
    root2->insertItem(item22, Location_OnRight);
    root->insertItem(root2.release(), Location_OnBottom);
    QVERIFY(root->checkSanity());

    auto c = item22->parentBoxContainer();
    QPoint rootPt = c->mapToRoot(QPoint(0, 0));
    QCOMPARE(rootPt, QPoint(0, item1->height() + st));
    QCOMPARE(c->mapFromRoot(rootPt), QPoint(0, 0));
}

void TestMultiSplitter::tst_closeAndRestorePreservesPosition()
{
    // Result is [1, 2, 3]

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item4 = createItem();

    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    root->insertItem(item3, Location_OnRight);
    root->insertItem(item4, Location_OnRight);

    const int oldW1 = item1->width();
    const int oldW2 = item2->width();
    const int oldW3 = item3->width();
    const int oldW4 = item4->width();

    auto guest3 = item3->guestWidget();
    item3->turnIntoPlaceholder();

    // Test that both sides reclaimed the space equally
    QCOMPARE(item1->width(), oldW1);
    QVERIFY(qAbs(item2->width() - (oldW2 + (oldW2/2))) < Item::separatorThickness);
    QVERIFY(qAbs(item4->width() - (oldW4 + (oldW4/2))) < Item::separatorThickness);

    item3->restore(guest3);

    QCOMPARE(item1->width(), oldW1);
    QCOMPARE(item2->width(), oldW2);
    QCOMPARE(item3->width(), oldW3);
    QCOMPARE(item4->width(), oldW4);
}

void TestMultiSplitter::tst_minSizeChangedBeforeRestore()
{
    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();

    root->insertItem(item1, Location_OnTop);
    root->insertItem(item2, Location_OnBottom);
    const QSize originalSize2 = item2->size();

    auto guest2 = qobject_cast<MyGuestWidget*>(item2->guestWidget()->asQWidget());
    const QSize newMinSize = originalSize2 + QSize(10, 10);

    item2->turnIntoPlaceholder();
    guest2->setMinSize(newMinSize);
    item2->restore(guest2);
}

void TestMultiSplitter::tst_separatorMoveCrash()
{
    // Tests a crash I got when moving separator to the right

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item4 = createItem();
    auto item5 = createItem();
    auto item6 = createItem();
    root->insertItem(item1, Location_OnTop);
    root->insertItem(item2, Location_OnBottom);
    ItemBoxContainer::insertItemRelativeTo(item3, item2, Location_OnRight);
    ItemBoxContainer::insertItemRelativeTo(item4, item3, Location_OnBottom);
    ItemBoxContainer::insertItemRelativeTo(item5, item4, Location_OnRight);
    root->insertItem(item6, Location_OnRight);

    ItemBoxContainer *c = item5->parentBoxContainer();
    auto separator = c->separators().constFirst();

    const int available5 = item5->availableLength(Qt::Horizontal);

    // Separator squeezes item5 and starts squeezing item6 by 10px
    c->requestSeparatorMove(separator, available5 + 10);
}

void TestMultiSplitter::tst_separatorMoveHonoursMax()
{
    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    root->insertItem(item1, Location_OnLeft);
    root->insertItem(item2, Location_OnRight);
    root->insertItem(item3, Location_OnRight);

    auto guest2 = static_cast<MyGuestWidget*>(item2->guestWidget());
    const int maxWidth = 250;
    guest2->setMaxSize(QSize(maxWidth, 250));
    auto separator1 = root->separators()[0];
    auto separator2 = root->separators()[1];

    const int min1 = root->minPosForSeparator_global(separator1);
    const int max1 = root->maxPosForSeparator_global(separator1);
    //const int min2 = root->minPosForSeparator_global(separator2);
    const int max2 = root->maxPosForSeparator_global(separator2);

    root->requestSeparatorMove(separator1, separator1->position() - min1);
    QVERIFY(root->checkSanity());
    QVERIFY(item2->width() <= maxWidth);

    root->requestSeparatorMove(separator2, max2 - separator2->position());
    QVERIFY(root->checkSanity());
    QVERIFY(item2->width() <= maxWidth);

    root->requestSeparatorMove(separator1, max1 - separator1->position());
    QVERIFY(root->checkSanity());
    QVERIFY(item2->width() <= maxWidth);

    root->requestSeparatorMove(separator1, -(separator1->position() - min1));
    QVERIFY(root->checkSanity());
    QVERIFY(item2->width() <= maxWidth);
}

void TestMultiSplitter::tst_maxSizeHonoured1()
{
    // Tests that the suggested rect honors max size when adding an item to a layout.

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    root->insertItem(item1, Location_OnTop);
    root->setSize_recursive(QSize(3000, 3000));

    auto guest2 = static_cast<MyGuestWidget*>(item2->guestWidget());
    const int maxHeight = 250;
    guest2->setMaxSize(QSize(250, maxHeight));

    root->insertItem(item2, Location_OnBottom);
    QCOMPARE(item2->height(), maxHeight);
}

void TestMultiSplitter::tst_maxSizeHonoured2()
{
    // Tests that a container gets the max size of its children

    //   2
    // -----
    //  1|3

    auto root1 = createRoot();
    auto root2 = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();

    root1->insertItem(item1, Location_OnTop);
    root1->insertItem(item3, Location_OnRight);
    root2->insertItem(item2, Location_OnTop);
    root2->insertItem(item3, Location_OnLeft);
    root2->removeItem(item3, /*hardRemove=*/false);

    item2->setMaxSizeHint(QSize(200, 200));

    root1->insertItem(root2.release(), Location_OnBottom);
    QCOMPARE(item2->parentBoxContainer()->maxSizeHint(), item2->maxSizeHint());
}

void TestMultiSplitter::tst_maxSizeHonoured3()
{
    {
        // Tests that resizing a window will now make the item with max-size grow past its max
        auto root = createRoot();
        const int minHeight = 100;
        const int maxHeight = 200;
        auto item1 = createItem(QSize(100, minHeight), QSize(200, maxHeight));
        auto item2 = createItem();
        root->setSize(QSize(2000, 2000));

        root->insertItem(item2, Location_OnBottom);
        root->insertItem(item1, Location_OnTop);

        // When adding, we respect max-size
        QVERIFY(item1->height() <= maxHeight);
        QVERIFY(item1->height() >= minHeight);

        // Now resize the window
        root->setSize_recursive(QSize(200, 8000));

        // and we respected max-size too
        QVERIFY(item1->height() <= maxHeight);
        QVERIFY(item1->height() >= minHeight);
    }

    {
        // Also do it with nested containers
        auto root1 = createRoot();
        auto root2 = createRoot();
        const int minHeight = 100;
        const int maxHeight = 200;
        auto item1 = createItem(QSize(100, minHeight), QSize(200, maxHeight));
        auto item2 = createItem();
        root1->setSize(QSize(2000, 2000));
        root2->setSize(QSize(2000, 2000));

        root2->insertItem(item2, Location_OnBottom);
        root1->insertItem(item1, Location_OnTop);
        root2->insertItem(root1.release(), Location_OnTop);

        // When adding, we respect max-size
        QVERIFY(item1->height() <= maxHeight);
        QVERIFY(item1->height() >= minHeight);

        // Now resize the window
        root2->setSize_recursive(QSize(200, 8000));

        // and we respected max-size too
        QVERIFY(item1->height() <= maxHeight);
        QVERIFY(item1->height() >= minHeight);
    }
}

void TestMultiSplitter::tst_requestEqualSize()
{
    // Tests that double-clicking a separator will make both sides equal

    {
        auto root = createRoot();
        auto item1 = createItem();
        auto item2 = createItem();
        root->insertItem(item1, Location_OnLeft);
        root->insertItem(item2, Location_OnRight);

        auto separator = root->separators().constFirst();

        const int item1Squeeze = item1->m_sizingInfo.availableLength(Qt::Horizontal);

        root->requestSeparatorMove(separator, -item1Squeeze);

        QCOMPARE(item1->width(), item1->minSize().width());
        QCOMPARE(item2->width(), root->length() - st - item1->width());

        root->requestEqualSize(separator);
        QVERIFY(qAbs(item1->width() - item2->width()) < 5);
    }

    {
        // Similar, but now we have max-size to honour too
        auto root = createRoot();
        const int minWidth1 = 100;
        const int maxWidth1 = 200;
        auto item1 = createItem(QSize(minWidth1, 100), QSize(maxWidth1, 200));
        auto item2 = createItem();
        root->insertItem(item2, Location_OnRight);
        root->insertItem(item1, Location_OnLeft);

        QCOMPARE(item1->width(), item1->maxSizeHint().width());
        auto separator = root->separators().constFirst();
        root->requestEqualSize(separator);
        // Separator didn't move, doing so would make item1 bigger than its max size
        QCOMPARE(item1->width(), item1->maxSizeHint().width());

        {
            // Let's put the separator further right manually, then try again:
            // (Can't use ItemBoxContainer::requstSeparatorMove() as it respects max-size constraints
            item1->m_sizingInfo.incrementLength(20, Qt::Horizontal);
            item2->m_sizingInfo.incrementLength(-20, Qt::Horizontal);
            root->positionItems();
        }


        QCOMPARE(item1->width(), maxWidth1 + 20);

        // Double clicking on the separator will put it back at a sane place

        const int minPos = root->minPosForSeparator_global(separator, true);
        const int maxPos = root->maxPosForSeparator_global(separator, true);

        QCOMPARE(minPos, minWidth1);
        QCOMPARE(maxPos, maxWidth1);

        root->requestEqualSize(separator);
        QCOMPARE(item1->width(), maxWidth1);
    }
}

void TestMultiSplitter::tst_maxSizeHonouredWhenAnotherRemoved()
{
    // Test that when removing item 3 that all the new available space goes to item1, so that
    // we don't violate the space of item 1

    auto root = createRoot();
    root->setSize(QSize(300, 3000));
    auto item1 = createItem();
    const int minHeight = 100;
    const int maxHeight = 200;
    auto item2 = createItem(QSize(100, minHeight), QSize(200, maxHeight));
    auto item3 = createItem();
    root->insertItem(item1, Location_OnTop);
    root->insertItem(item2, Location_OnBottom);
    root->insertItem(item3, Location_OnBottom);

    QVERIFY(item2->height() <= maxHeight);
    root->removeItem(item3);
    QVERIFY(item2->height() <= maxHeight);

    root->dumpLayout();
}

void TestMultiSplitter::tst_simplify()
{
    QScopedValueRollback<bool> inhibitSimplify(ItemBoxContainer::s_inhibitSimplify, true);

    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    root->insertItem(item1, Location_OnTop);
    root->insertItem(item2, Location_OnBottom);

    auto root2 = createRoot();
    auto root22 = createRoot();
    auto item21 = createItem();
    root22->insertItem(item21, Location_OnLeft);
    root2->insertItem(root22.release(), Location_OnLeft);
    root->insertItem(root2.release(), Location_OnBottom);

    QVERIFY(root->childItems().at(2)->isContainer());

    root->simplify();

    for (Item *item : root->childItems()) {
        QVERIFY(!item->isContainer());
    }
}

void TestMultiSplitter::tst_adjacentLayoutBorders()
{
    auto root = createRoot();
    auto item1 = createItem();
    auto item2 = createItem();
    auto item3 = createItem();
    auto item4 = createItem();
    auto item5 = createItem();

    root->insertItem(item1, Location_OnTop);
    const int allBorders = int(LayoutBorderLocation_All);

    auto borders1 = item1->adjacentLayoutBorders();
    QCOMPARE(borders1, allBorders);
    root->insertItem(item2, Location_OnBottom);

    borders1 = item1->adjacentLayoutBorders();
    QCOMPARE(borders1, allBorders & ~LayoutBorderLocation_South);

    auto borders2 = item2->adjacentLayoutBorders();
    QCOMPARE(borders2, allBorders & ~LayoutBorderLocation_North);

    root->insertItem(item3, Location_OnRight);

    borders1 = item1->adjacentLayoutBorders();
    QCOMPARE(borders1, LayoutBorderLocation_North | LayoutBorderLocation_West);

    borders2 = item2->adjacentLayoutBorders();
    QCOMPARE(borders2, LayoutBorderLocation_South | LayoutBorderLocation_West);

    auto borders3 = item3->adjacentLayoutBorders();
    QCOMPARE(borders3, allBorders & ~(LayoutBorderLocation_West));

    ItemBoxContainer::insertItemRelativeTo(item4, item3, Location_OnBottom);
    auto borders4 = item4->adjacentLayoutBorders();
    QCOMPARE(borders4, LayoutBorderLocation_East | LayoutBorderLocation_South);

    root->insertItem(item5, Location_OnRight);
    borders4 = item4->adjacentLayoutBorders();
    QCOMPARE(borders4, LayoutBorderLocation_South);
}

int main(int argc, char *argv[])
{
    bool qpaPassed = false;
    for (int i = 1; i < argc; ++i) {
        if (qstrcmp(argv[i], "-platform") == 0) {
            qpaPassed = true;
            break;
        }
    }

    if (!qpaPassed) {
        // Use offscreen by default as it's less annoying, doesn't create visible windows
        qputenv("QT_QPA_PLATFORM", "offscreen");
    }

    QApplication app(argc, argv);
    TestMultiSplitter test;

    return QTest::qExec(&test, argc, argv);
}

#include "tst_multisplitter.moc"
