/*
 * Audacity: A Digital Audio Editor
 */
#include <QCoreApplication>

#include <gtest/gtest.h>

int main(int argc, char** argv)
{
    QCoreApplication application(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
