#include "Sequence.h"
#include "DirManager.h"
#include <wx/hash.h>
#include <vector>
#include <iostream>

class SequenceTest
{
private:
    Sequence* mSequence;
    DirManager* mDirManager;
    std::vector<float> mMemorySequence;

public:
    SequenceTest()
    {
        std::cout << "==> Testing Sequence\n";
        srand(time(NULL));
    }

    void SetUp()
    {
        DirManager::SetTempDir("/tmp/sequence-test-dir");
        mDirManager = new DirManager;

        mSequence = new Sequence(mDirManager, floatSample);

        mMemorySequence.clear();
    }

    void TearDown()
    {
        delete mSequence;
        delete mDirManager;
        mMemorySequence.clear();
    }

    void TestReferencing()
    {
        /* Thrash the Sequence through repeated appends, deletes, etc.
         * Then delete the sequence and ensure that all blocks have
         * been unreferenced to the point of deletion -- the dirmanager
         * should be empty. */

        std::cout << "\tafter thrashing the sequence and deleting it, all block files should have been deleted..." << std::flush;

        int appendBufLen = (int)(mSequence->GetMaxBlockSize() * 1.4);
        samplePtr appendBuf = NewSamples(appendBufLen, floatSample);
        int i;

        for (i = 0; i < 10; i++) {
            mSequence->Append(appendBuf, floatSample, appendBufLen);
        }

        for (i = 0; i < 10; i++) {
            Sequence* tmpSequence;

            /* append */

            mSequence->Append(appendBuf, floatSample, appendBufLen);

            /* copy/paste */

            int s0 = rand() % mSequence->GetNumSamples();
            int len = rand() % (mSequence->GetNumSamples() - s0);
            mSequence->Copy(s0, s0 + len, &tmpSequence);

            int dest = rand() % mSequence->GetNumSamples();
            mSequence->Paste(dest, tmpSequence);
            delete tmpSequence;

            /* delete */

            int del = rand() % mSequence->GetNumSamples();
            int dellen = rand() % ((mSequence->GetNumSamples() - del) / 2);

            mSequence->Delete(del, dellen);
        }

        delete mSequence;
        mSequence = NULL;

        assert(mDirManager->blockFileHash->GetCount() == 0);

        std::cout << "ok\n";
    }

    void TestSetGarbageInput()
    {
        std::cout << "\tSequence::Set() should return false (and not crash) if given garbage input..." << std::flush;

        /* Create 10 samples in the sequence so the Set requests will
         * be valid */
        samplePtr appendBuf = NewSamples(10, floatSample);
        mSequence->Append(appendBuf, floatSample, 10);

        /* should fail, "set" buffer should not be null */
        assert(mSequence->Set(NULL, floatSample, 0, 10) == false);

        /* should fail, -5 is not a sample format */
        assert(mSequence->Set(appendBuf, (sampleFormat) - 5, 0, 10) == false);

        /* should fail, -1 is not a valid offset */
        assert(mSequence->Set(appendBuf, floatSample, -1, 10) == false);

        /* should fail, the sequence is only 10 samples long */
        assert(mSequence->Set(appendBuf, floatSample, 0, 15) == false);

        std::cout << "ok\n";
    }

    void TestGetGarbageInput()
    {
        std::cout << "\tSequence::Get() should return false (and not crash) if given garbage input..." << std::flush;

        /* Create 10 samples in the sequence so the Set requests will
         * be valid */
        samplePtr appendBuf = NewSamples(10, floatSample);
        mSequence->Append(appendBuf, floatSample, 10);

        /* should fail, "get" buffer should not be null */
        assert(mSequence->Get(NULL, floatSample, 0, 10) == false);

        /* should fail, -1 is not a valid offset */
        assert(mSequence->Get(appendBuf, floatSample, -1, 10) == false);

        /* should fail, the sequence is only 10 samples long */
        assert(mSequence->Get(appendBuf, floatSample, 0, 15) == false);

        std::cout << "ok\n";
    }
};

int main()
{
    SequenceTest tester;

    tester.SetUp();
    tester.TestReferencing();
    tester.TearDown();

    tester.SetUp();
    tester.TestSetGarbageInput();
    tester.TearDown();

    tester.SetUp();
    tester.TestGetGarbageInput();
    tester.TearDown();

    return 0;
}

class wxWindow;

void ShowWarningDialog(wxWindow* parent,
                       wxString internalDialogName,
                       wxString message)
{
    std::cout << "warning: " << message << std::endl;
}
