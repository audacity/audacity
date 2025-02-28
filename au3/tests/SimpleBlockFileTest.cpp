#include <iostream>
#include <ostream>
#include <cassert>

#include "sndfile.h"
#include "blockfile/SimpleBlockFile.h"

class SimpleBlockFileTest
{
    SimpleBlockFile* int16BlockFile;
    SimpleBlockFile* int24BlockFile;
    SimpleBlockFile* floatBlockFile;

    int summaryLen;

    short* int16Data;
    int* int24Data;
    float* floatData;
    int dataLen;

public:
    SimpleBlockFileTest()
    {
        std::cout << "==> Testing SimpleBlockFile\n";
    }

    void setUp()
    {
        dataLen = 200000;

        int16Data = new short[dataLen];
        int24Data = new int[dataLen];
        floatData = new float[dataLen];

        int i;
        int sign = 1;

        for (i = 0; i < dataLen; i++) {
            sign *= -1;
            // These have no significance, it's just random data
            int16Data[i] = sign * (i * i);
            int24Data[i] = sign * ((i * i * i) % 0x000FFFFF);
            float j = (float)i;
            floatData[i] = sign * j / ((j * j) + 1);
        }

        int16BlockFile = new SimpleBlockFile(wxFileName("/tmp/int16"),
                                             (samplePtr)int16Data, dataLen,
                                             int16Sample);
        int24BlockFile = new SimpleBlockFile(wxFileName("/tmp/int24"),
                                             (samplePtr)int24Data, dataLen,
                                             int24Sample);
        floatBlockFile = new SimpleBlockFile(wxFileName("/tmp/float"),
                                             (samplePtr)floatData, dataLen,
                                             floatSample);
    }

    void tearDown()
    {
        delete [] int16Data;
        delete [] int24Data;
        delete [] floatData;
        delete int16BlockFile;
        delete int24BlockFile;
        delete floatBlockFile;
    }

    void testFileValidity()
    {
        // Use libsndfile to read the file.  Make sure:
        //    1. it is correctly recognized as an AU file
        //    2. it has all the header information we expect

        std::cout << "\tthe created files should be valid AU files with the expected number of samples...";
        std::cout << std::flush;

        BlockFile* theFiles[] = { int16BlockFile, int24BlockFile, floatBlockFile };

        for (int i = 0; i < 3; i++) {
            BlockFile* bf = theFiles[i];
            SF_INFO info;

            memset(&info, 0, sizeof(info));
            SNDFILE* sf = sf_open(bf->GetFileName().GetFullPath(), SFM_READ, &info);

            assert(sf);
            assert(info.frames == dataLen);
            assert(info.channels == 1);
            assert(info.format & SF_FORMAT_AU);

            sf_close(sf);
        }

        std::cout << "OK\n";
    }

    template<class T> void AssertBuffersEqual(T* b1, T* b2, int len)
    {
        for ( int i = 0; i < len; i++ ) {
            if (b1[i] != b2[i]) {
                std::cout << b1[i] << " != " << b2[i] << " (i=" << i << ")" << std::endl;
                assert(false);
            }
        }
    }

    void testCorrectDataWritten()
    {
        // use libsndfile to read back the whole blockfile, make sure it matches
        // the data we initially wrote
        std::cout << "\tVerifying that we wrote what we think we wrote..." << std::flush;

        SF_INFO info1, info2, info3;
        memset(&info1, 0, sizeof(info1));
        memset(&info2, 0, sizeof(info2));
        memset(&info3, 0, sizeof(info3));

        SNDFILE* int16sf = sf_open(int16BlockFile->GetFileName().GetFullPath(), SFM_READ, &info1);
        SNDFILE* int24sf = sf_open(int24BlockFile->GetFileName().GetFullPath(), SFM_READ, &info2);
        SNDFILE* floatsf = sf_open(floatBlockFile->GetFileName().GetFullPath(), SFM_READ, &info3);

        // First do a read of the entire block
        short int16buf[dataLen];
        int int24buf[dataLen];
        float floatbuf[dataLen];

        sf_read_short(int16sf, int16buf, dataLen);
        sf_read_int(int24sf, int24buf, dataLen);
        sf_read_float(floatsf, floatbuf, dataLen);

        AssertBuffersEqual(int16buf, int16Data, dataLen);
        AssertBuffersEqual(floatbuf, floatData, dataLen);

        // for the 24-bit buffer, libsndfile gives the 24 bits to us in the 3 most significant
        // byts of a 32-bit int.  So we need to shift them right 8 to get back our 24-bit data.
        for ( int i = 0; i < dataLen; i++ ) {
            int24buf[i] = int24buf[i] >> 8;
        }
        AssertBuffersEqual(int24buf, int24Data, dataLen);

        sf_close(int16sf);
        sf_close(int24sf);
        sf_close(floatsf);

        std::cout << "OK\n";
    }

    void testReads()
    {
        // Now use the blockfile's method to read data and compare it to what we originally wrote
        std::cout << "\tVerifying that we can read back correctly..." << std::flush;

        samplePtr int16buf = NewSamples(dataLen, int16Sample);
        samplePtr int24buf = NewSamples(dataLen, int24Sample);
        samplePtr floatbuf = NewSamples(dataLen, floatSample);

        // First try a read of the entire buffer
        int16BlockFile->ReadData(int16buf, int16Sample, 0, dataLen);
        int24BlockFile->ReadData(int24buf, int24Sample, 0, dataLen);
        floatBlockFile->ReadData(floatbuf, floatSample, 0, dataLen);

        AssertBuffersEqual(int16Data, (short*)int16buf, dataLen);
        AssertBuffersEqual(int24Data, (int*)int24buf, dataLen);
        AssertBuffersEqual(floatData, (float*)floatbuf, dataLen);

        // Now test a read that starts at the beginning but quits
        // before the end
        int someOffset = 537;

        int16BlockFile->ReadData(int16buf, int16Sample, 0, someOffset);
        int24BlockFile->ReadData(int24buf, int24Sample, 0, someOffset);
        floatBlockFile->ReadData(floatbuf, floatSample, 0, someOffset);

        AssertBuffersEqual(int16Data, (short*)int16buf, someOffset);
        AssertBuffersEqual(int24Data, (int*)int24buf, someOffset);
        AssertBuffersEqual(floatData, (float*)floatbuf, someOffset);

        // Now try a read that starts in the middle and goes to the
        // end
        int16BlockFile->ReadData(int16buf, int16Sample, someOffset, dataLen - someOffset);
        int24BlockFile->ReadData(int24buf, int24Sample, someOffset, dataLen - someOffset);
        floatBlockFile->ReadData(floatbuf, floatSample, someOffset, dataLen - someOffset);

        AssertBuffersEqual(int16Data + someOffset, (short*)int16buf, dataLen - someOffset);
        AssertBuffersEqual(int24Data + someOffset, (int*)int24buf, dataLen - someOffset);
        AssertBuffersEqual(floatData + someOffset, (float*)floatbuf, dataLen - someOffset);

        std::cout << "OK\n";
    }
};

int main()
{
    SimpleBlockFileTest tester;

    tester.setUp();
    tester.testFileValidity();
    tester.tearDown();

    tester.setUp();
    tester.testCorrectDataWritten();
    tester.tearDown();

    tester.setUp();
    tester.testReads();
    tester.tearDown();

    return 0;
}
