#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <fcntl.h>
#include <sys/fcntl.h>
#include <sys/stat.h>
#endif
#include <stdio.h>
#include <string.h>
#include <string>
#include <fstream>

using namespace std;

inline string testFilePath(const string &filename)
{
  return string(TESTS_DIR "data/") + filename;
}

#define TEST_FILE_PATH_C(f) testFilePath(f).c_str()

inline string copyFile(const string &filename, const string &ext)
{
  string newname = string(tempnam(NULL, NULL)) + ext;
  string oldname = testFilePath(filename) + ext;
#ifdef _WIN32
  CopyFile(oldname.c_str(), newname.c_str(), FALSE);
  SetFileAttributes(newname.c_str(), GetFileAttributes(newname.c_str()) & ~FILE_ATTRIBUTE_READONLY);
#else
  char buffer[4096];
  int bytes;
  int inf = open(oldname.c_str(), O_RDONLY);
  int outf = open(newname.c_str(), O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);
  while((bytes = read(inf, buffer, sizeof(buffer))) > 0)
    write(outf, buffer, bytes);
  close(outf);
  close(inf);
#endif
  return newname;
}

inline void deleteFile(const string &filename)
{
  remove(filename.c_str());
}

inline bool fileEqual(const string &filename1, const string &filename2)
{
  char buf1[BUFSIZ];
  char buf2[BUFSIZ];

  ifstream stream1(filename1.c_str(), ios_base::in | ios_base::binary);
  ifstream stream2(filename2.c_str(), ios_base::in | ios_base::binary);

  if(!stream1 && !stream2) return true;
  if(!stream1 || !stream2) return false;

  for(;;)
  {
    stream1.read(buf1, BUFSIZ);
    stream2.read(buf2, BUFSIZ);

    streamsize n1 = stream1.gcount();
    streamsize n2 = stream2.gcount();

    if(n1 != n2) return false;

    if(n1 == 0) break;

    if(memcmp(buf1, buf2, n1) != 0) return false;
  }

  return stream1.good() == stream2.good();
}

class ScopedFileCopy
{
public:
  ScopedFileCopy(const string &filename, const string &ext, bool deleteFile=true)
  {
    m_deleteFile = deleteFile;
    m_filename = copyFile(filename, ext);
  }

  ~ScopedFileCopy()
  {
    if(m_deleteFile)
      deleteFile(m_filename);
  }

  string fileName()
  {
    return m_filename;
  }

private:
  bool m_deleteFile;
  string m_filename;
};
