#include <string>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/fcntl.h>

using namespace std;

inline string copyFile(const string &filename, const string &ext)
{
  string newname = string(tempnam(NULL, NULL)) + ext;
  string oldname = string("data/") + filename + ext;
  char buffer[4096];
  int bytes;
  int inf = open(oldname.c_str(), O_RDONLY);
  int outf = open(newname.c_str(), O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);
  while((bytes = read(inf, buffer, sizeof(buffer))) > 0)
    write(outf, buffer, bytes);
  close(outf);
  close(inf);
  return newname;
}

inline void deleteFile(const string &filename)
{
  remove(filename.c_str());
}
