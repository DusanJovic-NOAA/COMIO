#include <sys/errno.h>
#include <sys/stat.h>
#include <unistd.h>

#define COMIO_MAX_NAME 256

int comio_link_check(const char *path) {
  char sbuf[COMIO_MAX_NAME + 1];
  struct stat buf;

  /* read symbolic link */
  int lcount = readlink(path, sbuf, COMIO_MAX_NAME);
  if (lcount > 0) {
    /* path is a link */
    sbuf[lcount] = '\0';
    int rc = stat(path, &buf);
    if ((rc == -1) && (errno == ENOENT)) return 1;          /* target not found */
    if ((rc ==  0) && ((buf.st_mode & S_IFMT) == S_IFREG))  /* target is regular file: delete */
      return ((unlink(sbuf) == 0) ? 1 : -1);
  }
  return 0;
}
