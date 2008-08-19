#include <sys/times.h>
#include <limits.h>
#include <time.h>

double cputime_()
{
  struct tms buffer; /* this structure is defined in /usr/include/sys/times.h */

  times(&buffer);

  return( (buffer.tms_utime + buffer.tms_stime) * (double) (1.0 / CLK_TCK));
}
