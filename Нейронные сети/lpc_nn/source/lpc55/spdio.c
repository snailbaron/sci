#include <fcntl.h>
#include <stdio.h>
#include <string.h>

spd_open_(f_ptr, flags, f_len)
char *f_ptr;
int *flags, f_len;
{
	char *s, *z;
	int i, flag=*flags;

	s = z = (char *)malloc(f_len + 1);
	for (i=0; i<f_len; i++) *z++ = *f_ptr++;
	*z++ = 0;
	if (z=strchr(s, ' ')) *z = 0;
	if (flag && O_WRONLY) flag |= O_CREAT;
	if ((i = open(s, flag, 0666)) < 0) {
	    fprintf(stderr, "%s:", s);
	    Perr("open"); }
	free(s);
	return(i);;
}

spd_close_(fd)
int *fd;
{
	return(close(fd));
}

spd_read_(fd, buf, n)
int *fd, *n;
char *buf;
{
	int i;
	if((i = read(*fd, buf, *n)) < 0)
	    Perr("read");
	return(i);
}

spd_write_(fd, buf, n)
int *fd, *n;
char *buf;
{
	int i;
	if ((i = write(*fd, buf, *n)) < 0)
	    Perr("write");
	return(i);
}

Perr(s)
char *s;
{
	perror(s);
	exit(-1);
}
