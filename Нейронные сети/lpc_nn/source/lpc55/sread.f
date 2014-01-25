	function sread(fd, buf, len)
	include 'contrl.fh'
	integer sread, swrite, spd_read, spd_write
	integer fd, len, RECLEN, i, n
	real buf(len), RSCALE, t, t2
	parameter (RECLEN=1024)
	integer*2 rec(RECLEN)
	parameter (RSCALE=1./32768.)

	if (len .gt. RECLEN) stop 'sread: too long'
	n = spd_read(fd, rec, 2*len) / 2
	do i = 1, n
	    buf(i) = rec(i)*RSCALE
	end do
	sread = n
	return

	entry swrite(fd, buf, len)
	if (len .gt. RECLEN) stop 'swrite: too long'
	do i = 1, len
	    t = 32768.*buf(i)
	    t2 = max(-32768.,min(32767.,t))
	    if (t .ne. t2) iclip = iclip + 1
	    rec(i) = t2
	end do
	n = spd_write(fd, rec, 2*len) / 2
	if (n .ne. len) stop 'swrite: I/O error'
	swrite = n
	return
	end
