************************************************************************
*
*	BITIO Version 55
*
************************************************************************

	function bitsrd(fd, ibits, n)
	integer bitsrd, bitswr, gethx, puthx, fd, n, ibits(n)
	character str*80

************************************************************************
*   Read a frame from bitstream file
************************************************************************

	bitsrd = 0
20	read(fd, 80, end=90) str
80	format(a)
	if (str(1:1).eq.'*') goto 20
	bitsrd = gethx(str, ibits, n)
90	return

************************************************************************
*   Write a frame to bitstream file
************************************************************************

	entry bitswr(fd, ibits, n)

	bitswr = puthx(str, ibits, n)
	write(fd,80) str(1:(n+3)/4)
	return

	end

************************************************************************
*   Read bits from hex digit stream
************************************************************************
*
*   Skip leading blanks, split hex digits into individual bits,
*  terminate after getting n bits or finding non-hex character.
*  Return value = number of bits in input record (which could be
*  more or less than n).

	function gethx(str, ibits, n)
	integer gethx, puthx, n, ibits(n)
	integer ib, ic, i, ii, j, k, nc
	character*(*) str, hex*23
	data hex /'0123456789ABCDEFabcdef '/

	ic = 0
	do j = 1, len(str)
	    k = index(hex, str(j:j)) - 1
	    if (k.lt.0 .or. (k.gt.21 .and. ic.gt.0)) goto 20
	    if (k.le.21) ic = ic + 1
	end do

20	ib = 0
	j = j - ic
	nc = min((n+3)/4, ic)
	do i = 0, nc-1
	    k = index(hex, str(i+j:i+j)) - 1
	    if (k .lt. 0 .or. k.gt.21) stop 'gethx: internal error'
	    if (k .gt. 15) k = k - 6
	    do ii = 1 + max(0, 4*(nc-i)-n), 4
	        ib = ib + 1
	        ibits(ib) = and(ishft(k, ii-4), 1)
	    end do
	end do

90	gethx = ib
	if (ic .gt. nc) gethx = 4*ic
	return

************************************************************************
*   Write bits to hex digit stream
************************************************************************

	entry puthx(str, ibits, n)

	ib = 0
	str = ' '
	nc = (n+3) / 4
	do ic = 1, min(len(str), nc)
	    k = 0
	    do j = 1, min(n-4*(nc-ic), 4)
	        ib = ib + 1
	        k = or(ishft(k,1), and(ibits(ib),1))
	    end do
	    str(ic:ic) = hex(k+1:k+1)
	end do

	puthx = ib
	return
	end
