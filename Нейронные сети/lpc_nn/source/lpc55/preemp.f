********************************************************************
*
*	PREEMP Version 55
*
********************************************************************
*
*   Preemphasize speech with a single-zero filter.
*  (When coef = .9375, preemphasis is as in LPC43.)
*
* Inputs:
*  NSAMP  - Number of samples to filter
*  INBUF  - Input speech buffer
*  COEF   - Preemphasis coefficient
* In/Out:
*  Z      - Filter state
* Output:
*  PEBUF  - Preemphasized speech buffer (can be equal to INBUF)
*
	subroutine preemp(inbuf, pebuf, nsamp, coef, z)
	integer nsamp, i
	real temp, inbuf(nsamp), pebuf(nsamp), coef, z

	do 10 i = 1, nsamp
	    temp = inbuf(i) - coef*z
	    z = inbuf(i)
	    pebuf(i) = temp
10	continue

	return
	end
