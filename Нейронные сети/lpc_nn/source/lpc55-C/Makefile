PROGRAM= lpc2400
SOURCES.c.lpc2400 = analys.c bitio.c bsynz.c channel.c dcbias.c decode.c deemp.c difmag.c diskio.c dyptrk.c encode.c energy.c filter.c ham84.c hp100.c  initial.c invert.c irc2pc.c ivfilt.c lpc_both.c lpfilt31.c median.c mload.c onset.c pitsyn.c placea.c placev.c preemp.c random.c rcchk.c setup.c synths.c  tbdm.c trans.c voice.c vparms.c
# voice_nn.c 

LIBS= -lm

SOURCES.c= $(SOURCES.c.$(PROGRAM))
OBJECTS.o= $(SOURCES.c:.c=.o) 
OBJECTS= $(OBJECTS.o:%=$(ARCH)/%)
LPC_LINTS= $(SOURCES.c:.c=.ln)
ARCH= $(TARGET_ARCH:-%=%)
FP= $(FP$(TARGET_ARCH))
CC=cc -DTURBO -DFLEXIBLE

.KEEP_STATE:

$(PROGRAM): $(OBJECTS) $(ULIBS)
	$(LINK.c) -o $@ $(OBJECTS) $(ULIBS) $(LIBS)

$(OBJECTS): $(ARCH)
$(ARCH):
	mkdir $(ARCH)

$(ARCH)/%.o: %.c
#	$(COMPILE.c) -g -DL_25MS -o $@ $<
	$(COMPILE.c) -g -o $@ $<

lint: $(LPC_LINTS)
	lint $(LPC_LINTS) -o $(ARCH)/%

listing:
	cat $(SOURCES.c) >$(PROGRAM).list

clean:
	rm -rf $(PROGRAM) $(PROGRAM).list $(ARCH) core

