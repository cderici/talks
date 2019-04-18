#SLIDESHOW := $(shell which slideshow)
SLIDESHOW := $(PLTHOME)/racket/bin/slideshow

run-wide:
	$(SLIDESHOW) --widescreen the-talk.rkt

run:
	$(SLIDESHOW) the-talk.rkt

clean:
	rm -rf *~
	rm -rf compiled/
