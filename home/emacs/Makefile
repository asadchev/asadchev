EMACS=emacs-23.4

all: $(EMACS)
	ln -s $</bin bin

$(EMACS).tar.gz:
	wget http://ftp.gnu.org/pub/gnu/emacs/$@

$(EMACS): $(EMACS).tar.gz
	tar xf $<
	cd $@ \
          && ./configure --prefix=$(PWD) --with-jpeg=no --with-gif=no --with-tiff=no --with-sound=no \
             --without-x \
             --with-crt-dir=/usr/lib/x86_64-linux-gnu \
	  && make install
