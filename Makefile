VERSION:=1.2.2
PACKAGE_NAME:=lvznumbers-mode-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

package: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" --exclude="Makefile" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir $@
	cp -r ../lvznumbers-mode/* $@
	sed -re "s/VERSION/$(VERSION)/" $@/lvznumbers-mode-pkg.el > $@/"~tmp~"
	mv $@/"~tmp~" $@/lvznumbers-mode-pkg.el
	sed -re 's/;; Version: VERSION/;; Version: '"$(VERSION)"'/' $@/lvznumbers-mode.el > $@/"~tmp~"
	sed -re 's/\(defconst lvznumbers-version \"VERSION\"/\(defconst lvznumbers-version "'"$(VERSION)"'"/' $@/"~tmp~" > $@/lvznumbers-mode.el
	rm $@/"~tmp~"

install:
#emacs -e "(progn (package-initialize)(package-install \'lvznumbers-mode))"
	emacs -e '(progn (package-initialize) (package-install-file "'$(PACKAGE_DIR)'/lvznumbers-mode.el"))'

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)

#end

