EMACS ?= emacs
EASK ?= eask

.PHONY: clean checkdoc lint package install compile

ci: clean package install compile

package:
	@echo "Packaging..."
	$(EASK) package
	@echo "Copying snippets..."
	cp -r snippets $(EASK_PACKAGE_DIR)/

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

checkdoc:
	@echo "Run checkdoc..."
	$(EASK) lint checkdoc

lint:
	@echo "Run package-lint..."
	$(EASK) lint package

clean:
	$(EASK) clean all
