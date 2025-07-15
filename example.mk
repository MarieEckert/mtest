MTEST_SOURCE = mtest/src

.PHONY: tests
tests:
	fpc $(MTEST_SOURCE)/mtest.pas -Fi$(MTEST_SOURCE) -Fi"tests/"
	@echo
	@echo "--> Running tests..."
	$(MTEST_SOURCE)/mtest
