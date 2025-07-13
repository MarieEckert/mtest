all:
	@mkdir -p obj
	fpc src/mtest.pas -FE"obj" -Flmcfg_2/
	mv obj/mtest .
