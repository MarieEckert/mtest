all:
	@mkdir -p obj
	fpc src/mtest.pas -FE"obj" -Flmcfg_2/ -g
	mv obj/mtest .
