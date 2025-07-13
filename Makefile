all:
	@mkdir -p obj
	fpc src/mtest.pas -FE"obj"
	mv obj/mtest .