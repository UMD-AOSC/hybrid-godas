.PHONY : all build clean debug

all: build

build:
	mkdir -p build
	. ../../config/env; cd build; cmake ../ -DCMAKE_BUILD_TYPE=Release; make --no-print-directory

debug:
	mkdir -p build
	. ../../config/env; cd build; cmake ../ -DCMAKE_BUILD_TYPE=Debug; make --no-print-directory

clean:
	rm -rf build
