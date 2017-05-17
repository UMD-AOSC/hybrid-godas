.PHONY : all build clean

all: build

build:
	mkdir -p build
	source ../../config/env; cd build; cmake ../ -DCMAKE_BUILD_TYPE=Release; make --no-print-directory

clean:
	rm -rf build