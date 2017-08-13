.PHONY : all build clean

all: build

build:
	mkdir -p build
	. ../../config/env; cd build; cmake ../ ; make --no-print-directory

clean:
	rm -rf build
