Installation
=================


The following commands will download the hybrid-GODAS repository as well as any other child repositories that it requires:

```
git clone https://github.com/UMD-AOSC/hybrid-godas.git
cd hybrid-godas
git submodule update --init --recursive
```

For a new build, configure using an existing machine configuration file, or create your own configuration file

```
./configure.sh config/gcc.env
```

Build the source code:
```
cd build/gcc
make -j
```
