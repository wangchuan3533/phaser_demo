all:
	git submodule update --init --recursive
	pushd dep/tmx && rm -rf build && mkdir -p build && pushd build && cmake .. && make && popd && popd
	pushd dep/uWebSockets && rm -rf build && mkdir -p build && pushd build && cmake .. && make && popd && popd
