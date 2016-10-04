all:
	protoc --proto_path=. --cpp_out=cpp messages.proto 
	cp cpp/* ../../server_websocketpp/src/
