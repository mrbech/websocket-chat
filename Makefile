.PHONY: server client
server:
	stack run server-exe

client:
	stack run client-exe
