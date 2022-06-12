.PHONY: server tui-client
server:
	stack run server-exe

tui-client:
	stack run tui-client-exe
