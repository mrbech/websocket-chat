.PHONY: server tui-client gui-client
server: build
	cabal run server-exe

tui-client: build
	cabal run tui-client-exe

gui-client: build
	cabal run gui-client-exe

build: websocket-chat.cabal

websocket-chat.cabal: package.yaml
	hpack
