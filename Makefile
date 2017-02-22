
.PHONY: all
all: new-dummy.mdl

dummy.json: dummy.hs
	cabal run

new-dummy.mdl: dummy.json template.erb render.rb
	ruby render.rb
