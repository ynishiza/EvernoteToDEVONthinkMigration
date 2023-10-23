SHELL=/usr/bin/env bash
# e.g. function usage
# 	SRCFILES=$(call get_source_in_directory,src)					get all .hs files in ./src
# 	SRCPATHS=$(call map,abspath,$(SRCFILES)) 							map to absolute path
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(1)/*.hs)

PROJECTNAME=EvernoteToDEVONThink
DOCUMENTATION_DIR=docs

default: help

.PHONY: install
install: ## Install 
	make compile

.PHONY: uninstall
uninstall: ## Uninstall
	make clean
	stack purge

.PHONY: compile
compile: ## Compile
	stack build 

.PHONY: compile-profile
compile-profile: ## Compile with profiler
	stack build --profile 

.PHONY: document
document: ## Build haddock documentation 
	stack haddock --no-haddock-deps --haddock-arguments "-o $(DOCUMENTATION_DIR)" 

.PHONY: lint
lint: ## Lint
	@stack exec -- hlint --verbose -h=.hlint.yaml src test

.PHONY: benchmark
benchmark: ## Run benchmark
	@stack bench

.PHONY: test
test: ## Run tests
	@echo "Tests"
	@stack test --test-arguments="+RTS -N"

.PHONY: ide
ide: ## Check IDE setup
	haskell-language-server-wrapper ./src/
	haskell-language-server-wrapper ./test/

.PHONY: debug
debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo PROJECTNAME=$(PROJECTNAME)

.PHONY: clean
clean: ## Clean
	stack clean
	rm -f *.hi *.o *.cabal *.png *.svg *.html
	rm -rf $(DOCUMENTATION_DIR)
	find src test \
		-iname '*.o' \
		-o -iname '*.hi' \
		-o -iname '*dump-*' \
		-delete

.PHONY: help
help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
