default: dev

# https://github.com/truqu/real-world-elm/blob/2f6f083c631f4461a5b782d51822ae20450d6d2e/elm/Makefile
ELM_FILES = $(shell find . -path ./elm-stuff -prune -o -type f -name '*.elm')

SASS_FILES = $(shell find . -path ./node_modules -prune -o -type f -name '*.scss')

.PHONY: clean clean-deps server dev release test format-validate check sass-watch

main.js: $(ELM_FILES)
	yarn elm-make src/Main.elm --yes --warn $(ELM_MAKE_FLAGS) --output $@

css/main.css: $(SASS_FILES)
	yarn node-sass $(SASS_OPT_FLAGS) --include-path node_modules -r -o css --output-style $(SASS_OUTPUT_STYLE) --error-bell css/main.scss

dev : ELM_MAKE_FLAGS = --debug
dev : SASS_OUTPUT_STYLE = expanded
dev : SASS_OPT_FLAGS =
dev: main.js css/main.css

clean-deps:
	rm -rf elm-stuff

clean:
	rm -f main.js css/main.css main.min.js
	rm -rf elm-stuff/build-artifacts

server:
	yarn elm-live src/Main.elm --path-to-elm-make=node_modules/.bin/elm-make --output=main.js --debug --host=$(shell ipconfig getifaddr en0)

sass-watch : SASS_OPT_FLAGS = --watch
sass-watch : SASS_OUTPUT_STYLE = expanded
sass-watch: css/main.css
	touch css/main.scss

main.min.js : ELM_MAKE_FLAGS =
main.min.js: main.js
	yarn uglifyjs --output $@ $<

release : SASS_OPT_FLAGS =
release : SASS_OUTPUT_STYLE = compressed
release: clean main.min.js css/main.css

test:
	yarn elm-test

format-validate:
	yarn elm-format src/ --validate

check: test format-validate
