.PHONY: test docs

# veritas

# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# veritas is free software: you can redistribute it and/or modify
# it under the terms of the Lesser GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# veritas is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# Lesser GNU General Public License for more details.

# You should have received a copy of the Lesser GNU General Public License
# along with veritas.  If not, see <https://www.gnu.org/licenses/>.

ELISP_MANUAL_CMD := '(progn (find-file "$(shell realpath ./docs/api-documentation.org)")(org-html-export-to-html)(message "finished publishing Veritas API documentation"))'
API_DOCS_CSS_URL := "https://jointhefreeworld.org/static-assets/api-docs/css/style.css"
DEPLOY_WWW = "/srv/http/jointhefreeworld.org"

# Logging helpers

define veritas-log-info
	@printf "\n\033[1m[INFO] $(shell date --rfc-3339=seconds) - %s\033[0m\n" $(1)
endef

# Target to format source code files according to the style guide
fmt:
	$(call veritas-log-info,"formatting all Scheme files")
	@-find . -maxdepth 8 -name "*.scm" -type f -exec guix style -f {} \;
	$(call veritas-log-info,"formatting Makefile")
	@-mbake format --config .mbake.toml Makefile

# Development REPL
repl:
	guix shell -m manifest.scm -- \
		guile \
		-L ./src \
		-c '((@ (ares server) run-nrepl-server))'

# Unit tests
test: fmt
	VERITAS_REPORTER="emoji-reporter" \
  VERITAS_RUN_BENCHMARKS=true \
  VERITAS_TEST_TYPE=unit \
  guix shell -m manifest.scm -- \
  guile --fresh-auto-compile -L ./src \
  -L ./test \
  -c '((@ (veritas runner) run-tests))'

# docs: Generate technical API documentation
docs:
	rm -rfv doc
	guix shell -m manifest.scm -- documenta api ./src/veritas
	guix shell -m manifest.scm -- texi2any -v --html \
		--css-ref=$(API_DOCS_CSS_URL) ./doc/api/index.texi
	mv ./index ./doc/api-dist

# deploy: Deploys the project and documentation to a web server directory.
deploy: docs
	guix shell -m manifest.scm -- emacs -Q --batch \
		--eval $(ELISP_MANUAL_CMD)
	mkdir -pv $(DEPLOY_WWW)/manuals/veritas-api-documentation
	rm -rfv $(DEPLOY_WWW)/manuals/veritas-api-documentation/index.html
	mv -v docs/api-documentation.html $(DEPLOY_WWW)/manuals/veritas-api-documentation/index.html
	rm -rfv $(DEPLOY_WWW)/api-docs/veritas
	mkdir -p $(DEPLOY_WWW)/api-docs/veritas
	mv doc/api-dist/* $(DEPLOY_WWW)/api-docs/veritas/
	rm -rfv doc
