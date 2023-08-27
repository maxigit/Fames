vim: src/Application.hs
current:
	.stack-work/dist/x86_64-linux/Cabal-1.22.8.0/build/test/test

devel:
	mv -f distou dist
	stack exec yesod devel
	mv -f dist distou

# Not used anymore. Use docker-compose instead 
## Docker Image for Server
#
## Set root username and password for the db
## use environment variable to override the default value.
#DB_ROOT ?= root
#DB_ROOT_PASSWORD ?= mu
#DB_PASSWORD ?= test
#DB_USER ?= test
#DB_CONTAINER_NAME ?= fames_dbt # database test
#DB_NAME ?= fames_devel
#
#
#dump_fa_test:
#	docker exec ${DB_CONTAINER_NAME} mysqldump Fames_test -u${DB_USER} -p${DB_PASSWORD} > test/sql/fa_test.sql
#
#restore_fa_test:
#	cat test/sql/fa_test.sql | docker exec -i ${DB_CONTAINER_NAME} mysql Fames_test -u${DB_USER} -p${DB_PASSWORD}
#run_db:
#	docker run --name ${DB_CONTAINER_NAME} \
#			-e MYSQL_ROOT_PASSWORD=${DB_ROOT_PASSWORD} \
#			-e MYSQL_USER=${DB_USER} \
#			-e MYSQL_PASSWORD=${DB_PASSWORD} \
#			-e MYSQL_DB=${DB_NAME} \
#			-d mysql:5.6
#
#db_ip:
#	@docker inspect --format '{{.NetworkSettings.IPAddress }}' ${DB_CONTAINER_NAME}



# From DB Handler and routes generatio
.PHONY: config/routes.gen
config/routes.gen: config/routes config/fa-routes config/fax-routes config/fames-routes
	cat $^ > $@


config/%-routes:
	stack exec FAGenerator $*

# docker-compose from mae super-project.
# For test, run a db from mae super-project
up:
	docker-compose down -v; docker-compose up -d
# To test manually (ie open a browser)
# use docker-compose of mae super-project.
# which will run everything, db, fa and Fames.


test: up
	stack test
	docker-compose down -v

BIN_PATH= $(shell stack exec which Fames)
.PHONY: install
install: install_bin install_static
install_bin:
	rsync -zz $(BIN_PATH) sinbad:prod/fames-config/bin/Fames
	rsync -zz .nixpkgs sinbad:prod/
install_static:
	rsync -zz static/ sinbad:prod/Fames/static

install_hot: install_bin_hot install_static_hot
install_bin_hot:
	rsync -zz $(BIN_PATH) sinbad:hot/fames-config/bin/Fames
install_bin_stag:
	rsync -zz $(BIN_PATH) sinbad:stag/fames-config/bin/Fames
install_static_stag:
	rsync -zz -a static/ sinbad:stag/Fames/static

restart_sinbad: install
	ssh sinbad docker restart prod_fames_1
restart_hot: install_hot
	ssh sinbad docker restart hot_fames_1
restart_stag: install_stag
	ssh sinbad docker restart stag_fames_1

install_all: build restart_sinbad restart_hot

GHCID_EXTRA= --reload=.ghcid-reload --reload=../fames-config/staging.yml --restart=.ghcid-restart # -h65
GHCID= LC_ALL=C.UTF-8 ghcid 
ghcid-old:
	$(GHCID) --command="stack exec ghci --test -- -iapp -ilegacy -isrc -ifay-shared -itest -iconfig/fa -hide-package=cryptonite  -w test/Spec.hs"   --test ":main --rerun --color"

ghcid-focus: reset-hspec
	$(GHCID) --command="stack zghci :ghcid" --test ":main --color -m@focus --rerun --rerun-all-on-success" $(GHCID_EXTRA)
ghcid-test-all: reset-hspec
	$(GHCID) --command="stack zghci :ghcid" --test ":main --color" $(GHCID_EXTRA) 
ghcid-test-force:
	$(GHCID) --command="stack zghci :test --ghc-options=-w --ghc-options=-fdefer-type-errors" --test ":main --color"
ghcid:
	$(GHCID) --command="stack zghci --ghc-options=-w" $(GHCID_EXTRA)
ghcid-run:
	$(GHCID) --command="stack zghci" --test "appMain" $(GHCID_EXTRA)
ghcid-run-force:
	$(GHCID) --command="stack zghci --ghc-options=-w --ghc-options=-fdefer-type-errors" --test "appMain" $(GHCID_EXTRA)
ghcid-run-now:
	$(GHCID) --command="stack zghci --ghc-options=-w" --test "appMain" $(GHCID_EXTRA)

reset-hspec:
	rm -rf .hspc-failures
# Generate FrontAccounting model
config/tables/xx%: config/fa-models

config/fa/FAxx%.hs: config/tables/xx%
	@echo '{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}'  > $@
	@echo "module FAxx$* where" >> $@
	@echo  >> $@
	@echo 'import ClassyPrelude.Yesod' >> $@
	@echo 'import Database.Persist.Quasi' >> $@
	@echo '' >> $@
	@echo 'share [mkPersist sqlSettings] -- , mkMigrate "migrateAll"]' >> $@
	@echo '    $$(persistFileWith lowerCaseSettings "$<")' >> $@

FAS=$(patsubst config/tables/%,config/fa/FA%.hs,$(wildcard config/tables/xx*))

FA.hs: $(FAS)
	echo 'module FA (module X) where ' > $@
	for file in $(patsubst config/fa/%.hs,%, $?);do echo import $$file as X >> $@; done 

clean_fa:
	rm -rf config/fa/*


# Split fa-models to one file per table
# should be faster to compile
gen_tables:
	mkdir -p config/tables
	cd config; csplit fa-models /^$$/ {*}
	mv config/xx* config/tables

restart: 
	cd ..; docker-compose restart fames

brestart: build restart
build:
	stack build

VAR_YML= prod-var.yml
# VAR_YML = variations.yml
CONFIG_DIR= /home/max/devel/mae/fames-config
RUN_CONFIG= ${CONFIG_DIR}/development.yml ${CONFIG_DIR}/staging.yml ${CONFIG_DIR}/${VAR_YML} ${CONFIG_DIR}/default.yml ${CONFIG_DIR}/item-cost.yml
run:
	stack exec  Fames -- ${RUN_CONFIG}

build_profile:
	stack build Fames:exe:Fames  --profile --work-dir .stack-profile --flag Fames:-dev --library-profiling --executable-profiling

profile: build_profile
	stack exec --work-dir .stack-profile Fames --library-profiling -- $(RUN_CONFIG) +RTS -p
	mkdir -p .prof
	mv Fames.hp Fames.prof  .prof

run_with_stack_trace: build_profile
	stack exec --work-dir .stack-profile Fames --library-profiling -- $(RUN_CONFIG) +RTS -hy -p -xc -M500M
	mkdir -p .prof
	mv Fames.hp Fames.prof  .prof

# Generate federated tables and view
# use the local copy of the commerce database database 
sql/commerce.schema:
	mysqldump -uroot -pmu commerce --no-data  -h127.0.0.1 > $@

sql/federated_dc.sql: sql/commerce.schema tools/create_federated.sh
	tools/create_federated.sh sql/commerce.schema > $@

ctags:
	LC_ALL=C.UTF-8 haskdogs

# incremental
%.itags:
	LC_ALL=C.UTF-8 haskdogs -i $* --hasktags-args "-x -c -a" | sort -u -o tags tags

pin_nix: .nixpkgs

.PHONY: .nixpkgs
.nixpkgs:
	cat ~/.nix-defexpr/channels/nixpkgs/.git-revision | sed 's/.*/"&"/' > $@
