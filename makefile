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
config/routes.gen: config/routes config/fa-routes config/fax-routes config/fames-routes config/dc-routes
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

.PHONY: install
install:
	rsync -z .stack-work/install/x86_64-linux/lts-8.24/8.0.2/bin/Fames sinbad:prod/fames-config/bin/Fames

install_local:
	rsync -z .stack-work/install/x86_64-linux/lts-8.24/8.0.2/bin/Fames ../fames-config/bin/Fames

GHCID_EXTRA= --reload=.ghcid-reload --reload=../fames-config/staging.yml --restart=.ghcid-restart # -h65
GHCID= LC_ALL=C.UTF-8 ghcid 
ghcid-old:
	$(GHCID) --command="stack exec ghci --test -- -iapp -ilegacy -isrc -ifay-shared -itest -iconfig/fa -hide-package=cryptonite  -w test/Spec.hs"   --test ":main --rerun --color"

ghcid-focus: reset-hspec
	$(GHCID) --command="stack ghci :ghcid" --test ":main --color -m@focus --rerun --rerun-all-on-success" $(GHCID_EXTRA)
ghcid-test-all: reset-hspec
	$(GHCID) --command="stack ghci :ghcid" --test ":main --color" $(GHCID_EXTRA) 
ghcid-test-force:
	$(GHCID) --command="stack ghci :test --ghc-options=-w --ghc-options=-fdefer-type-errors" --test ":main --color"
ghcid:
	$(GHCID) --command="stack ghci --ghc-options=-w" $(GHCID_EXTRA)
ghcid-run:
	$(GHCID) --command="stack ghci" --test "appMain" $(GHCID_EXTRA)
ghcid-run-force:
	$(GHCID) --command="stack ghci --ghc-options=-w --ghc-options=-fdefer-type-errors" --test "appMain" $(GHCID_EXTRA)
ghcid-run-now:
	$(GHCID) --command="stack ghci --ghc-options=-w" --test "appMain" $(GHCID_EXTRA)

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

# Generate Drupal Commerce model
config/dc-tables/dcx%: config/dc-models

dc/DC%.hs: config/dc-tables/dcx%
	@echo '{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}'  > $@
	@echo "module DC$* where" >> $@
	@echo  >> $@
	@echo 'import ClassyPrelude.Yesod' >> $@
	@echo 'import Database.Persist.Quasi' >> $@
	@echo '' >> $@
	@echo 'share [mkPersist sqlSettings] -- , mkMigrate "migrateAll"]' >> $@
	@echo '    $$(persistFileWith lowerCaseSettings "$<")' >> $@

DCS=$(patsubst config/dc-tables/dcx%,dc/DC%.hs,$(wildcard config/dc-tables/dcx*))

DC.hs: $(DCS)
	echo '{-# OPTIONS_GHC -fno-warn-unused-imports #-}' > $@
	echo 'module DC (module X) where ' >> $@
	mkdir -p config/dc
	for file in $(patsubst dc/%.hs,%, $?);do echo import $$file as X >> $@; done

clean_dc:
	rm -rf dc/*


# Split dc-models to one file per table
# should be faster to compile
gen_dc_tables:
	cd config; csplit -fdcx dc-models '/^$$/' '{*}'
	mkdir -p config/dc-tables
	mkdir -p config/dc
	mv config/dcx* config/dc-tables

restart: 
	cd ..; docker-compose restart fames

brestart: build restart
build:
	stack build

RUN_CONFIG= ../fames-config/development.yml ../fames-config/staging.yml ../fames-config/variations.yml ../fames-config/default.yml ../fames-config/item-cost.yml
run:
	stack exec  Fames -- ${RUN_CONFIG}

build_profile:
	stack build --profile --work-dir .stack-profile --flag Fames:-dev --library-profiling --executable-profiling

profile: build_profile
	stack exec --work-dir .stack-profile Fames -- $(RUN_CONFIG) +RTS -p
	mkdir -p .prof
	mv Fames.hp Fames.prof  .prof

run_with_stack_trace: build_profile
	stack exec --work-dir .stack-profile Fames -- $(RUN_CONFIG) +RTS -hy -p -xc
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

