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
config/routes.gen: config/routes config/fa-routes config/fax-routes
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
