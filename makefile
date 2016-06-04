current:
	.stack-work/dist/x86_64-linux/Cabal-1.22.8.0/build/test/test

devel:
	mv -f distou dist
	stack exec yesod devel
	mv -f dist distou

# Docker Image for Server

# Set root username and password for the db
# use environment variable to override the default value.
DB_ROOT ?= root
DB_ROOT_PASSWORD ?= mu
DB_PASSWORD ?= test
DB_USER ?= test
DB_CONTAINER_NAME ?= fames_dbt # database test
DB_NAME ?= fames_devel


run_db:
	docker run --name ${DB_CONTAINER_NAME} \
			-e MYSQL_ROOT_PASSWORD=${DB_ROOT_PASSWORD} \
			-e MYSQL_USER=${DB_USER} \
			-e MYSQL_PASSWORD=${DB_PASSWORD} \
			-e MYSQL_DB=${DB_NAME} \
			-d mysql:5.6

db_ip:
	@docker inspect --format '{{.NetworkSettings.IPAddress }}' ${DB_CONTAINER_NAME}



