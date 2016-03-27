devel:
	stack exec yesod devel

# Docker Image for Server

# Set root username and password for the db
# use environment variable to override the default value.
DB_ROOT ?= postgres
DB_PASSWORD ?= # empty 
DB_CONTAINER_NAME ?= fames_dbt # database test
DB_NAME ?= fames_test


run_db:
	docker run --name ${DB_CONTAINER_NAME} \
			-e POSTGRES_USER=${DB_ROOT} \
			-e POSTGRES_PASSWORD=${DB_PASSWORD} \
			-e POSTGRES_DB=${DB_NAME} \
			-d postgres:9.5

db_ip:
	@docker inspect --format '{{.NetworkSettings.IPAddress }}' ${DB_CONTAINER_NAME}



