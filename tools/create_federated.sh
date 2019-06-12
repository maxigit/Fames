#!/usr/bin/env sh

schema=$1

# Generate a script to create federated tables  from a schea
# And the corresponding view 
cat <<EOD
-- * Create federated table to use with Drupal commerce
INSTALL PLUGIN federated SONAME 'ha_federatedx.so';


-- * Init
-- You should create a dedicated user on the remote database
-- with a host set.
drop server if exists dc;

create server 'dc' foreign data wrapper 'mysql' options
( HOST '172.17.0.1'
, DATABASE 'commerce'
, USER 'root'
, PASSWORD 'mu'
, PORT 3306
, OWNER 'root'
);

create database if not exists commerceX;

-- 
-- * Tables

use commerceX;
EOD

# replace engine with FEDERATED
sed -e 's/ENGINE=[^[:space:]]* /ENGINE="FEDERATED" CONNECTION="dc" /'\
    -e 's/AUTO_INCREMENT=[0-9]*//'\
    -e '/^\/\*\!/d' \
    ${schema} 

#Create views between  commerceX And fa
cat <<EOD
-- * Create view
-- Persistent doesn't allow to use table for a different database
-- We therefore we need to create a view in fa

use fa;
EOD

sed -n -e 's/CREATE TABLE `\(.*\)`.*/DROP TABLE IF EXISTS `dcx_\1`;\
DROP VIEW IF EXISTS `dcx_\1`;\
CREATE VIEW `dcx_\1` AS SELECT * FROM commerceX.`\1`;\n\
/p' \
  ${schema}


