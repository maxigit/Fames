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
-- MySQL dump 10.13  Distrib 5.7.26, for Linux (x86_64)
--
-- Host: 127.0.0.1    Database: commerce
-- ------------------------------------------------------
-- Server version	5.5.5-10.1.18-MariaDB-1~jessie


--
-- Table structure for table `accesslog`
--

DROP TABLE IF EXISTS `accesslog`;
CREATE TABLE `accesslog` (
  `aid` int(11) NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique accesslog ID.',
  `sid` varchar(128) NOT NULL DEFAULT '' COMMENT 'Browser session ID of user that visited page.',
  `title` varchar(255) DEFAULT NULL COMMENT 'Title of page visited.',
  `path` varchar(255) DEFAULT NULL COMMENT 'Internal path to page visited (relative to Drupal root.)',
  `url` text COMMENT 'Referrer URI.',
  `hostname` varchar(128) DEFAULT NULL COMMENT 'Hostname of user that visited the page.',
  `uid` int(10) unsigned DEFAULT '0' COMMENT 'User users.uid that visited the page.',
  `timer` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Time in milliseconds that the page took to load.',
  `timestamp` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Timestamp of when the page was visited.',
  PRIMARY KEY (`aid`),
  KEY `accesslog_timestamp` (`timestamp`),
  KEY `uid` (`uid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores site access information for statistics.';

--
-- Table structure for table `actions`
--

DROP TABLE IF EXISTS `actions`;
CREATE TABLE `actions` (
  `aid` varchar(255) NOT NULL DEFAULT '0' COMMENT 'Primary Key: Unique actions ID.',
  `type` varchar(32) NOT NULL DEFAULT '' COMMENT 'The object that that action acts on (node, user, comment, system or custom types.)',
  `callback` varchar(255) NOT NULL DEFAULT '' COMMENT 'The callback function that executes when the action runs.',
  `parameters` longblob NOT NULL COMMENT 'Parameters to be passed to the callback function.',
  `label` varchar(255) NOT NULL DEFAULT '0' COMMENT 'Label of the action.',
  PRIMARY KEY (`aid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores action information.';

--
-- Table structure for table `advanced_help_index`
--

DROP TABLE IF EXISTS `advanced_help_index`;
CREATE TABLE `advanced_help_index` (
  `sid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary key to give to the search engine for this topic.',
  `module` varchar(255) NOT NULL DEFAULT '' COMMENT 'The module that owns this topic.',
  `topic` varchar(255) NOT NULL DEFAULT '' COMMENT 'The topic id.',
  `language` varchar(12) NOT NULL DEFAULT '' COMMENT 'The language this search index relates to.',
  PRIMARY KEY (`sid`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores search index correlations for advanced help topics.';

--
-- Table structure for table `authcache_p13n_key_value`
--

DROP TABLE IF EXISTS `authcache_p13n_key_value`;
CREATE TABLE `authcache_p13n_key_value` (
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique key name.',
  `collection` varchar(63) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique collection name.',
  `value` longblob COMMENT 'Serialized data.',
  PRIMARY KEY (`name`,`collection`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Generic key-value store for caching things not separated...';

--
-- Table structure for table `authmap`
--

DROP TABLE IF EXISTS `authmap`;
CREATE TABLE `authmap` (
  `aid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique authmap ID.',
  `uid` int(11) NOT NULL DEFAULT '0' COMMENT 'User’s users.uid.',
  `authname` varchar(128) NOT NULL DEFAULT '' COMMENT 'Unique authentication name.',
  `module` varchar(128) NOT NULL DEFAULT '' COMMENT 'Module which is controlling the authentication.',
  PRIMARY KEY (`aid`),
  UNIQUE KEY `authname` (`authname`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores distributed authentication mapping.';

--
-- Table structure for table `batch`
--

DROP TABLE IF EXISTS `batch`;
CREATE TABLE `batch` (
  `bid` int(10) unsigned NOT NULL COMMENT 'Primary Key: Unique batch ID.',
  `token` varchar(64) NOT NULL COMMENT 'A string token generated against the current user’s session id and the batch id, used to ensure that only the user who submitted the batch can effectively access it.',
  `timestamp` int(11) NOT NULL COMMENT 'A Unix timestamp indicating when this batch was submitted for processing. Stale batches are purged at cron time.',
  `batch` longblob COMMENT 'A serialized array containing the processing data for the batch.',
  PRIMARY KEY (`bid`),
  KEY `token` (`token`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores details about batches (processes that run in...';

--
-- Table structure for table `block`
--

DROP TABLE IF EXISTS `block`;
CREATE TABLE `block` (
  `bid` int(11) NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique block ID.',
  `module` varchar(64) NOT NULL DEFAULT '' COMMENT 'The module from which the block originates; for example, ’user’ for the Who’s Online block, and ’block’ for any custom blocks.',
  `delta` varchar(32) NOT NULL DEFAULT '0' COMMENT 'Unique ID for block within a module.',
  `theme` varchar(64) NOT NULL DEFAULT '' COMMENT 'The theme under which the block settings apply.',
  `status` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Block enabled status. (1 = enabled, 0 = disabled)',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'Block weight within region.',
  `region` varchar(64) NOT NULL DEFAULT '' COMMENT 'Theme region within which the block is set.',
  `custom` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Flag to indicate how users may control visibility of the block. (0 = Users cannot control, 1 = On by default, but can be hidden, 2 = Hidden by default, but can be shown)',
  `visibility` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Flag to indicate how to show blocks on pages. (0 = Show on all pages except listed pages, 1 = Show only on listed pages, 2 = Use custom PHP code to determine visibility)',
  `pages` text NOT NULL COMMENT 'Contents of the "Pages" block; contains either a list of paths on which to include/exclude the block or PHP code, depending on "visibility" setting.',
  `title` varchar(255) NOT NULL DEFAULT '' COMMENT 'Custom title for the block. (Empty string will use block default title, <none> will remove the title, text will cause block to use specified title.)',
  `cache` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'Binary flag to indicate block cache mode. (-2: Custom cache, -1: Do not cache, 1: Cache per role, 2: Cache per user, 4: Cache per page, 8: Block cache global) See DRUPAL_CACHE_* constants in ../includes/common.inc for more detailed information.',
  PRIMARY KEY (`bid`),
  UNIQUE KEY `tmd` (`theme`,`module`,`delta`),
  KEY `list` (`theme`,`status`,`region`,`weight`,`module`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores block settings, such as region and visibility...';

--
-- Table structure for table `block_current_search`
--

DROP TABLE IF EXISTS `block_current_search`;
CREATE TABLE `block_current_search` (
  `delta` varchar(32) NOT NULL COMMENT 'The block’s unique delta within module, from block.delta.',
  `searcher` varchar(128) NOT NULL COMMENT 'The machine-readable name of the searcher.',
  PRIMARY KEY (`delta`),
  KEY `searcher` (`searcher`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Sets up display criteria for blocks based on searcher';

--
-- Table structure for table `block_custom`
--

DROP TABLE IF EXISTS `block_custom`;
CREATE TABLE `block_custom` (
  `bid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The block’s block.bid.',
  `body` longtext COMMENT 'Block contents.',
  `info` varchar(128) NOT NULL DEFAULT '' COMMENT 'Block description.',
  `format` varchar(255) DEFAULT NULL COMMENT 'The filter_format.format of the block body.',
  PRIMARY KEY (`bid`),
  UNIQUE KEY `info` (`info`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores contents of custom-made blocks.';

--
-- Table structure for table `block_node_type`
--

DROP TABLE IF EXISTS `block_node_type`;
CREATE TABLE `block_node_type` (
  `module` varchar(64) NOT NULL COMMENT 'The block’s origin module, from block.module.',
  `delta` varchar(32) NOT NULL COMMENT 'The block’s unique delta within module, from block.delta.',
  `type` varchar(32) NOT NULL COMMENT 'The machine-readable name of this type from node_type.type.',
  PRIMARY KEY (`module`,`delta`,`type`),
  KEY `type` (`type`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Sets up display criteria for blocks based on content types';

--
-- Table structure for table `block_role`
--

DROP TABLE IF EXISTS `block_role`;
CREATE TABLE `block_role` (
  `module` varchar(64) NOT NULL COMMENT 'The block’s origin module, from block.module.',
  `delta` varchar(32) NOT NULL COMMENT 'The block’s unique delta within module, from block.delta.',
  `rid` int(10) unsigned NOT NULL COMMENT 'The user’s role ID from users_roles.rid.',
  PRIMARY KEY (`module`,`delta`,`rid`),
  KEY `rid` (`rid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Sets up access permissions for blocks based on user roles';

--
-- Table structure for table `blocked_ips`
--

DROP TABLE IF EXISTS `blocked_ips`;
CREATE TABLE `blocked_ips` (
  `iid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: unique ID for IP addresses.',
  `ip` varchar(40) NOT NULL DEFAULT '' COMMENT 'IP address',
  PRIMARY KEY (`iid`),
  KEY `blocked_ip` (`ip`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores blocked IP addresses.';

--
-- Table structure for table `cache`
--

DROP TABLE IF EXISTS `cache`;
CREATE TABLE `cache` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Generic cache table for caching things not separated out...';

--
-- Table structure for table `cache_admin_menu`
--

DROP TABLE IF EXISTS `cache_admin_menu`;
CREATE TABLE `cache_admin_menu` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for Administration menu to store client-side...';

--
-- Table structure for table `cache_authcache_debug`
--

DROP TABLE IF EXISTS `cache_authcache_debug`;
CREATE TABLE `cache_authcache_debug` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for authcache debug.';

--
-- Table structure for table `cache_authcache_key`
--

DROP TABLE IF EXISTS `cache_authcache_key`;
CREATE TABLE `cache_authcache_key` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for mapping sessions to authcache keys.';

--
-- Table structure for table `cache_authcache_p13n`
--

DROP TABLE IF EXISTS `cache_authcache_p13n`;
CREATE TABLE `cache_authcache_p13n` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for authcache p13n.';

--
-- Table structure for table `cache_block`
--

DROP TABLE IF EXISTS `cache_block`;
CREATE TABLE `cache_block` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for the Block module to store already built...';

--
-- Table structure for table `cache_bootstrap`
--

DROP TABLE IF EXISTS `cache_bootstrap`;
CREATE TABLE `cache_bootstrap` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for data required to bootstrap Drupal, may be...';

--
-- Table structure for table `cache_commerce_shipping_rates`
--

DROP TABLE IF EXISTS `cache_commerce_shipping_rates`;
CREATE TABLE `cache_commerce_shipping_rates` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Order ID and shipping method the rates are for.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for the temporary storage of base calculated...';

--
-- Table structure for table `cache_display_cache`
--

DROP TABLE IF EXISTS `cache_display_cache`;
CREATE TABLE `cache_display_cache` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Generic cache table for caching things not separated out...';

--
-- Table structure for table `cache_entity_comment`
--

DROP TABLE IF EXISTS `cache_entity_comment`;
CREATE TABLE `cache_entity_comment` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store comment entity records.';

--
-- Table structure for table `cache_entity_file`
--

DROP TABLE IF EXISTS `cache_entity_file`;
CREATE TABLE `cache_entity_file` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store file entity records.';

--
-- Table structure for table `cache_entity_message`
--

DROP TABLE IF EXISTS `cache_entity_message`;
CREATE TABLE `cache_entity_message` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store message entity records.';

--
-- Table structure for table `cache_entity_message_type`
--

DROP TABLE IF EXISTS `cache_entity_message_type`;
CREATE TABLE `cache_entity_message_type` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store message_type entity records.';

--
-- Table structure for table `cache_entity_message_type_category`
--

DROP TABLE IF EXISTS `cache_entity_message_type_category`;
CREATE TABLE `cache_entity_message_type_category` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store message_type_category entity...';

--
-- Table structure for table `cache_entity_node`
--

DROP TABLE IF EXISTS `cache_entity_node`;
CREATE TABLE `cache_entity_node` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store node entity records.';

--
-- Table structure for table `cache_entity_taxonomy_term`
--

DROP TABLE IF EXISTS `cache_entity_taxonomy_term`;
CREATE TABLE `cache_entity_taxonomy_term` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store taxonomy_term entity records.';

--
-- Table structure for table `cache_entity_taxonomy_vocabulary`
--

DROP TABLE IF EXISTS `cache_entity_taxonomy_vocabulary`;
CREATE TABLE `cache_entity_taxonomy_vocabulary` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store taxonomy_vocabulary entity...';

--
-- Table structure for table `cache_entity_user`
--

DROP TABLE IF EXISTS `cache_entity_user`;
CREATE TABLE `cache_entity_user` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store user entity records.';

--
-- Table structure for table `cache_field`
--

DROP TABLE IF EXISTS `cache_field`;
CREATE TABLE `cache_field` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Generic cache table for caching things not separated out...';

--
-- Table structure for table `cache_filter`
--

DROP TABLE IF EXISTS `cache_filter`;
CREATE TABLE `cache_filter` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for the Filter module to store already...';

--
-- Table structure for table `cache_form`
--

DROP TABLE IF EXISTS `cache_form`;
CREATE TABLE `cache_form` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for the form system to store recently built...';

--
-- Table structure for table `cache_image`
--

DROP TABLE IF EXISTS `cache_image`;
CREATE TABLE `cache_image` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store information about image...';

--
-- Table structure for table `cache_libraries`
--

DROP TABLE IF EXISTS `cache_libraries`;
CREATE TABLE `cache_libraries` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table to store library information.';

--
-- Table structure for table `cache_menu`
--

DROP TABLE IF EXISTS `cache_menu`;
CREATE TABLE `cache_menu` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for the menu system to store router...';

--
-- Table structure for table `cache_metatag`
--

DROP TABLE IF EXISTS `cache_metatag`;
CREATE TABLE `cache_metatag` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for the generated meta tag output.';

--
-- Table structure for table `cache_page`
--

DROP TABLE IF EXISTS `cache_page`;
CREATE TABLE `cache_page` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table used to store compressed pages for anonymous...';

--
-- Table structure for table `cache_path`
--

DROP TABLE IF EXISTS `cache_path`;
CREATE TABLE `cache_path` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for path alias lookup.';

--
-- Table structure for table `cache_path_alias`
--

DROP TABLE IF EXISTS `cache_path_alias`;
CREATE TABLE `cache_path_alias` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for URL aliases';

--
-- Table structure for table `cache_path_source`
--

DROP TABLE IF EXISTS `cache_path_source`;
CREATE TABLE `cache_path_source` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for URL aliases';

--
-- Table structure for table `cache_rules`
--

DROP TABLE IF EXISTS `cache_rules`;
CREATE TABLE `cache_rules` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for the rules engine to store configured items.';

--
-- Table structure for table `cache_token`
--

DROP TABLE IF EXISTS `cache_token`;
CREATE TABLE `cache_token` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for token information.';

--
-- Table structure for table `cache_update`
--

DROP TABLE IF EXISTS `cache_update`;
CREATE TABLE `cache_update` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for the Update module to store information...';

--
-- Table structure for table `cache_views`
--

DROP TABLE IF EXISTS `cache_views`;
CREATE TABLE `cache_views` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Generic cache table for caching things not separated out...';

--
-- Table structure for table `cache_views_data`
--

DROP TABLE IF EXISTS `cache_views_data`;
CREATE TABLE `cache_views_data` (
  `cid` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry should expire, or 0 for never.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT '1' COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Cache table for views to store pre-rendered queries,...';

--
-- Table structure for table `cmp_menu_perms`
--

DROP TABLE IF EXISTS `cmp_menu_perms`;
CREATE TABLE `cmp_menu_perms` (
  `menu_path` varchar(255) NOT NULL DEFAULT '' COMMENT 'The menu path to which the permission will be used as the access arguments to be passed to user_access()',
  `cmp_permission_key` varchar(255) DEFAULT NULL COMMENT 'The perm_key from cmp_permissions for the permission that should be applied to the given menu path',
  PRIMARY KEY (`menu_path`),
  KEY `cmp_permission_key` (`cmp_permission_key`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores the custom permission to be used with user_access...';

--
-- Table structure for table `cmp_permissions`
--

DROP TABLE IF EXISTS `cmp_permissions`;
CREATE TABLE `cmp_permissions` (
  `perm_key` varchar(255) NOT NULL COMMENT 'The permission key used by the system',
  `perm_name` varchar(255) NOT NULL COMMENT 'The human readable name of the permission',
  `description` text COMMENT 'The description of the permission',
  PRIMARY KEY (`perm_key`),
  KEY `perm_name` (`perm_name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores custom permissions that this module creates...';

--
-- Table structure for table `comment`
--

DROP TABLE IF EXISTS `comment`;
CREATE TABLE `comment` (
  `cid` int(11) NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique comment ID.',
  `pid` int(11) NOT NULL DEFAULT '0' COMMENT 'The comment.cid to which this comment is a reply. If set to 0, this comment is not a reply to an existing comment.',
  `nid` int(11) NOT NULL DEFAULT '0' COMMENT 'The node.nid to which this comment is a reply.',
  `uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid who authored the comment. If set to 0, this comment was created by an anonymous user.',
  `subject` varchar(64) NOT NULL DEFAULT '' COMMENT 'The comment title.',
  `hostname` varchar(128) NOT NULL DEFAULT '' COMMENT 'The author’s host name.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'The time that the comment was created, as a Unix timestamp.',
  `changed` int(11) NOT NULL DEFAULT '0' COMMENT 'The time that the comment was last edited, as a Unix timestamp.',
  `status` tinyint(3) unsigned NOT NULL DEFAULT '1' COMMENT 'The published status of a comment. (0 = Not Published, 1 = Published)',
  `thread` varchar(255) NOT NULL COMMENT 'The vancode representation of the comment’s place in a thread.',
  `name` varchar(60) DEFAULT NULL COMMENT 'The comment author’s name. Uses users.name if the user is logged in, otherwise uses the value typed into the comment form.',
  `mail` varchar(64) DEFAULT NULL COMMENT 'The comment author’s e-mail address from the comment form, if user is anonymous, and the ’Anonymous users may/must leave their contact information’ setting is turned on.',
  `homepage` varchar(255) DEFAULT NULL COMMENT 'The comment author’s home page address from the comment form, if user is anonymous, and the ’Anonymous users may/must leave their contact information’ setting is turned on.',
  `language` varchar(12) NOT NULL DEFAULT '' COMMENT 'The languages.language of this comment.',
  PRIMARY KEY (`cid`),
  KEY `comment_status_pid` (`pid`,`status`),
  KEY `comment_num_new` (`nid`,`status`,`created`,`cid`,`thread`),
  KEY `comment_uid` (`uid`),
  KEY `comment_nid_language` (`nid`,`language`),
  KEY `comment_created` (`created`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores comments and associated data.';

--
-- Table structure for table `commerce_addressbook_defaults`
--

DROP TABLE IF EXISTS `commerce_addressbook_defaults`;
CREATE TABLE `commerce_addressbook_defaults` (
  `cad_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Serial numeric ID of the default customer profile of a specific type.',
  `profile_id` int(10) unsigned NOT NULL COMMENT 'Serial numeric ID of the customer profile in the local database.',
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT 'The customer profile type.',
  `uid` int(10) unsigned NOT NULL COMMENT 'Serial numeric ID of the customer profile in the local database.',
  PRIMARY KEY (`cad_id`),
  KEY `profile_id` (`profile_id`),
  KEY `uid` (`uid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores addressbook defaults by customer profile type.';

--
-- Table structure for table `commerce_autosku_patterns`
--

DROP TABLE IF EXISTS `commerce_autosku_patterns`;
CREATE TABLE `commerce_autosku_patterns` (
  `product_type` varchar(32) NOT NULL COMMENT 'Product type identifier.',
  `pattern` longtext NOT NULL COMMENT 'Token replacement pattern.',
  `advanced` longblob COMMENT 'Serialized array of additional settings.',
  PRIMARY KEY (`product_type`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='TODO: please describe this table!';

--
-- Table structure for table `commerce_calculated_price`
--

DROP TABLE IF EXISTS `commerce_calculated_price`;
CREATE TABLE `commerce_calculated_price` (
  `module` varchar(255) NOT NULL DEFAULT '' COMMENT 'The name of the module performing the calculation.',
  `module_key` mediumtext NOT NULL COMMENT 'A module specific key useful for indicating the context of a particular calculation, e.g. the IDs of Rules evaluated to produce the calculated price.',
  `entity_type` varchar(255) NOT NULL DEFAULT '' COMMENT 'The type of entity this price belongs to.',
  `entity_id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The entity ID of the object this price belongs to.',
  `field_name` varchar(32) NOT NULL DEFAULT '' COMMENT 'The name of the field the calculated price relates to.',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The languages.language of the entity.',
  `delta` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The sequence number for this data item, used for multi-value fields',
  `amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `data` longtext COMMENT 'A serialized array of additional price data.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the price was calculated.',
  KEY `module` (`module`),
  KEY `entity_type` (`entity_type`),
  KEY `entity_id` (`entity_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores pre-calculated dynamic prices.';

--
-- Table structure for table `commerce_checkout_pane`
--

DROP TABLE IF EXISTS `commerce_checkout_pane`;
CREATE TABLE `commerce_checkout_pane` (
  `pane_id` varchar(255) NOT NULL COMMENT 'The machine readable name of the order state.',
  `page` varchar(255) NOT NULL DEFAULT '1' COMMENT 'The ID of the checkout page on which this pane appears.',
  `fieldset` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'Boolean value indicating whether or not the pane should appear in a fieldset.',
  `collapsible` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Boolean value indicating whether or not the pane should appear collapsed.',
  `collapsed` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Boolean value indicating whether or not the pane should appear collapsed.',
  `weight` smallint(6) NOT NULL DEFAULT '0' COMMENT 'The sorting weight of the status for lists of statuses.',
  `enabled` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'Boolean value indicating whether or not the pane is enabled.',
  `review` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'Boolean value indicating whether or not the pane should appear on the checkout review.',
  PRIMARY KEY (`pane_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Checkout pane configuration data.';

--
-- Table structure for table `commerce_customer_profile`
--

DROP TABLE IF EXISTS `commerce_customer_profile`;
CREATE TABLE `commerce_customer_profile` (
  `profile_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a customer profile.',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The current commerce_customer_profile_revision.revision_id version identifier.',
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT 'The commerce_customer_profile_type.type of this profile.',
  `uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that this profile belongs to.',
  `status` int(11) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether the profile is active or not.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the profile was created.',
  `changed` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the profile was most recently saved.',
  `data` longblob COMMENT 'A serialized array of additional data.',
  PRIMARY KEY (`profile_id`),
  UNIQUE KEY `revision_id` (`revision_id`),
  KEY `uid` (`uid`),
  KEY `customer_profile_type` (`type`),
  KEY `uid_by_type` (`uid`,`type`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='The base table for customer profiles.';

--
-- Table structure for table `commerce_customer_profile_revision`
--

DROP TABLE IF EXISTS `commerce_customer_profile_revision`;
CREATE TABLE `commerce_customer_profile_revision` (
  `profile_id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The commerce_customer_profile.customer_id of the profile this revision belongs to.',
  `revision_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for this revision.',
  `revision_uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that created this profile at this revision.',
  `status` int(11) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether the profile is active or not.',
  `log` longtext NOT NULL COMMENT 'The log entry explaining the changes in this version.',
  `revision_timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when this revision was created.',
  `data` longblob COMMENT 'A serialized array of additional data.',
  PRIMARY KEY (`revision_id`),
  KEY `profile_id` (`profile_id`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Saves information about each saved revision of a commerce...';

--
-- Table structure for table `commerce_discount`
--

DROP TABLE IF EXISTS `commerce_discount`;
CREATE TABLE `commerce_discount` (
  `discount_id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'The internal identifier for any discount.',
  `name` varchar(64) NOT NULL DEFAULT '' COMMENT 'The machine name of the discount.',
  `label` varchar(255) NOT NULL DEFAULT '' COMMENT 'The label of the discount.',
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT 'The discount type (bundle).',
  `status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'Whether the discount is active.',
  `export_status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'The exportable status of the entity.',
  `module` varchar(255) DEFAULT NULL COMMENT 'The name of the providing module if the entity has been defined in code.',
  `component_title` varchar(255) DEFAULT NULL COMMENT 'The component price title',
  PRIMARY KEY (`discount_id`),
  UNIQUE KEY `name` (`name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='The base table for discounts.';

--
-- Table structure for table `commerce_discount_offer`
--

DROP TABLE IF EXISTS `commerce_discount_offer`;
CREATE TABLE `commerce_discount_offer` (
  `discount_offer_id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'The internal identifier for any discount offer.',
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT 'The discount offer type (bundle).',
  PRIMARY KEY (`discount_offer_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='The base table for discount offers.';

--
-- Table structure for table `commerce_flat_rate_service`
--

DROP TABLE IF EXISTS `commerce_flat_rate_service`;
CREATE TABLE `commerce_flat_rate_service` (
  `name` varchar(32) NOT NULL DEFAULT '' COMMENT 'The machine-name of the flat rate service.',
  `title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The human-readable title of the flat rate service.',
  `display_title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The title of the flat rate service displayed to customers.',
  `description` mediumtext NOT NULL COMMENT 'A brief description of the flat rate service.',
  `rules_component` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether or not this service should have a default Rules component for enabling it for orders.',
  `amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The amount of the base rate of the service.',
  `currency_code` varchar(32) NOT NULL COMMENT 'The currency code of the base rate of the service.',
  `data` longtext COMMENT 'A serialized array of additional data.',
  PRIMARY KEY (`name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores information about shipping services created...';

--
-- Table structure for table `commerce_line_item`
--

DROP TABLE IF EXISTS `commerce_line_item`;
CREATE TABLE `commerce_line_item` (
  `line_item_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a line item.',
  `order_id` int(11) NOT NULL DEFAULT '0' COMMENT 'The unique ID of the order the line item belongs to.',
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT 'The module defined type of this line item.',
  `line_item_label` varchar(255) NOT NULL COMMENT 'The merchant defined label for a line item.',
  `quantity` decimal(10,2) NOT NULL DEFAULT '0.00',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the line item was created.',
  `changed` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the line item was most recently saved.',
  `data` longblob COMMENT 'A serialized array of additional data.',
  PRIMARY KEY (`line_item_id`),
  KEY `order_id` (`order_id`),
  KEY `line_item_type` (`type`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='The base table for line items.';

--
-- Table structure for table `commerce_order`
--

DROP TABLE IF EXISTS `commerce_order`;
CREATE TABLE `commerce_order` (
  `order_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for an order.',
  `order_number` varchar(255) DEFAULT NULL COMMENT 'The order number displayed to the customer.',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The current commerce_order_revision.revision_id version identifier.',
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT 'The type of this order.',
  `uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that owns this order.',
  `mail` varchar(255) NOT NULL DEFAULT '' COMMENT 'The e-mail address associated with the order.',
  `status` varchar(255) NOT NULL COMMENT 'The status name of this order.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the order was created.',
  `changed` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the order was most recently saved.',
  `hostname` varchar(128) NOT NULL DEFAULT '' COMMENT 'The IP address that created this order.',
  `data` longblob COMMENT 'A serialized array of additional data.',
  PRIMARY KEY (`order_id`),
  UNIQUE KEY `order_number` (`order_number`),
  UNIQUE KEY `revision_id` (`revision_id`),
  KEY `uid` (`uid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='The base table for orders.';

--
-- Table structure for table `commerce_order_revision`
--

DROP TABLE IF EXISTS `commerce_order_revision`;
CREATE TABLE `commerce_order_revision` (
  `order_id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The commerce_order.order_id of the order this revision belongs to.',
  `order_number` varchar(255) DEFAULT NULL COMMENT 'The order number displayed to the customer for this revision.',
  `revision_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for this revision.',
  `revision_uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that owns the order at this revision.',
  `mail` varchar(255) NOT NULL COMMENT 'The e-mail address associated with the order at this revision.',
  `status` varchar(255) NOT NULL COMMENT 'The status name of this revision.',
  `log` longtext NOT NULL COMMENT 'The log entry explaining the changes in this version.',
  `revision_timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when this revision was created.',
  `revision_hostname` varchar(128) NOT NULL DEFAULT '' COMMENT 'The IP address that created this order.',
  `data` longblob COMMENT 'A serialized array of additional data.',
  PRIMARY KEY (`revision_id`),
  KEY `order_id` (`order_id`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Saves information about each saved revision of a commerce...';

--
-- Table structure for table `commerce_payment_transaction`
--

DROP TABLE IF EXISTS `commerce_payment_transaction`;
CREATE TABLE `commerce_payment_transaction` (
  `transaction_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a transaction.',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The current commerce_payment_transaction_revision.revision_id version identifier.',
  `uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that created this transaction.',
  `order_id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The commerce_order.order_id of the order this payment is for.',
  `payment_method` varchar(128) NOT NULL COMMENT 'The payment method method_id for this transaction.',
  `instance_id` varchar(255) NOT NULL COMMENT 'The payment method instance ID for this transaction.',
  `remote_id` varchar(255) NOT NULL COMMENT 'The remote identifier for this transaction.',
  `message` longtext NOT NULL COMMENT 'The human-readable message associated to this transaction.',
  `message_variables` longblob NOT NULL COMMENT 'The variables associated with the human-readable message.',
  `amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The amount of this transaction.',
  `currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `status` varchar(128) NOT NULL COMMENT 'The status of this transaction (pending, success, or failure).',
  `remote_status` varchar(128) NOT NULL COMMENT 'The status of the transaction at the payment provider.',
  `payload` longblob NOT NULL COMMENT 'The payment-gateway specific payload associated with this transaction.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when this transaction was created.',
  `changed` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when this transaction was last changed.',
  `data` longblob COMMENT 'A serialized array of additional data.',
  PRIMARY KEY (`transaction_id`),
  UNIQUE KEY `revision_id` (`revision_id`),
  KEY `payment_method` (`payment_method`),
  KEY `uid` (`uid`),
  KEY `order_id` (`order_id`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Transaction information for every attempted payment.';

--
-- Table structure for table `commerce_payment_transaction_revision`
--

DROP TABLE IF EXISTS `commerce_payment_transaction_revision`;
CREATE TABLE `commerce_payment_transaction_revision` (
  `transaction_id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The primary identifier for a transaction.',
  `revision_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The current commerce_payment_transaction_revision.revision_id version identifier.',
  `revision_uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that created this revision.',
  `remote_id` varchar(255) NOT NULL COMMENT 'The remote identifier for this transaction.',
  `message` longtext NOT NULL COMMENT 'The human-readable message associated to this transaction.',
  `message_variables` longblob NOT NULL COMMENT 'The variables associated with the human-readable message.',
  `amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The amount of this transaction.',
  `currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `status` varchar(128) NOT NULL COMMENT 'The status of this transaction (pending, success, or failure).',
  `remote_status` varchar(128) NOT NULL COMMENT 'The status of the transaction at the payment provider.',
  `log` longtext NOT NULL COMMENT 'The log entry explaining the changes in this version.',
  `revision_timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when this revision was created.',
  `data` longblob COMMENT 'A serialized array of additional data.',
  PRIMARY KEY (`revision_id`),
  KEY `transaction_id` (`transaction_id`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Saves information about each saved revision of a commerce...';

--
-- Table structure for table `commerce_product`
--

DROP TABLE IF EXISTS `commerce_product`;
CREATE TABLE `commerce_product` (
  `product_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a product, used internally only.',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The current commerce_product_revision.revision_id version identifier.',
  `sku` varchar(255) NOT NULL COMMENT 'The unique, human-readable identifier for a product.',
  `title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The title of this product, always treated as non-markup plain text.',
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT 'The commerce_product_type.type of this product.',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The languages.language of this product.',
  `uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that created this product.',
  `status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'Boolean indicating whether or not the product appears in lists and may be added to orders.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the product was created.',
  `changed` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the product was most recently saved.',
  `data` longblob COMMENT 'A serialized array of additional data.',
  PRIMARY KEY (`product_id`),
  UNIQUE KEY `sku` (`sku`),
  UNIQUE KEY `revision_id` (`revision_id`),
  KEY `product_type` (`type`),
  KEY `uid` (`uid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='The base table for products.';

--
-- Table structure for table `commerce_product_revision`
--

DROP TABLE IF EXISTS `commerce_product_revision`;
CREATE TABLE `commerce_product_revision` (
  `product_id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The commerce_product.product_id of the product this revision belongs to.',
  `revision_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for this revision.',
  `sku` varchar(255) NOT NULL COMMENT 'The unique, human-readable identifier of a product for this revision.',
  `title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The title of this product for this revision',
  `revision_uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that owns the product at this revision.',
  `status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'The status of this revision.',
  `log` longtext NOT NULL COMMENT 'The log entry explaining the changes in this version.',
  `revision_timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when this revision was created.',
  `data` longblob COMMENT 'A serialized array of additional data for this revision.',
  PRIMARY KEY (`revision_id`),
  KEY `product_id` (`product_id`),
  KEY `revision_uid` (`revision_uid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Saves information about each saved revision of a commerce...';

--
-- Table structure for table `commerce_product_type`
--

DROP TABLE IF EXISTS `commerce_product_type`;
CREATE TABLE `commerce_product_type` (
  `type` varchar(32) NOT NULL DEFAULT '' COMMENT 'The machine-readable name of this type.',
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'The human-readable name of this type.',
  `description` mediumtext NOT NULL COMMENT 'A brief description of this type.',
  `help` mediumtext NOT NULL COMMENT 'Help information shown to the user when creating a commerce_product of this type.',
  `revision` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'Determine whether to create a new revision when a product of this type is updated.',
  PRIMARY KEY (`type`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores information about commerce_product types created...';

--
-- Table structure for table `commerce_tax_rate`
--

DROP TABLE IF EXISTS `commerce_tax_rate`;
CREATE TABLE `commerce_tax_rate` (
  `name` varchar(64) NOT NULL DEFAULT '' COMMENT 'The machine-name of this rate.',
  `title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The administrative title of this rate.',
  `display_title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The front end display title of this rate.',
  `description` mediumtext NOT NULL COMMENT 'A brief description of this rate.',
  `rate` varchar(64) NOT NULL DEFAULT '0' COMMENT 'The percentage used to calculate this tax expressed as a decimal.',
  `type` varchar(64) NOT NULL DEFAULT '' COMMENT 'The machine-name of the rate’s commerce_tax_type.',
  `default_rules_component` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether or not this rate should have a default Rules component for applying it to products.',
  `module` varchar(255) NOT NULL DEFAULT '' COMMENT 'The name of the module that defines this tax type.',
  PRIMARY KEY (`name`),
  KEY `type` (`type`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores information about tax rates created via Tax UI.';

--
-- Table structure for table `commerce_tax_type`
--

DROP TABLE IF EXISTS `commerce_tax_type`;
CREATE TABLE `commerce_tax_type` (
  `name` varchar(64) NOT NULL DEFAULT '' COMMENT 'The machine-name of this type.',
  `title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The administrative title of this type.',
  `display_title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The front end display title of this type.',
  `description` mediumtext NOT NULL COMMENT 'A brief description of this type.',
  `display_inclusive` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether or not taxes of this type display inclusively in product prices.',
  `round_mode` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Integer indicating what type of rounding (if any) should be done for taxes of this type.',
  `module` varchar(255) NOT NULL DEFAULT '' COMMENT 'The name of the module that defines this tax type.',
  PRIMARY KEY (`name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores information about tax types created via Tax UI.';

--
-- Table structure for table `contact`
--

DROP TABLE IF EXISTS `contact`;
CREATE TABLE `contact` (
  `cid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique category ID.',
  `category` varchar(255) NOT NULL DEFAULT '' COMMENT 'Category name.',
  `recipients` longtext NOT NULL COMMENT 'Comma-separated list of recipient e-mail addresses.',
  `reply` longtext NOT NULL COMMENT 'Text of the auto-reply message.',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'The category’s weight.',
  `selected` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Flag to indicate whether or not category is selected by default. (1 = Yes, 0 = No)',
  PRIMARY KEY (`cid`),
  UNIQUE KEY `category` (`category`),
  KEY `list` (`weight`,`category`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Contact form category settings.';

--
-- Table structure for table `ctools_css_cache`
--

DROP TABLE IF EXISTS `ctools_css_cache`;
CREATE TABLE `ctools_css_cache` (
  `cid` varchar(128) NOT NULL COMMENT 'The CSS ID this cache object belongs to.',
  `filename` varchar(255) DEFAULT NULL COMMENT 'The filename this CSS is stored in.',
  `css` longtext COMMENT 'CSS being stored.',
  `filter` tinyint(4) DEFAULT NULL COMMENT 'Whether or not this CSS needs to be filtered.',
  PRIMARY KEY (`cid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='A special cache used to store CSS that must be non-volatile.';

--
-- Table structure for table `ctools_object_cache`
--

DROP TABLE IF EXISTS `ctools_object_cache`;
CREATE TABLE `ctools_object_cache` (
  `sid` varchar(64) NOT NULL COMMENT 'The session ID this cache object belongs to.',
  `name` varchar(128) NOT NULL COMMENT 'The name of the object this cache is attached to.',
  `obj` varchar(128) NOT NULL COMMENT 'The type of the object this cache is attached to; this essentially represents the owner so that several sub-systems can use this cache.',
  `updated` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The time this cache was created or updated.',
  `data` longblob COMMENT 'Serialized data being stored.',
  PRIMARY KEY (`sid`,`obj`,`name`),
  KEY `updated` (`updated`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='A special cache used to store objects that are being...';

--
-- Table structure for table `current_search`
--

DROP TABLE IF EXISTS `current_search`;
CREATE TABLE `current_search` (
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'The machine readable name of the configuration.',
  `label` varchar(255) NOT NULL DEFAULT '' COMMENT 'The human readable name of the configuration.',
  `settings` text COMMENT 'Serialized storage of general settings.',
  PRIMARY KEY (`name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Current search block configurations.';

--
-- Table structure for table `date_format_locale`
--

DROP TABLE IF EXISTS `date_format_locale`;
CREATE TABLE `date_format_locale` (
  `format` varchar(100) NOT NULL COMMENT 'The date format string.',
  `type` varchar(64) NOT NULL COMMENT 'The date format type, e.g. medium.',
  `language` varchar(12) NOT NULL COMMENT 'A languages.language for this format to be used with.',
  PRIMARY KEY (`type`,`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores configured date formats for each locale.';

--
-- Table structure for table `date_format_type`
--

DROP TABLE IF EXISTS `date_format_type`;
CREATE TABLE `date_format_type` (
  `type` varchar(64) NOT NULL COMMENT 'The date format type, e.g. medium.',
  `title` varchar(255) NOT NULL COMMENT 'The human readable name of the format type.',
  `locked` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Whether or not this is a system provided format.',
  PRIMARY KEY (`type`),
  KEY `title` (`title`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores configured date format types.';

--
-- Table structure for table `date_formats`
--

DROP TABLE IF EXISTS `date_formats`;
CREATE TABLE `date_formats` (
  `dfid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The date format identifier.',
  `format` varchar(100) CHARACTER SET utf8 COLLATE utf8_bin NOT NULL COMMENT 'The date format string.',
  `type` varchar(64) NOT NULL COMMENT 'The date format type, e.g. medium.',
  `locked` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Whether or not this format can be modified.',
  PRIMARY KEY (`dfid`),
  UNIQUE KEY `formats` (`format`,`type`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores configured date formats.';

--
-- Table structure for table `facetapi`
--

DROP TABLE IF EXISTS `facetapi`;
CREATE TABLE `facetapi` (
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'The machine readable name of the configuration.',
  `searcher` varchar(64) NOT NULL DEFAULT '' COMMENT 'The machine readable name of the searcher.',
  `realm` varchar(64) NOT NULL DEFAULT '' COMMENT 'The machine readable name of the realm.',
  `facet` varchar(255) NOT NULL DEFAULT '' COMMENT 'The machine readable name of the facet.',
  `enabled` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Whether the facet is enabled.',
  `settings` blob COMMENT 'Serialized storage of general settings.',
  PRIMARY KEY (`name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Facet configurations.';

--
-- Table structure for table `feeds_importer`
--

DROP TABLE IF EXISTS `feeds_importer`;
CREATE TABLE `feeds_importer` (
  `id` varchar(128) NOT NULL DEFAULT '' COMMENT 'Id of the fields object.',
  `config` longblob COMMENT 'Configuration of the feeds object.',
  PRIMARY KEY (`id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Configuration of feeds objects.';

--
-- Table structure for table `feeds_item`
--

DROP TABLE IF EXISTS `feeds_item`;
CREATE TABLE `feeds_item` (
  `entity_type` varchar(32) NOT NULL DEFAULT '' COMMENT 'The entity type.',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The imported entity’s serial id.',
  `id` varchar(128) NOT NULL DEFAULT '' COMMENT 'The id of the importer that created this item.',
  `feed_nid` int(10) unsigned NOT NULL COMMENT 'Node id of the source, if available.',
  `imported` int(11) NOT NULL DEFAULT '0' COMMENT 'Import date of the feed item, as a Unix timestamp.',
  `url` text NOT NULL COMMENT 'Link to the feed item.',
  `guid` text NOT NULL COMMENT 'Unique identifier for the feed item.',
  `hash` varchar(32) NOT NULL DEFAULT '' COMMENT 'The hash of the source item.',
  PRIMARY KEY (`entity_type`,`entity_id`),
  KEY `id` (`id`),
  KEY `feed_nid` (`feed_nid`),
  KEY `lookup_url` (`entity_type`,`id`,`feed_nid`,`url`(128)),
  KEY `lookup_guid` (`entity_type`,`id`,`feed_nid`,`guid`(128)),
  KEY `global_lookup_url` (`entity_type`,`url`(128)),
  KEY `global_lookup_guid` (`entity_type`,`guid`(128)),
  KEY `imported` (`imported`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Tracks items such as nodes, terms, users.';

--
-- Table structure for table `feeds_log`
--

DROP TABLE IF EXISTS `feeds_log`;
CREATE TABLE `feeds_log` (
  `flid` int(11) NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique feeds event ID.',
  `id` varchar(128) NOT NULL DEFAULT '' COMMENT 'The id of the importer that logged the event.',
  `feed_nid` int(10) unsigned NOT NULL COMMENT 'Node id of the source, if available.',
  `log_time` int(11) NOT NULL DEFAULT '0' COMMENT 'Unix timestamp of when event occurred.',
  `request_time` int(11) NOT NULL DEFAULT '0' COMMENT 'Unix timestamp of the request when the event occurred.',
  `type` varchar(64) NOT NULL DEFAULT '' COMMENT 'Type of log message, for example "feeds_import"."',
  `message` longtext NOT NULL COMMENT 'Text of log message to be passed into the t() function.',
  `variables` longblob NOT NULL COMMENT 'Serialized array of variables that match the message string and that is passed into the t() function.',
  `severity` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'The severity level of the event; ranges from 0 (Emergency) to 7 (Debug)',
  PRIMARY KEY (`flid`),
  KEY `id` (`id`),
  KEY `id_feed_nid` (`id`,`feed_nid`),
  KEY `request_time` (`request_time`),
  KEY `log_time` (`log_time`),
  KEY `type` (`type`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Table that contains logs of feeds events.';

--
-- Table structure for table `feeds_push_subscriptions`
--

DROP TABLE IF EXISTS `feeds_push_subscriptions`;
CREATE TABLE `feeds_push_subscriptions` (
  `domain` varchar(128) NOT NULL DEFAULT '' COMMENT 'Domain of the subscriber. Corresponds to an importer id.',
  `subscriber_id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'ID of the subscriber. Corresponds to a feed nid.',
  `timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'Created timestamp.',
  `hub` text NOT NULL COMMENT 'The URL of the hub endpoint of this subscription.',
  `topic` text NOT NULL COMMENT 'The topic URL (feed URL) of this subscription.',
  `secret` varchar(128) NOT NULL DEFAULT '' COMMENT 'Shared secret for message authentication.',
  `status` varchar(64) NOT NULL DEFAULT '' COMMENT 'Status of subscription.',
  `post_fields` text COMMENT 'Fields posted.',
  PRIMARY KEY (`domain`,`subscriber_id`),
  KEY `timestamp` (`timestamp`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='PubSubHubbub subscriptions.';

--
-- Table structure for table `feeds_selfnode_processor_item`
--

DROP TABLE IF EXISTS `feeds_selfnode_processor_item`;
CREATE TABLE `feeds_selfnode_processor_item` (
  `id` varchar(128) NOT NULL DEFAULT '' COMMENT 'The id of the importer that created this item.',
  `feed_nid` int(10) unsigned NOT NULL COMMENT 'Node id of the source.',
  `imported` int(11) NOT NULL DEFAULT '0' COMMENT 'Import date of the feed item, as a Unix timestamp.',
  `hash` varchar(32) NOT NULL DEFAULT '' COMMENT 'The hash of the source item.',
  PRIMARY KEY (`feed_nid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Tracks selfnode_processor nodes.';

--
-- Table structure for table `feeds_source`
--

DROP TABLE IF EXISTS `feeds_source`;
CREATE TABLE `feeds_source` (
  `id` varchar(128) NOT NULL DEFAULT '' COMMENT 'Id of the feed configuration.',
  `feed_nid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Node nid if this particular source is attached to a feed node.',
  `config` longblob COMMENT 'Configuration of the feeds object.',
  `source` text NOT NULL COMMENT 'Main source resource identifier. E. g. a path or a URL.',
  `state` longblob COMMENT 'State of import or clearing batches.',
  `fetcher_result` longblob COMMENT 'Cache for fetcher result.',
  `imported` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Timestamp when this source was imported last.',
  PRIMARY KEY (`id`,`feed_nid`),
  KEY `id` (`id`),
  KEY `feed_nid` (`feed_nid`),
  KEY `id_source` (`id`,`source`(128))
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Source definitions for feeds.';

--
-- Table structure for table `feeds_tamper`
--

DROP TABLE IF EXISTS `feeds_tamper`;
CREATE TABLE `feeds_tamper` (
  `id` varchar(128) NOT NULL DEFAULT '' COMMENT 'Id of the feeds tamper instance.',
  `importer` varchar(128) NOT NULL DEFAULT '' COMMENT 'Id of the feeds importer.',
  `source` varchar(128) NOT NULL DEFAULT '' COMMENT 'The source field of the importer.',
  `plugin_id` varchar(128) NOT NULL DEFAULT '' COMMENT 'Id of the tamper plugin.',
  `settings` longtext COMMENT 'A serialized array of options for a Feeds Tamper plugin.',
  `weight` int(10) unsigned NOT NULL COMMENT 'The weight of a plugin instance. Plugins are executed in order.',
  `description` varchar(255) NOT NULL DEFAULT '' COMMENT 'Description of this plugin.',
  PRIMARY KEY (`id`),
  KEY `importer` (`importer`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Table storing tamper instances.';

--
-- Table structure for table `field_config`
--

DROP TABLE IF EXISTS `field_config`;
CREATE TABLE `field_config` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a field',
  `field_name` varchar(32) NOT NULL COMMENT 'The name of this field. Non-deleted field names are unique, but multiple deleted fields can have the same name.',
  `type` varchar(128) NOT NULL COMMENT 'The type of this field.',
  `module` varchar(128) NOT NULL DEFAULT '' COMMENT 'The module that implements the field type.',
  `active` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether the module that implements the field type is enabled.',
  `storage_type` varchar(128) NOT NULL COMMENT 'The storage backend for the field.',
  `storage_module` varchar(128) NOT NULL DEFAULT '' COMMENT 'The module that implements the storage backend.',
  `storage_active` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether the module that implements the storage backend is enabled.',
  `locked` tinyint(4) NOT NULL DEFAULT '0' COMMENT '@TODO',
  `data` longblob NOT NULL COMMENT 'Serialized data containing the field properties that do not warrant a dedicated column.',
  `cardinality` tinyint(4) NOT NULL DEFAULT '0',
  `translatable` tinyint(4) NOT NULL DEFAULT '0',
  `deleted` tinyint(4) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `field_name` (`field_name`),
  KEY `active` (`active`),
  KEY `storage_active` (`storage_active`),
  KEY `deleted` (`deleted`),
  KEY `module` (`module`),
  KEY `storage_module` (`storage_module`),
  KEY `type` (`type`),
  KEY `storage_type` (`storage_type`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8;

--
-- Table structure for table `field_config_instance`
--

DROP TABLE IF EXISTS `field_config_instance`;
CREATE TABLE `field_config_instance` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a field instance',
  `field_id` int(11) NOT NULL COMMENT 'The identifier of the field attached by this instance',
  `field_name` varchar(32) NOT NULL DEFAULT '',
  `entity_type` varchar(32) NOT NULL DEFAULT '',
  `bundle` varchar(128) NOT NULL DEFAULT '',
  `data` longblob NOT NULL,
  `deleted` tinyint(4) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `field_name_bundle` (`field_name`,`entity_type`,`bundle`),
  KEY `deleted` (`deleted`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8;

--
-- Table structure for table `field_data_body`
--

DROP TABLE IF EXISTS `field_data_body`;
CREATE TABLE `field_data_body` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `body_value` longtext,
  `body_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `body_format` (`body_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 20 (body)';

--
-- Table structure for table `field_data_comment_body`
--

DROP TABLE IF EXISTS `field_data_comment_body`;
CREATE TABLE `field_data_comment_body` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `comment_body_value` longtext,
  `comment_body_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `comment_body_format` (`comment_body_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 21 (comment_body)';

--
-- Table structure for table `field_data_commerce_customer_address`
--

DROP TABLE IF EXISTS `field_data_commerce_customer_address`;
CREATE TABLE `field_data_commerce_customer_address` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_customer_address_country` varchar(2) DEFAULT '' COMMENT 'Two letter ISO country code of this address.',
  `commerce_customer_address_administrative_area` varchar(255) DEFAULT '' COMMENT 'The administrative area of this address. (i.e. State/Province)',
  `commerce_customer_address_sub_administrative_area` varchar(255) DEFAULT '' COMMENT 'The sub administrative area of this address.',
  `commerce_customer_address_locality` varchar(255) DEFAULT '' COMMENT 'The locality of this address. (i.e. City)',
  `commerce_customer_address_dependent_locality` varchar(255) DEFAULT '' COMMENT 'The dependent locality of this address.',
  `commerce_customer_address_postal_code` varchar(255) DEFAULT '' COMMENT 'The postal code of this address.',
  `commerce_customer_address_thoroughfare` varchar(255) DEFAULT '' COMMENT 'The thoroughfare of this address. (i.e. Street address)',
  `commerce_customer_address_premise` varchar(255) DEFAULT '' COMMENT 'The premise of this address. (i.e. Apartment / Suite number)',
  `commerce_customer_address_sub_premise` varchar(255) DEFAULT '' COMMENT 'The sub_premise of this address.',
  `commerce_customer_address_organisation_name` varchar(255) DEFAULT '' COMMENT 'Contents of a primary OrganisationName element in the xNL XML.',
  `commerce_customer_address_name_line` varchar(255) DEFAULT '' COMMENT 'Contents of a primary NameLine element in the xNL XML.',
  `commerce_customer_address_first_name` varchar(255) DEFAULT '' COMMENT 'Contents of the FirstName element of a primary PersonName element in the xNL XML.',
  `commerce_customer_address_last_name` varchar(255) DEFAULT '' COMMENT 'Contents of the LastName element of a primary PersonName element in the xNL XML.',
  `commerce_customer_address_data` longtext COMMENT 'Additional data for this address.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 1 (commerce_customer_address)';

--
-- Table structure for table `field_data_commerce_customer_billing`
--

DROP TABLE IF EXISTS `field_data_commerce_customer_billing`;
CREATE TABLE `field_data_commerce_customer_billing` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_customer_billing_profile_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_customer_billing_profile_id` (`commerce_customer_billing_profile_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 4 (commerce_customer_billing)';

--
-- Table structure for table `field_data_commerce_customer_shipping`
--

DROP TABLE IF EXISTS `field_data_commerce_customer_shipping`;
CREATE TABLE `field_data_commerce_customer_shipping` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_customer_shipping_profile_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_customer_shipping_profile_id` (`commerce_customer_shipping_profile_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 19 (commerce_customer_shipping)';

--
-- Table structure for table `field_data_commerce_discount_date`
--

DROP TABLE IF EXISTS `field_data_commerce_discount_date`;
CREATE TABLE `field_data_commerce_discount_date` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_discount_date_value` int(11) DEFAULT NULL,
  `commerce_discount_date_value2` int(11) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 17 (commerce_discount_date)';

--
-- Table structure for table `field_data_commerce_discount_offer`
--

DROP TABLE IF EXISTS `field_data_commerce_discount_offer`;
CREATE TABLE `field_data_commerce_discount_offer` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_discount_offer_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_discount_offer_target_id` (`commerce_discount_offer_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 13 (commerce_discount_offer)';

--
-- Table structure for table `field_data_commerce_discounts`
--

DROP TABLE IF EXISTS `field_data_commerce_discounts`;
CREATE TABLE `field_data_commerce_discounts` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_discounts_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_discounts_target_id` (`commerce_discounts_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 16 (commerce_discounts)';

--
-- Table structure for table `field_data_commerce_display_path`
--

DROP TABLE IF EXISTS `field_data_commerce_display_path`;
CREATE TABLE `field_data_commerce_display_path` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_display_path_value` varchar(255) DEFAULT NULL,
  `commerce_display_path_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_display_path_format` (`commerce_display_path_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 8 (commerce_display_path)';

--
-- Table structure for table `field_data_commerce_fixed_amount`
--

DROP TABLE IF EXISTS `field_data_commerce_fixed_amount`;
CREATE TABLE `field_data_commerce_fixed_amount` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_fixed_amount_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `commerce_fixed_amount_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `commerce_fixed_amount_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_fixed_amount_currency_price` (`commerce_fixed_amount_amount`,`commerce_fixed_amount_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 14 (commerce_fixed_amount)';

--
-- Table structure for table `field_data_commerce_free_products`
--

DROP TABLE IF EXISTS `field_data_commerce_free_products`;
CREATE TABLE `field_data_commerce_free_products` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_free_products_product_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_free_products_product_id` (`commerce_free_products_product_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 66 (commerce_free_products)';

--
-- Table structure for table `field_data_commerce_free_shipping`
--

DROP TABLE IF EXISTS `field_data_commerce_free_shipping`;
CREATE TABLE `field_data_commerce_free_shipping` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_free_shipping_value` varchar(255) DEFAULT NULL,
  `commerce_free_shipping_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_free_shipping_format` (`commerce_free_shipping_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 65 (commerce_free_shipping)';

--
-- Table structure for table `field_data_commerce_line_items`
--

DROP TABLE IF EXISTS `field_data_commerce_line_items`;
CREATE TABLE `field_data_commerce_line_items` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_line_items_line_item_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_line_items_line_item_id` (`commerce_line_items_line_item_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 2 (commerce_line_items)';

--
-- Table structure for table `field_data_commerce_order_total`
--

DROP TABLE IF EXISTS `field_data_commerce_order_total`;
CREATE TABLE `field_data_commerce_order_total` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_order_total_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `commerce_order_total_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `commerce_order_total_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_order_total_currency_price` (`commerce_order_total_amount`,`commerce_order_total_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 3 (commerce_order_total)';

--
-- Table structure for table `field_data_commerce_percentage`
--

DROP TABLE IF EXISTS `field_data_commerce_percentage`;
CREATE TABLE `field_data_commerce_percentage` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_percentage_value` decimal(10,2) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 15 (commerce_percentage)';

--
-- Table structure for table `field_data_commerce_price`
--

DROP TABLE IF EXISTS `field_data_commerce_price`;
CREATE TABLE `field_data_commerce_price` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_price_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `commerce_price_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `commerce_price_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_price_currency_price` (`commerce_price_amount`,`commerce_price_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 12 (commerce_price)';

--
-- Table structure for table `field_data_commerce_product`
--

DROP TABLE IF EXISTS `field_data_commerce_product`;
CREATE TABLE `field_data_commerce_product` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_product_product_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_product_product_id` (`commerce_product_product_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 7 (commerce_product)';

--
-- Table structure for table `field_data_commerce_shipping_service`
--

DROP TABLE IF EXISTS `field_data_commerce_shipping_service`;
CREATE TABLE `field_data_commerce_shipping_service` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_shipping_service_value` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_shipping_service_value` (`commerce_shipping_service_value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 18 (commerce_shipping_service)';

--
-- Table structure for table `field_data_commerce_total`
--

DROP TABLE IF EXISTS `field_data_commerce_total`;
CREATE TABLE `field_data_commerce_total` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_total_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `commerce_total_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `commerce_total_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_total_currency_price` (`commerce_total_amount`,`commerce_total_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 6 (commerce_total)';

--
-- Table structure for table `field_data_commerce_unit_price`
--

DROP TABLE IF EXISTS `field_data_commerce_unit_price`;
CREATE TABLE `field_data_commerce_unit_price` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_unit_price_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `commerce_unit_price_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `commerce_unit_price_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_unit_price_currency_price` (`commerce_unit_price_amount`,`commerce_unit_price_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 5 (commerce_unit_price)';

--
-- Table structure for table `field_data_field_availability_date`
--

DROP TABLE IF EXISTS `field_data_field_availability_date`;
CREATE TABLE `field_data_field_availability_date` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_availability_date_value` datetime DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 76 (field_availability_date)';

--
-- Table structure for table `field_data_field_availability_period`
--

DROP TABLE IF EXISTS `field_data_field_availability_period`;
CREATE TABLE `field_data_field_availability_period` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_availability_period_value` varchar(11) DEFAULT NULL,
  `field_availability_period_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_availability_period_format` (`field_availability_period_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 77 (field_availability_period)';

--
-- Table structure for table `field_data_field_brand`
--

DROP TABLE IF EXISTS `field_data_field_brand`;
CREATE TABLE `field_data_field_brand` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_brand_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_brand_tid` (`field_brand_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 37 (field_brand)';

--
-- Table structure for table `field_data_field_collection`
--

DROP TABLE IF EXISTS `field_data_field_collection`;
CREATE TABLE `field_data_field_collection` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_collection_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_collection_tid` (`field_collection_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 38 (field_collection)';

--
-- Table structure for table `field_data_field_colour`
--

DROP TABLE IF EXISTS `field_data_field_colour`;
CREATE TABLE `field_data_field_colour` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_colour_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_colour_tid` (`field_colour_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 36 (field_colour)';

--
-- Table structure for table `field_data_field_colour_code`
--

DROP TABLE IF EXISTS `field_data_field_colour_code`;
CREATE TABLE `field_data_field_colour_code` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_colour_code_value` varchar(12) DEFAULT NULL,
  `field_colour_code_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_colour_code_format` (`field_colour_code_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 34 (field_colour_code)';

--
-- Table structure for table `field_data_field_headline`
--

DROP TABLE IF EXISTS `field_data_field_headline`;
CREATE TABLE `field_data_field_headline` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_headline_value` varchar(20) DEFAULT NULL,
  `field_headline_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_headline_format` (`field_headline_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 33 (field_headline)';

--
-- Table structure for table `field_data_field_image`
--

DROP TABLE IF EXISTS `field_data_field_image`;
CREATE TABLE `field_data_field_image` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_image_fid` int(10) unsigned DEFAULT NULL COMMENT 'The file_managed.fid being referenced in this field.',
  `field_image_alt` varchar(512) DEFAULT NULL COMMENT 'Alternative image text, for the image’s ’alt’ attribute.',
  `field_image_title` varchar(1024) DEFAULT NULL COMMENT 'Image title text, for the image’s ’title’ attribute.',
  `field_image_width` int(10) unsigned DEFAULT NULL COMMENT 'The width of the image in pixels.',
  `field_image_height` int(10) unsigned DEFAULT NULL COMMENT 'The height of the image in pixels.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_image_fid` (`field_image_fid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 29 (field_image)';

--
-- Table structure for table `field_data_field_images`
--

DROP TABLE IF EXISTS `field_data_field_images`;
CREATE TABLE `field_data_field_images` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_images_fid` int(10) unsigned DEFAULT NULL COMMENT 'The file_managed.fid being referenced in this field.',
  `field_images_alt` varchar(512) DEFAULT NULL COMMENT 'Alternative image text, for the image’s ’alt’ attribute.',
  `field_images_title` varchar(1024) DEFAULT NULL COMMENT 'Image title text, for the image’s ’title’ attribute.',
  `field_images_width` int(10) unsigned DEFAULT NULL COMMENT 'The width of the image in pixels.',
  `field_images_height` int(10) unsigned DEFAULT NULL COMMENT 'The height of the image in pixels.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_images_fid` (`field_images_fid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 26 (field_images)';

--
-- Table structure for table `field_data_field_link`
--

DROP TABLE IF EXISTS `field_data_field_link`;
CREATE TABLE `field_data_field_link` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_link_url` varchar(2048) DEFAULT NULL,
  `field_link_title` varchar(255) DEFAULT NULL,
  `field_link_attributes` mediumtext,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 30 (field_link)';

--
-- Table structure for table `field_data_field_price_pl_01`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_01`;
CREATE TABLE `field_data_field_price_pl_01` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_01_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_01_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_01_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_01_currency_price` (`field_price_pl_01_amount`,`field_price_pl_01_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 48 (field_price_pl_01)';

--
-- Table structure for table `field_data_field_price_pl_02`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_02`;
CREATE TABLE `field_data_field_price_pl_02` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_02_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_02_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_02_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_02_currency_price` (`field_price_pl_02_amount`,`field_price_pl_02_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 49 (field_price_pl_02)';

--
-- Table structure for table `field_data_field_price_pl_03`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_03`;
CREATE TABLE `field_data_field_price_pl_03` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_03_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_03_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_03_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_03_currency_price` (`field_price_pl_03_amount`,`field_price_pl_03_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 50 (field_price_pl_03)';

--
-- Table structure for table `field_data_field_price_pl_04`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_04`;
CREATE TABLE `field_data_field_price_pl_04` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_04_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_04_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_04_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_04_currency_price` (`field_price_pl_04_amount`,`field_price_pl_04_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 51 (field_price_pl_04)';

--
-- Table structure for table `field_data_field_price_pl_05`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_05`;
CREATE TABLE `field_data_field_price_pl_05` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_05_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_05_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_05_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_05_currency_price` (`field_price_pl_05_amount`,`field_price_pl_05_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 52 (field_price_pl_05)';

--
-- Table structure for table `field_data_field_price_pl_06`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_06`;
CREATE TABLE `field_data_field_price_pl_06` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_06_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_06_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_06_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_06_currency_price` (`field_price_pl_06_amount`,`field_price_pl_06_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 53 (field_price_pl_06)';

--
-- Table structure for table `field_data_field_price_pl_07`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_07`;
CREATE TABLE `field_data_field_price_pl_07` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_07_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_07_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_07_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_07_currency_price` (`field_price_pl_07_amount`,`field_price_pl_07_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 54 (field_price_pl_07)';

--
-- Table structure for table `field_data_field_price_pl_08`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_08`;
CREATE TABLE `field_data_field_price_pl_08` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_08_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_08_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_08_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_08_currency_price` (`field_price_pl_08_amount`,`field_price_pl_08_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 55 (field_price_pl_08)';

--
-- Table structure for table `field_data_field_price_pl_09`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_09`;
CREATE TABLE `field_data_field_price_pl_09` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_09_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_09_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_09_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_09_currency_price` (`field_price_pl_09_amount`,`field_price_pl_09_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 56 (field_price_pl_09)';

--
-- Table structure for table `field_data_field_price_pl_10`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_10`;
CREATE TABLE `field_data_field_price_pl_10` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_10_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_10_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_10_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_10_currency_price` (`field_price_pl_10_amount`,`field_price_pl_10_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 57 (field_price_pl_10)';

--
-- Table structure for table `field_data_field_price_pl_11`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_11`;
CREATE TABLE `field_data_field_price_pl_11` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_11_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_11_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_11_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_11_currency_price` (`field_price_pl_11_amount`,`field_price_pl_11_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 58 (field_price_pl_11)';

--
-- Table structure for table `field_data_field_price_pl_12`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_12`;
CREATE TABLE `field_data_field_price_pl_12` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_12_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_12_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_12_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_12_currency_price` (`field_price_pl_12_amount`,`field_price_pl_12_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 60 (field_price_pl_12)';

--
-- Table structure for table `field_data_field_price_pl_13`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_13`;
CREATE TABLE `field_data_field_price_pl_13` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_13_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_13_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_13_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_13_currency_price` (`field_price_pl_13_amount`,`field_price_pl_13_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 64 (field_price_pl_13)';

--
-- Table structure for table `field_data_field_price_pl_14`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_14`;
CREATE TABLE `field_data_field_price_pl_14` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_14_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_14_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_14_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_14_currency_price` (`field_price_pl_14_amount`,`field_price_pl_14_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 68 (field_price_pl_14)';

--
-- Table structure for table `field_data_field_price_pl_15`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_15`;
CREATE TABLE `field_data_field_price_pl_15` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_15_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_15_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_15_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_15_currency_price` (`field_price_pl_15_amount`,`field_price_pl_15_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 69 (field_price_pl_15)';

--
-- Table structure for table `field_data_field_price_pl_16`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_16`;
CREATE TABLE `field_data_field_price_pl_16` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_16_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_16_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_16_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_16_currency_price` (`field_price_pl_16_amount`,`field_price_pl_16_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 70 (field_price_pl_16)';

--
-- Table structure for table `field_data_field_price_pl_17`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_17`;
CREATE TABLE `field_data_field_price_pl_17` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_17_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_17_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_17_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_17_currency_price` (`field_price_pl_17_amount`,`field_price_pl_17_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 71 (field_price_pl_17)';

--
-- Table structure for table `field_data_field_price_pl_18`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_18`;
CREATE TABLE `field_data_field_price_pl_18` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_18_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_18_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_18_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_18_currency_price` (`field_price_pl_18_amount`,`field_price_pl_18_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 72 (field_price_pl_18)';

--
-- Table structure for table `field_data_field_price_pl_19`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_19`;
CREATE TABLE `field_data_field_price_pl_19` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_19_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_19_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_19_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_19_currency_price` (`field_price_pl_19_amount`,`field_price_pl_19_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 73 (field_price_pl_19)';

--
-- Table structure for table `field_data_field_price_pl_20`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_20`;
CREATE TABLE `field_data_field_price_pl_20` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_20_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_20_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_20_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_20_currency_price` (`field_price_pl_20_amount`,`field_price_pl_20_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 74 (field_price_pl_20)';

--
-- Table structure for table `field_data_field_price_pl_21`
--

DROP TABLE IF EXISTS `field_data_field_price_pl_21`;
CREATE TABLE `field_data_field_price_pl_21` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_21_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_21_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_21_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_21_currency_price` (`field_price_pl_21_amount`,`field_price_pl_21_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 75 (field_price_pl_21)';

--
-- Table structure for table `field_data_field_product`
--

DROP TABLE IF EXISTS `field_data_field_product`;
CREATE TABLE `field_data_field_product` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_product_product_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_product_product_id` (`field_product_product_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 27 (field_product)';

--
-- Table structure for table `field_data_field_product_category`
--

DROP TABLE IF EXISTS `field_data_field_product_category`;
CREATE TABLE `field_data_field_product_category` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_product_category_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_product_category_tid` (`field_product_category_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 28 (field_product_category)';

--
-- Table structure for table `field_data_field_required_delivery_date`
--

DROP TABLE IF EXISTS `field_data_field_required_delivery_date`;
CREATE TABLE `field_data_field_required_delivery_date` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_required_delivery_date_value` datetime DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 62 (field_required_delivery_date)';

--
-- Table structure for table `field_data_field_rgb`
--

DROP TABLE IF EXISTS `field_data_field_rgb`;
CREATE TABLE `field_data_field_rgb` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_rgb_value` varchar(7) DEFAULT NULL,
  `field_rgb_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_rgb_format` (`field_rgb_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 35 (field_rgb)';

--
-- Table structure for table `field_data_field_special_request`
--

DROP TABLE IF EXISTS `field_data_field_special_request`;
CREATE TABLE `field_data_field_special_request` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_special_request_value` longtext,
  `field_special_request_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_special_request_format` (`field_special_request_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 63 (field_special_request)';

--
-- Table structure for table `field_data_field_stock_status`
--

DROP TABLE IF EXISTS `field_data_field_stock_status`;
CREATE TABLE `field_data_field_stock_status` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_stock_status_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_stock_status_tid` (`field_stock_status_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 59 (field_stock_status)';

--
-- Table structure for table `field_data_field_table`
--

DROP TABLE IF EXISTS `field_data_field_table`;
CREATE TABLE `field_data_field_table` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_table_value` longtext,
  `field_table_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_table_format` (`field_table_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 78 (field_table)';

--
-- Table structure for table `field_data_field_table_`
--

DROP TABLE IF EXISTS `field_data_field_table_`;
CREATE TABLE `field_data_field_table_` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_table__value` longtext,
  `field_table__format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_table__format` (`field_table__format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 78 (field_table_)';

--
-- Table structure for table `field_data_field_tagline`
--

DROP TABLE IF EXISTS `field_data_field_tagline`;
CREATE TABLE `field_data_field_tagline` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_tagline_value` varchar(80) DEFAULT NULL,
  `field_tagline_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_tagline_format` (`field_tagline_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 31 (field_tagline)';

--
-- Table structure for table `field_data_field_trim_colour`
--

DROP TABLE IF EXISTS `field_data_field_trim_colour`;
CREATE TABLE `field_data_field_trim_colour` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_trim_colour_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_trim_colour_tid` (`field_trim_colour_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 39 (field_trim_colour)';

--
-- Table structure for table `field_data_field_wholesale_price`
--

DROP TABLE IF EXISTS `field_data_field_wholesale_price`;
CREATE TABLE `field_data_field_wholesale_price` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_wholesale_price_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_wholesale_price_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_wholesale_price_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_wholesale_price_currency_price` (`field_wholesale_price_amount`,`field_wholesale_price_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 40 (field_wholesale_price)';

--
-- Table structure for table `field_data_inline_conditions`
--

DROP TABLE IF EXISTS `field_data_inline_conditions`;
CREATE TABLE `field_data_inline_conditions` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `inline_conditions_condition_name` varchar(255) NOT NULL COMMENT 'Name of the condition, as defined in hook_inline_condition_info().',
  `inline_conditions_condition_settings` longblob COMMENT 'Settings for the condition, serialized.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 67 (inline_conditions)';

--
-- Table structure for table `field_data_message_commerce_body`
--

DROP TABLE IF EXISTS `field_data_message_commerce_body`;
CREATE TABLE `field_data_message_commerce_body` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_body_value` longtext,
  `message_commerce_body_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_body_format` (`message_commerce_body_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 25 (message_commerce_body)';

--
-- Table structure for table `field_data_message_commerce_line_item`
--

DROP TABLE IF EXISTS `field_data_message_commerce_line_item`;
CREATE TABLE `field_data_message_commerce_line_item` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_line_item_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_line_item_target_id` (`message_commerce_line_item_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 23 (message_commerce_line_item)';

--
-- Table structure for table `field_data_message_commerce_order`
--

DROP TABLE IF EXISTS `field_data_message_commerce_order`;
CREATE TABLE `field_data_message_commerce_order` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_order_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_order_target_id` (`message_commerce_order_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 22 (message_commerce_order)';

--
-- Table structure for table `field_data_message_commerce_payment`
--

DROP TABLE IF EXISTS `field_data_message_commerce_payment`;
CREATE TABLE `field_data_message_commerce_payment` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_payment_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_payment_target_id` (`message_commerce_payment_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 24 (message_commerce_payment)';

--
-- Table structure for table `field_data_message_order_display_name`
--

DROP TABLE IF EXISTS `field_data_message_order_display_name`;
CREATE TABLE `field_data_message_order_display_name` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_order_display_name_value` varchar(255) DEFAULT NULL,
  `message_order_display_name_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_order_display_name_format` (`message_order_display_name_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 11 (message_order_display_name)';

--
-- Table structure for table `field_data_message_text`
--

DROP TABLE IF EXISTS `field_data_message_text`;
CREATE TABLE `field_data_message_text` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_text_value` longtext,
  `message_text_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_text_format` (`message_text_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 9 (message_text)';

--
-- Table structure for table `field_data_message_text_subject`
--

DROP TABLE IF EXISTS `field_data_message_text_subject`;
CREATE TABLE `field_data_message_text_subject` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_text_subject_value` varchar(255) DEFAULT NULL,
  `message_text_subject_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_text_subject_format` (`message_text_subject_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 10 (message_text_subject)';

--
-- Table structure for table `field_data_title_field`
--

DROP TABLE IF EXISTS `field_data_title_field`;
CREATE TABLE `field_data_title_field` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `title_field_value` varchar(255) DEFAULT NULL,
  `title_field_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `title_field_format` (`title_field_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 32 (title_field)';

--
-- Table structure for table `field_deleted_data_11`
--

DROP TABLE IF EXISTS `field_deleted_data_11`;
CREATE TABLE `field_deleted_data_11` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_order_display_name_value` varchar(255) DEFAULT NULL,
  `message_order_display_name_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_order_display_name_format` (`message_order_display_name_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 11 (message_order_display_name)';

--
-- Table structure for table `field_deleted_data_22`
--

DROP TABLE IF EXISTS `field_deleted_data_22`;
CREATE TABLE `field_deleted_data_22` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_order_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_order_target_id` (`message_commerce_order_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 22 (message_commerce_order)';

--
-- Table structure for table `field_deleted_data_23`
--

DROP TABLE IF EXISTS `field_deleted_data_23`;
CREATE TABLE `field_deleted_data_23` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_line_item_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_line_item_target_id` (`message_commerce_line_item_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 23 (message_commerce_line_item)';

--
-- Table structure for table `field_deleted_data_24`
--

DROP TABLE IF EXISTS `field_deleted_data_24`;
CREATE TABLE `field_deleted_data_24` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_payment_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_payment_target_id` (`message_commerce_payment_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 24 (message_commerce_payment)';

--
-- Table structure for table `field_deleted_data_25`
--

DROP TABLE IF EXISTS `field_deleted_data_25`;
CREATE TABLE `field_deleted_data_25` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned DEFAULT NULL COMMENT 'The entity revision id this data is attached to, or NULL if the entity type is not versioned',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_body_value` longtext,
  `message_commerce_body_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_body_format` (`message_commerce_body_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Data storage for field 25 (message_commerce_body)';

--
-- Table structure for table `field_deleted_revision_11`
--

DROP TABLE IF EXISTS `field_deleted_revision_11`;
CREATE TABLE `field_deleted_revision_11` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_order_display_name_value` varchar(255) DEFAULT NULL,
  `message_order_display_name_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_order_display_name_format` (`message_order_display_name_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 11 (message_order...';

--
-- Table structure for table `field_deleted_revision_22`
--

DROP TABLE IF EXISTS `field_deleted_revision_22`;
CREATE TABLE `field_deleted_revision_22` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_order_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_order_target_id` (`message_commerce_order_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 22 (message_commerce...';

--
-- Table structure for table `field_deleted_revision_23`
--

DROP TABLE IF EXISTS `field_deleted_revision_23`;
CREATE TABLE `field_deleted_revision_23` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_line_item_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_line_item_target_id` (`message_commerce_line_item_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 23 (message_commerce...';

--
-- Table structure for table `field_deleted_revision_24`
--

DROP TABLE IF EXISTS `field_deleted_revision_24`;
CREATE TABLE `field_deleted_revision_24` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_payment_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_payment_target_id` (`message_commerce_payment_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 24 (message_commerce...';

--
-- Table structure for table `field_deleted_revision_25`
--

DROP TABLE IF EXISTS `field_deleted_revision_25`;
CREATE TABLE `field_deleted_revision_25` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_body_value` longtext,
  `message_commerce_body_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_body_format` (`message_commerce_body_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 25 (message_commerce...';

--
-- Table structure for table `field_group`
--

DROP TABLE IF EXISTS `field_group`;
CREATE TABLE `field_group` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a group',
  `identifier` varchar(255) NOT NULL DEFAULT '' COMMENT 'The unique string identifier for a group.',
  `group_name` varchar(32) NOT NULL DEFAULT '' COMMENT 'The name of this group.',
  `entity_type` varchar(32) NOT NULL DEFAULT '',
  `bundle` varchar(128) NOT NULL DEFAULT '',
  `mode` varchar(128) NOT NULL DEFAULT '',
  `parent_name` varchar(32) NOT NULL DEFAULT '' COMMENT 'The parent name for a group',
  `data` longblob NOT NULL COMMENT 'Serialized data containing the group properties that do not warrant a dedicated column.',
  PRIMARY KEY (`id`),
  UNIQUE KEY `identifier` (`identifier`),
  KEY `group_name` (`group_name`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Table that contains field group entries and settings.';

--
-- Table structure for table `field_revision_body`
--

DROP TABLE IF EXISTS `field_revision_body`;
CREATE TABLE `field_revision_body` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `body_value` longtext,
  `body_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `body_format` (`body_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 20 (body)';

--
-- Table structure for table `field_revision_comment_body`
--

DROP TABLE IF EXISTS `field_revision_comment_body`;
CREATE TABLE `field_revision_comment_body` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `comment_body_value` longtext,
  `comment_body_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `comment_body_format` (`comment_body_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 21 (comment_body)';

--
-- Table structure for table `field_revision_commerce_customer_address`
--

DROP TABLE IF EXISTS `field_revision_commerce_customer_address`;
CREATE TABLE `field_revision_commerce_customer_address` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_customer_address_country` varchar(2) DEFAULT '' COMMENT 'Two letter ISO country code of this address.',
  `commerce_customer_address_administrative_area` varchar(255) DEFAULT '' COMMENT 'The administrative area of this address. (i.e. State/Province)',
  `commerce_customer_address_sub_administrative_area` varchar(255) DEFAULT '' COMMENT 'The sub administrative area of this address.',
  `commerce_customer_address_locality` varchar(255) DEFAULT '' COMMENT 'The locality of this address. (i.e. City)',
  `commerce_customer_address_dependent_locality` varchar(255) DEFAULT '' COMMENT 'The dependent locality of this address.',
  `commerce_customer_address_postal_code` varchar(255) DEFAULT '' COMMENT 'The postal code of this address.',
  `commerce_customer_address_thoroughfare` varchar(255) DEFAULT '' COMMENT 'The thoroughfare of this address. (i.e. Street address)',
  `commerce_customer_address_premise` varchar(255) DEFAULT '' COMMENT 'The premise of this address. (i.e. Apartment / Suite number)',
  `commerce_customer_address_sub_premise` varchar(255) DEFAULT '' COMMENT 'The sub_premise of this address.',
  `commerce_customer_address_organisation_name` varchar(255) DEFAULT '' COMMENT 'Contents of a primary OrganisationName element in the xNL XML.',
  `commerce_customer_address_name_line` varchar(255) DEFAULT '' COMMENT 'Contents of a primary NameLine element in the xNL XML.',
  `commerce_customer_address_first_name` varchar(255) DEFAULT '' COMMENT 'Contents of the FirstName element of a primary PersonName element in the xNL XML.',
  `commerce_customer_address_last_name` varchar(255) DEFAULT '' COMMENT 'Contents of the LastName element of a primary PersonName element in the xNL XML.',
  `commerce_customer_address_data` longtext COMMENT 'Additional data for this address.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 1 (commerce_customer...';

--
-- Table structure for table `field_revision_commerce_customer_billing`
--

DROP TABLE IF EXISTS `field_revision_commerce_customer_billing`;
CREATE TABLE `field_revision_commerce_customer_billing` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_customer_billing_profile_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_customer_billing_profile_id` (`commerce_customer_billing_profile_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 4 (commerce_customer...';

--
-- Table structure for table `field_revision_commerce_customer_shipping`
--

DROP TABLE IF EXISTS `field_revision_commerce_customer_shipping`;
CREATE TABLE `field_revision_commerce_customer_shipping` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_customer_shipping_profile_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_customer_shipping_profile_id` (`commerce_customer_shipping_profile_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 19 (commerce_customer...';

--
-- Table structure for table `field_revision_commerce_discount_date`
--

DROP TABLE IF EXISTS `field_revision_commerce_discount_date`;
CREATE TABLE `field_revision_commerce_discount_date` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_discount_date_value` int(11) DEFAULT NULL,
  `commerce_discount_date_value2` int(11) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 17 (commerce_discount...';

--
-- Table structure for table `field_revision_commerce_discount_offer`
--

DROP TABLE IF EXISTS `field_revision_commerce_discount_offer`;
CREATE TABLE `field_revision_commerce_discount_offer` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_discount_offer_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_discount_offer_target_id` (`commerce_discount_offer_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 13 (commerce_discount...';

--
-- Table structure for table `field_revision_commerce_discounts`
--

DROP TABLE IF EXISTS `field_revision_commerce_discounts`;
CREATE TABLE `field_revision_commerce_discounts` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_discounts_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_discounts_target_id` (`commerce_discounts_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 16 (commerce_discounts)';

--
-- Table structure for table `field_revision_commerce_display_path`
--

DROP TABLE IF EXISTS `field_revision_commerce_display_path`;
CREATE TABLE `field_revision_commerce_display_path` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_display_path_value` varchar(255) DEFAULT NULL,
  `commerce_display_path_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_display_path_format` (`commerce_display_path_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 8 (commerce_display_path)';

--
-- Table structure for table `field_revision_commerce_fixed_amount`
--

DROP TABLE IF EXISTS `field_revision_commerce_fixed_amount`;
CREATE TABLE `field_revision_commerce_fixed_amount` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_fixed_amount_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `commerce_fixed_amount_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `commerce_fixed_amount_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_fixed_amount_currency_price` (`commerce_fixed_amount_amount`,`commerce_fixed_amount_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 14 (commerce_fixed...';

--
-- Table structure for table `field_revision_commerce_free_products`
--

DROP TABLE IF EXISTS `field_revision_commerce_free_products`;
CREATE TABLE `field_revision_commerce_free_products` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_free_products_product_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_free_products_product_id` (`commerce_free_products_product_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 66 (commerce_free...';

--
-- Table structure for table `field_revision_commerce_free_shipping`
--

DROP TABLE IF EXISTS `field_revision_commerce_free_shipping`;
CREATE TABLE `field_revision_commerce_free_shipping` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_free_shipping_value` varchar(255) DEFAULT NULL,
  `commerce_free_shipping_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_free_shipping_format` (`commerce_free_shipping_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 65 (commerce_free...';

--
-- Table structure for table `field_revision_commerce_line_items`
--

DROP TABLE IF EXISTS `field_revision_commerce_line_items`;
CREATE TABLE `field_revision_commerce_line_items` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_line_items_line_item_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_line_items_line_item_id` (`commerce_line_items_line_item_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 2 (commerce_line_items)';

--
-- Table structure for table `field_revision_commerce_order_total`
--

DROP TABLE IF EXISTS `field_revision_commerce_order_total`;
CREATE TABLE `field_revision_commerce_order_total` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_order_total_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `commerce_order_total_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `commerce_order_total_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_order_total_currency_price` (`commerce_order_total_amount`,`commerce_order_total_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 3 (commerce_order_total)';

--
-- Table structure for table `field_revision_commerce_percentage`
--

DROP TABLE IF EXISTS `field_revision_commerce_percentage`;
CREATE TABLE `field_revision_commerce_percentage` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_percentage_value` decimal(10,2) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 15 (commerce_percentage)';

--
-- Table structure for table `field_revision_commerce_price`
--

DROP TABLE IF EXISTS `field_revision_commerce_price`;
CREATE TABLE `field_revision_commerce_price` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_price_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `commerce_price_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `commerce_price_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_price_currency_price` (`commerce_price_amount`,`commerce_price_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 12 (commerce_price)';

--
-- Table structure for table `field_revision_commerce_product`
--

DROP TABLE IF EXISTS `field_revision_commerce_product`;
CREATE TABLE `field_revision_commerce_product` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_product_product_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_product_product_id` (`commerce_product_product_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 7 (commerce_product)';

--
-- Table structure for table `field_revision_commerce_shipping_service`
--

DROP TABLE IF EXISTS `field_revision_commerce_shipping_service`;
CREATE TABLE `field_revision_commerce_shipping_service` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_shipping_service_value` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_shipping_service_value` (`commerce_shipping_service_value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 18 (commerce_shipping...';

--
-- Table structure for table `field_revision_commerce_total`
--

DROP TABLE IF EXISTS `field_revision_commerce_total`;
CREATE TABLE `field_revision_commerce_total` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_total_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `commerce_total_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `commerce_total_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_total_currency_price` (`commerce_total_amount`,`commerce_total_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 6 (commerce_total)';

--
-- Table structure for table `field_revision_commerce_unit_price`
--

DROP TABLE IF EXISTS `field_revision_commerce_unit_price`;
CREATE TABLE `field_revision_commerce_unit_price` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `commerce_unit_price_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `commerce_unit_price_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `commerce_unit_price_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `commerce_unit_price_currency_price` (`commerce_unit_price_amount`,`commerce_unit_price_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 5 (commerce_unit_price)';

--
-- Table structure for table `field_revision_field_availability_date`
--

DROP TABLE IF EXISTS `field_revision_field_availability_date`;
CREATE TABLE `field_revision_field_availability_date` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_availability_date_value` datetime DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 76 (field_availability...';

--
-- Table structure for table `field_revision_field_availability_period`
--

DROP TABLE IF EXISTS `field_revision_field_availability_period`;
CREATE TABLE `field_revision_field_availability_period` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_availability_period_value` varchar(11) DEFAULT NULL,
  `field_availability_period_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_availability_period_format` (`field_availability_period_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 77 (field_availability...';

--
-- Table structure for table `field_revision_field_brand`
--

DROP TABLE IF EXISTS `field_revision_field_brand`;
CREATE TABLE `field_revision_field_brand` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_brand_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_brand_tid` (`field_brand_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 37 (field_brand)';

--
-- Table structure for table `field_revision_field_collection`
--

DROP TABLE IF EXISTS `field_revision_field_collection`;
CREATE TABLE `field_revision_field_collection` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_collection_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_collection_tid` (`field_collection_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 38 (field_collection)';

--
-- Table structure for table `field_revision_field_colour`
--

DROP TABLE IF EXISTS `field_revision_field_colour`;
CREATE TABLE `field_revision_field_colour` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_colour_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_colour_tid` (`field_colour_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 36 (field_colour)';

--
-- Table structure for table `field_revision_field_colour_code`
--

DROP TABLE IF EXISTS `field_revision_field_colour_code`;
CREATE TABLE `field_revision_field_colour_code` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_colour_code_value` varchar(12) DEFAULT NULL,
  `field_colour_code_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_colour_code_format` (`field_colour_code_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 34 (field_colour_code)';

--
-- Table structure for table `field_revision_field_headline`
--

DROP TABLE IF EXISTS `field_revision_field_headline`;
CREATE TABLE `field_revision_field_headline` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_headline_value` varchar(20) DEFAULT NULL,
  `field_headline_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_headline_format` (`field_headline_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 33 (field_headline)';

--
-- Table structure for table `field_revision_field_image`
--

DROP TABLE IF EXISTS `field_revision_field_image`;
CREATE TABLE `field_revision_field_image` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_image_fid` int(10) unsigned DEFAULT NULL COMMENT 'The file_managed.fid being referenced in this field.',
  `field_image_alt` varchar(512) DEFAULT NULL COMMENT 'Alternative image text, for the image’s ’alt’ attribute.',
  `field_image_title` varchar(1024) DEFAULT NULL COMMENT 'Image title text, for the image’s ’title’ attribute.',
  `field_image_width` int(10) unsigned DEFAULT NULL COMMENT 'The width of the image in pixels.',
  `field_image_height` int(10) unsigned DEFAULT NULL COMMENT 'The height of the image in pixels.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_image_fid` (`field_image_fid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 29 (field_image)';

--
-- Table structure for table `field_revision_field_images`
--

DROP TABLE IF EXISTS `field_revision_field_images`;
CREATE TABLE `field_revision_field_images` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_images_fid` int(10) unsigned DEFAULT NULL COMMENT 'The file_managed.fid being referenced in this field.',
  `field_images_alt` varchar(512) DEFAULT NULL COMMENT 'Alternative image text, for the image’s ’alt’ attribute.',
  `field_images_title` varchar(1024) DEFAULT NULL COMMENT 'Image title text, for the image’s ’title’ attribute.',
  `field_images_width` int(10) unsigned DEFAULT NULL COMMENT 'The width of the image in pixels.',
  `field_images_height` int(10) unsigned DEFAULT NULL COMMENT 'The height of the image in pixels.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_images_fid` (`field_images_fid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 26 (field_images)';

--
-- Table structure for table `field_revision_field_link`
--

DROP TABLE IF EXISTS `field_revision_field_link`;
CREATE TABLE `field_revision_field_link` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_link_url` varchar(2048) DEFAULT NULL,
  `field_link_title` varchar(255) DEFAULT NULL,
  `field_link_attributes` mediumtext,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 30 (field_link)';

--
-- Table structure for table `field_revision_field_price_pl_01`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_01`;
CREATE TABLE `field_revision_field_price_pl_01` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_01_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_01_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_01_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_01_currency_price` (`field_price_pl_01_amount`,`field_price_pl_01_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 48 (field_price_pl_01)';

--
-- Table structure for table `field_revision_field_price_pl_02`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_02`;
CREATE TABLE `field_revision_field_price_pl_02` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_02_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_02_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_02_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_02_currency_price` (`field_price_pl_02_amount`,`field_price_pl_02_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 49 (field_price_pl_02)';

--
-- Table structure for table `field_revision_field_price_pl_03`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_03`;
CREATE TABLE `field_revision_field_price_pl_03` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_03_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_03_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_03_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_03_currency_price` (`field_price_pl_03_amount`,`field_price_pl_03_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 50 (field_price_pl_03)';

--
-- Table structure for table `field_revision_field_price_pl_04`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_04`;
CREATE TABLE `field_revision_field_price_pl_04` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_04_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_04_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_04_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_04_currency_price` (`field_price_pl_04_amount`,`field_price_pl_04_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 51 (field_price_pl_04)';

--
-- Table structure for table `field_revision_field_price_pl_05`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_05`;
CREATE TABLE `field_revision_field_price_pl_05` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_05_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_05_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_05_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_05_currency_price` (`field_price_pl_05_amount`,`field_price_pl_05_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 52 (field_price_pl_05)';

--
-- Table structure for table `field_revision_field_price_pl_06`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_06`;
CREATE TABLE `field_revision_field_price_pl_06` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_06_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_06_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_06_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_06_currency_price` (`field_price_pl_06_amount`,`field_price_pl_06_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 53 (field_price_pl_06)';

--
-- Table structure for table `field_revision_field_price_pl_07`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_07`;
CREATE TABLE `field_revision_field_price_pl_07` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_07_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_07_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_07_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_07_currency_price` (`field_price_pl_07_amount`,`field_price_pl_07_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 54 (field_price_pl_07)';

--
-- Table structure for table `field_revision_field_price_pl_08`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_08`;
CREATE TABLE `field_revision_field_price_pl_08` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_08_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_08_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_08_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_08_currency_price` (`field_price_pl_08_amount`,`field_price_pl_08_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 55 (field_price_pl_08)';

--
-- Table structure for table `field_revision_field_price_pl_09`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_09`;
CREATE TABLE `field_revision_field_price_pl_09` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_09_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_09_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_09_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_09_currency_price` (`field_price_pl_09_amount`,`field_price_pl_09_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 56 (field_price_pl_09)';

--
-- Table structure for table `field_revision_field_price_pl_10`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_10`;
CREATE TABLE `field_revision_field_price_pl_10` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_10_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_10_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_10_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_10_currency_price` (`field_price_pl_10_amount`,`field_price_pl_10_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 57 (field_price_pl_10)';

--
-- Table structure for table `field_revision_field_price_pl_11`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_11`;
CREATE TABLE `field_revision_field_price_pl_11` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_11_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_11_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_11_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_11_currency_price` (`field_price_pl_11_amount`,`field_price_pl_11_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 58 (field_price_pl_11)';

--
-- Table structure for table `field_revision_field_price_pl_12`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_12`;
CREATE TABLE `field_revision_field_price_pl_12` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_12_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_12_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_12_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_12_currency_price` (`field_price_pl_12_amount`,`field_price_pl_12_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 60 (field_price_pl_12)';

--
-- Table structure for table `field_revision_field_price_pl_13`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_13`;
CREATE TABLE `field_revision_field_price_pl_13` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_13_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_13_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_13_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_13_currency_price` (`field_price_pl_13_amount`,`field_price_pl_13_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 64 (field_price_pl_13)';

--
-- Table structure for table `field_revision_field_price_pl_14`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_14`;
CREATE TABLE `field_revision_field_price_pl_14` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_14_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_14_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_14_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_14_currency_price` (`field_price_pl_14_amount`,`field_price_pl_14_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 68 (field_price_pl_14)';

--
-- Table structure for table `field_revision_field_price_pl_15`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_15`;
CREATE TABLE `field_revision_field_price_pl_15` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_15_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_15_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_15_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_15_currency_price` (`field_price_pl_15_amount`,`field_price_pl_15_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 69 (field_price_pl_15)';

--
-- Table structure for table `field_revision_field_price_pl_16`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_16`;
CREATE TABLE `field_revision_field_price_pl_16` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_16_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_16_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_16_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_16_currency_price` (`field_price_pl_16_amount`,`field_price_pl_16_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 70 (field_price_pl_16)';

--
-- Table structure for table `field_revision_field_price_pl_17`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_17`;
CREATE TABLE `field_revision_field_price_pl_17` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_17_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_17_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_17_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_17_currency_price` (`field_price_pl_17_amount`,`field_price_pl_17_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 71 (field_price_pl_17)';

--
-- Table structure for table `field_revision_field_price_pl_18`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_18`;
CREATE TABLE `field_revision_field_price_pl_18` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_18_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_18_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_18_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_18_currency_price` (`field_price_pl_18_amount`,`field_price_pl_18_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 72 (field_price_pl_18)';

--
-- Table structure for table `field_revision_field_price_pl_19`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_19`;
CREATE TABLE `field_revision_field_price_pl_19` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_19_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_19_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_19_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_19_currency_price` (`field_price_pl_19_amount`,`field_price_pl_19_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 73 (field_price_pl_19)';

--
-- Table structure for table `field_revision_field_price_pl_20`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_20`;
CREATE TABLE `field_revision_field_price_pl_20` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_20_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_20_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_20_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_20_currency_price` (`field_price_pl_20_amount`,`field_price_pl_20_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 74 (field_price_pl_20)';

--
-- Table structure for table `field_revision_field_price_pl_21`
--

DROP TABLE IF EXISTS `field_revision_field_price_pl_21`;
CREATE TABLE `field_revision_field_price_pl_21` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_price_pl_21_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_price_pl_21_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_price_pl_21_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_price_pl_21_currency_price` (`field_price_pl_21_amount`,`field_price_pl_21_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 75 (field_price_pl_21)';

--
-- Table structure for table `field_revision_field_product`
--

DROP TABLE IF EXISTS `field_revision_field_product`;
CREATE TABLE `field_revision_field_product` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_product_product_id` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_product_product_id` (`field_product_product_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 27 (field_product)';

--
-- Table structure for table `field_revision_field_product_category`
--

DROP TABLE IF EXISTS `field_revision_field_product_category`;
CREATE TABLE `field_revision_field_product_category` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_product_category_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_product_category_tid` (`field_product_category_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 28 (field_product...';

--
-- Table structure for table `field_revision_field_required_delivery_date`
--

DROP TABLE IF EXISTS `field_revision_field_required_delivery_date`;
CREATE TABLE `field_revision_field_required_delivery_date` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_required_delivery_date_value` datetime DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 62 (field_required...';

--
-- Table structure for table `field_revision_field_rgb`
--

DROP TABLE IF EXISTS `field_revision_field_rgb`;
CREATE TABLE `field_revision_field_rgb` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_rgb_value` varchar(7) DEFAULT NULL,
  `field_rgb_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_rgb_format` (`field_rgb_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 35 (field_rgb)';

--
-- Table structure for table `field_revision_field_special_request`
--

DROP TABLE IF EXISTS `field_revision_field_special_request`;
CREATE TABLE `field_revision_field_special_request` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_special_request_value` longtext,
  `field_special_request_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_special_request_format` (`field_special_request_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 63 (field_special...';

--
-- Table structure for table `field_revision_field_stock_status`
--

DROP TABLE IF EXISTS `field_revision_field_stock_status`;
CREATE TABLE `field_revision_field_stock_status` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_stock_status_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_stock_status_tid` (`field_stock_status_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 59 (field_stock_status)';

--
-- Table structure for table `field_revision_field_table`
--

DROP TABLE IF EXISTS `field_revision_field_table`;
CREATE TABLE `field_revision_field_table` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_table_value` longtext,
  `field_table_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_table_format` (`field_table_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 78 (field_table)';

--
-- Table structure for table `field_revision_field_table_`
--

DROP TABLE IF EXISTS `field_revision_field_table_`;
CREATE TABLE `field_revision_field_table_` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_table__value` longtext,
  `field_table__format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_table__format` (`field_table__format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 78 (field_table_)';

--
-- Table structure for table `field_revision_field_tagline`
--

DROP TABLE IF EXISTS `field_revision_field_tagline`;
CREATE TABLE `field_revision_field_tagline` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_tagline_value` varchar(80) DEFAULT NULL,
  `field_tagline_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_tagline_format` (`field_tagline_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 31 (field_tagline)';

--
-- Table structure for table `field_revision_field_trim_colour`
--

DROP TABLE IF EXISTS `field_revision_field_trim_colour`;
CREATE TABLE `field_revision_field_trim_colour` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_trim_colour_tid` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_trim_colour_tid` (`field_trim_colour_tid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 39 (field_trim_colour)';

--
-- Table structure for table `field_revision_field_wholesale_price`
--

DROP TABLE IF EXISTS `field_revision_field_wholesale_price`;
CREATE TABLE `field_revision_field_wholesale_price` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `field_wholesale_price_amount` int(11) NOT NULL DEFAULT '0' COMMENT 'The price amount.',
  `field_wholesale_price_currency_code` varchar(32) NOT NULL COMMENT 'The currency code for the price.',
  `field_wholesale_price_data` longtext COMMENT 'A serialized array of additional price data.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `field_wholesale_price_currency_price` (`field_wholesale_price_amount`,`field_wholesale_price_currency_code`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 40 (field_wholesale...';

--
-- Table structure for table `field_revision_inline_conditions`
--

DROP TABLE IF EXISTS `field_revision_inline_conditions`;
CREATE TABLE `field_revision_inline_conditions` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `inline_conditions_condition_name` varchar(255) NOT NULL COMMENT 'Name of the condition, as defined in hook_inline_condition_info().',
  `inline_conditions_condition_settings` longblob COMMENT 'Settings for the condition, serialized.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 67 (inline_conditions)';

--
-- Table structure for table `field_revision_message_commerce_body`
--

DROP TABLE IF EXISTS `field_revision_message_commerce_body`;
CREATE TABLE `field_revision_message_commerce_body` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_body_value` longtext,
  `message_commerce_body_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_body_format` (`message_commerce_body_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 25 (message_commerce...';

--
-- Table structure for table `field_revision_message_commerce_line_item`
--

DROP TABLE IF EXISTS `field_revision_message_commerce_line_item`;
CREATE TABLE `field_revision_message_commerce_line_item` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_line_item_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_line_item_target_id` (`message_commerce_line_item_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 23 (message_commerce...';

--
-- Table structure for table `field_revision_message_commerce_order`
--

DROP TABLE IF EXISTS `field_revision_message_commerce_order`;
CREATE TABLE `field_revision_message_commerce_order` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_order_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_order_target_id` (`message_commerce_order_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 22 (message_commerce...';

--
-- Table structure for table `field_revision_message_commerce_payment`
--

DROP TABLE IF EXISTS `field_revision_message_commerce_payment`;
CREATE TABLE `field_revision_message_commerce_payment` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_commerce_payment_target_id` int(10) unsigned NOT NULL COMMENT 'The id of the target entity.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_commerce_payment_target_id` (`message_commerce_payment_target_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 24 (message_commerce...';

--
-- Table structure for table `field_revision_message_order_display_name`
--

DROP TABLE IF EXISTS `field_revision_message_order_display_name`;
CREATE TABLE `field_revision_message_order_display_name` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_order_display_name_value` varchar(255) DEFAULT NULL,
  `message_order_display_name_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_order_display_name_format` (`message_order_display_name_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 11 (message_order...';

--
-- Table structure for table `field_revision_message_text`
--

DROP TABLE IF EXISTS `field_revision_message_text`;
CREATE TABLE `field_revision_message_text` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_text_value` longtext,
  `message_text_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_text_format` (`message_text_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 9 (message_text)';

--
-- Table structure for table `field_revision_message_text_subject`
--

DROP TABLE IF EXISTS `field_revision_message_text_subject`;
CREATE TABLE `field_revision_message_text_subject` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `message_text_subject_value` varchar(255) DEFAULT NULL,
  `message_text_subject_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `message_text_subject_format` (`message_text_subject_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 10 (message_text_subject)';

--
-- Table structure for table `field_revision_title_field`
--

DROP TABLE IF EXISTS `field_revision_title_field`;
CREATE TABLE `field_revision_title_field` (
  `entity_type` varchar(128) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `bundle` varchar(128) NOT NULL DEFAULT '' COMMENT 'The field instance bundle to which this row belongs, used when deleting a field instance',
  `deleted` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this data item has been deleted',
  `entity_id` int(10) unsigned NOT NULL COMMENT 'The entity id this data is attached to',
  `revision_id` int(10) unsigned NOT NULL COMMENT 'The entity revision id this data is attached to',
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language for this data item.',
  `delta` int(10) unsigned NOT NULL COMMENT 'The sequence number for this data item, used for multi-value fields',
  `title_field_value` varchar(255) DEFAULT NULL,
  `title_field_format` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`deleted`,`delta`,`language`),
  KEY `entity_type` (`entity_type`),
  KEY `bundle` (`bundle`),
  KEY `deleted` (`deleted`),
  KEY `entity_id` (`entity_id`),
  KEY `revision_id` (`revision_id`),
  KEY `language` (`language`),
  KEY `title_field_format` (`title_field_format`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Revision archive storage for field 32 (title_field)';

--
-- Table structure for table `file_managed`
--

DROP TABLE IF EXISTS `file_managed`;
CREATE TABLE `file_managed` (
  `fid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'File ID.',
  `uid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The users.uid of the user who is associated with the file.',
  `filename` varchar(255) NOT NULL DEFAULT '' COMMENT 'Name of the file with no path components. This may differ from the basename of the URI if the file is renamed to avoid overwriting an existing file.',
  `uri` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin NOT NULL DEFAULT '' COMMENT 'The URI to access the file (either local or remote).',
  `filemime` varchar(255) NOT NULL DEFAULT '' COMMENT 'The file’s MIME type.',
  `filesize` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT 'The size of the file in bytes.',
  `status` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A field indicating the status of the file. Two status are defined in core: temporary (0) and permanent (1). Temporary files older than DRUPAL_MAXIMUM_TEMP_FILE_AGE will be removed during a cron run.',
  `timestamp` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'UNIX timestamp for when the file was added.',
  PRIMARY KEY (`fid`),
  UNIQUE KEY `uri` (`uri`),
  KEY `uid` (`uid`),
  KEY `status` (`status`),
  KEY `timestamp` (`timestamp`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores information for uploaded files.';

--
-- Table structure for table `file_usage`
--

DROP TABLE IF EXISTS `file_usage`;
CREATE TABLE `file_usage` (
  `fid` int(10) unsigned NOT NULL COMMENT 'File ID.',
  `module` varchar(255) NOT NULL DEFAULT '' COMMENT 'The name of the module that is using the file.',
  `type` varchar(64) NOT NULL DEFAULT '' COMMENT 'The name of the object type in which the file is used.',
  `id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The primary key of the object using the file.',
  `count` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The number of times this file is used by this object.',
  PRIMARY KEY (`fid`,`type`,`id`,`module`),
  KEY `type_id` (`type`,`id`),
  KEY `fid_count` (`fid`,`count`),
  KEY `fid_module` (`fid`,`module`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Track where a file is used.';

--
-- Table structure for table `filter`
--

DROP TABLE IF EXISTS `filter`;
CREATE TABLE `filter` (
  `format` varchar(255) NOT NULL COMMENT 'Foreign key: The filter_format.format to which this filter is assigned.',
  `module` varchar(64) NOT NULL DEFAULT '' COMMENT 'The origin module of the filter.',
  `name` varchar(32) NOT NULL DEFAULT '' COMMENT 'Name of the filter being referenced.',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'Weight of filter within format.',
  `status` int(11) NOT NULL DEFAULT '0' COMMENT 'Filter enabled status. (1 = enabled, 0 = disabled)',
  `settings` longblob COMMENT 'A serialized array of name value pairs that store the filter settings for the specific format.',
  PRIMARY KEY (`format`,`name`),
  KEY `list` (`weight`,`module`,`name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Table that maps filters (HTML corrector) to text formats ...';

--
-- Table structure for table `filter_format`
--

DROP TABLE IF EXISTS `filter_format`;
CREATE TABLE `filter_format` (
  `format` varchar(255) NOT NULL COMMENT 'Primary Key: Unique machine name of the format.',
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'Name of the text format (Filtered HTML).',
  `cache` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Flag to indicate whether format is cacheable. (1 = cacheable, 0 = not cacheable)',
  `status` tinyint(3) unsigned NOT NULL DEFAULT '1' COMMENT 'The status of the text format. (1 = enabled, 0 = disabled)',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'Weight of text format to use when listing.',
  PRIMARY KEY (`format`),
  UNIQUE KEY `name` (`name`),
  KEY `status_weight` (`status`,`weight`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores text formats: custom groupings of filters, such as...';

--
-- Table structure for table `flood`
--

DROP TABLE IF EXISTS `flood`;
CREATE TABLE `flood` (
  `fid` int(11) NOT NULL AUTO_INCREMENT COMMENT 'Unique flood event ID.',
  `event` varchar(64) NOT NULL DEFAULT '' COMMENT 'Name of event (e.g. contact).',
  `identifier` varchar(128) NOT NULL DEFAULT '' COMMENT 'Identifier of the visitor, such as an IP address or hostname.',
  `timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'Timestamp of the event.',
  `expiration` int(11) NOT NULL DEFAULT '0' COMMENT 'Expiration timestamp. Expired events are purged on cron run.',
  PRIMARY KEY (`fid`),
  KEY `allow` (`event`,`identifier`,`timestamp`),
  KEY `purge` (`expiration`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Flood controls the threshold of events, such as the...';

--
-- Table structure for table `history`
--

DROP TABLE IF EXISTS `history`;
CREATE TABLE `history` (
  `uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that read the node nid.',
  `nid` int(11) NOT NULL DEFAULT '0' COMMENT 'The node.nid that was read.',
  `timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp at which the read occurred.',
  PRIMARY KEY (`uid`,`nid`),
  KEY `nid` (`nid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='A record of which users have read which nodes.';

--
-- Table structure for table `honeypot_user`
--

DROP TABLE IF EXISTS `honeypot_user`;
CREATE TABLE `honeypot_user` (
  `uid` int(10) unsigned NOT NULL COMMENT 'Foreign key to users.uid; uniquely identifies a Drupal user to whom this ACL data applies.',
  `timestamp` int(10) unsigned NOT NULL COMMENT 'Date/time when the form submission failed, as Unix timestamp.',
  KEY `uid` (`uid`),
  KEY `timestamp` (`timestamp`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Table that stores failed attempts to submit a form.';

--
-- Table structure for table `image_effects`
--

DROP TABLE IF EXISTS `image_effects`;
CREATE TABLE `image_effects` (
  `ieid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for an image effect.',
  `isid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The image_styles.isid for an image style.',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'The weight of the effect in the style.',
  `name` varchar(255) NOT NULL COMMENT 'The unique name of the effect to be executed.',
  `data` longblob NOT NULL COMMENT 'The configuration data for the effect.',
  PRIMARY KEY (`ieid`),
  KEY `isid` (`isid`),
  KEY `weight` (`weight`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores configuration options for image effects.';

--
-- Table structure for table `image_styles`
--

DROP TABLE IF EXISTS `image_styles`;
CREATE TABLE `image_styles` (
  `isid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for an image style.',
  `name` varchar(255) NOT NULL COMMENT 'The style name.',
  `label` varchar(255) NOT NULL DEFAULT '' COMMENT 'The style administrative name.',
  PRIMARY KEY (`isid`),
  UNIQUE KEY `name` (`name`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores configuration options for image styles.';

--
-- Table structure for table `job_schedule`
--

DROP TABLE IF EXISTS `job_schedule`;
CREATE TABLE `job_schedule` (
  `item_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique item ID.',
  `name` varchar(128) NOT NULL DEFAULT '' COMMENT 'Name of the schedule.',
  `type` varchar(128) NOT NULL DEFAULT '' COMMENT 'Type identifier of the job.',
  `id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Numeric identifier of the job.',
  `period` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Time period after which job is to be executed.',
  `crontab` varchar(255) NOT NULL DEFAULT '' COMMENT 'Crontab line in *NIX format.',
  `data` longblob COMMENT 'The arbitrary data for the item.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'Timestamp when job expires.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'Timestamp when the item was created.',
  `last` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Timestamp when a job was last executed.',
  `periodic` smallint(5) unsigned NOT NULL DEFAULT '0' COMMENT 'If true job will be automatically rescheduled.',
  `next` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Timestamp when a job is to be executed (next = last + period), used for fast ordering.',
  `scheduled` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Timestamp when a job was scheduled. 0 if a job is currently not scheduled.',
  PRIMARY KEY (`item_id`),
  KEY `name_type_id` (`name`,`type`,`id`),
  KEY `name_type` (`name`,`type`),
  KEY `next` (`next`),
  KEY `scheduled` (`scheduled`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Schedule of jobs to be executed.';

--
-- Table structure for table `masquerade`
--

DROP TABLE IF EXISTS `masquerade`;
CREATE TABLE `masquerade` (
  `sid` varchar(64) NOT NULL DEFAULT '' COMMENT 'The current session for this masquerading user corresponding to their sessions.sid.',
  `uid_from` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid corresponding to a session.',
  `uid_as` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid this session is masquerading as.',
  KEY `sid` (`sid`,`uid_from`),
  KEY `sid_2` (`sid`,`uid_as`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Each masquerading user has their session recorded into...';

--
-- Table structure for table `masquerade_users`
--

DROP TABLE IF EXISTS `masquerade_users`;
CREATE TABLE `masquerade_users` (
  `uid_from` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that can masquerade as masquerade_users.uid_to.',
  `uid_to` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that masquerade_users.uid_from can masquerade as.',
  PRIMARY KEY (`uid_from`,`uid_to`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Per-user permission table granting permissions to switch...';

--
-- Table structure for table `megamenu`
--

DROP TABLE IF EXISTS `megamenu`;
CREATE TABLE `megamenu` (
  `menu_name` varchar(32) NOT NULL DEFAULT '' COMMENT 'The name of a Drupal menu and corresponding mega menu',
  `enabled` tinyint(4) DEFAULT '0' COMMENT 'Enabled state of a mega menu: 1 = enabled, 0 = dissabled',
  `skin` varchar(32) NOT NULL DEFAULT 'friendly' COMMENT 'Name of skin (CSS class)',
  `menu_orientation` varchar(12) NOT NULL DEFAULT 'horizontal' COMMENT 'Orientation of the entire menu (horizontal or vertical)',
  `slot_orientation` varchar(32) NOT NULL DEFAULT 'columnar' COMMENT 'Orientation CSS class to apply to slots (stacking or columnar)',
  `slot_attributes` tinytext COMMENT 'Custom CSS classes to apply to slots',
  PRIMARY KEY (`menu_name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores the enabled state and other attributes of mega menus.';

--
-- Table structure for table `menu_custom`
--

DROP TABLE IF EXISTS `menu_custom`;
CREATE TABLE `menu_custom` (
  `menu_name` varchar(32) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique key for menu. This is used as a block delta so length is 32.',
  `title` varchar(255) NOT NULL DEFAULT '' COMMENT 'Menu title; displayed at top of block.',
  `description` text COMMENT 'Menu description.',
  PRIMARY KEY (`menu_name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Holds definitions for top-level custom menus (for example...';

--
-- Table structure for table `menu_links`
--

DROP TABLE IF EXISTS `menu_links`;
CREATE TABLE `menu_links` (
  `menu_name` varchar(32) NOT NULL DEFAULT '' COMMENT 'The menu name. All links with the same menu name (such as ’navigation’) are part of the same menu.',
  `mlid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The menu link ID (mlid) is the integer primary key.',
  `plid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The parent link ID (plid) is the mlid of the link above in the hierarchy, or zero if the link is at the top level in its menu.',
  `link_path` varchar(255) NOT NULL DEFAULT '' COMMENT 'The Drupal path or external path this link points to.',
  `router_path` varchar(255) NOT NULL DEFAULT '' COMMENT 'For links corresponding to a Drupal path (external = 0), this connects the link to a menu_router.path for joins.',
  `link_title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The text displayed for the link, which may be modified by a title callback stored in menu_router.',
  `options` blob COMMENT 'A serialized array of options to be passed to the url() or l() function, such as a query string or HTML attributes.',
  `module` varchar(255) NOT NULL DEFAULT 'system' COMMENT 'The name of the module that generated this link.',
  `hidden` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag for whether the link should be rendered in menus. (1 = a disabled menu item that may be shown on admin screens, -1 = a menu callback, 0 = a normal, visible link)',
  `external` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate if the link points to a full URL starting with a protocol, like http:// (1 = external, 0 = internal).',
  `has_children` smallint(6) NOT NULL DEFAULT '0' COMMENT 'Flag indicating whether any links have this link as a parent (1 = children exist, 0 = no children).',
  `expanded` smallint(6) NOT NULL DEFAULT '0' COMMENT 'Flag for whether this link should be rendered as expanded in menus - expanded links always have their child links displayed, instead of only when the link is in the active trail (1 = expanded, 0 = not expanded)',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'Link weight among links in the same menu at the same depth.',
  `depth` smallint(6) NOT NULL DEFAULT '0' COMMENT 'The depth relative to the top level. A link with plid == 0 will have depth == 1.',
  `customized` smallint(6) NOT NULL DEFAULT '0' COMMENT 'A flag to indicate that the user has manually created or edited the link (1 = customized, 0 = not customized).',
  `p1` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The first mlid in the materialized path. If N = depth, then pN must equal the mlid. If depth > 1 then p(N-1) must equal the plid. All pX where X > depth must equal zero. The columns p1 .. p9 are also called the parents.',
  `p2` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The second mlid in the materialized path. See p1.',
  `p3` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The third mlid in the materialized path. See p1.',
  `p4` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The fourth mlid in the materialized path. See p1.',
  `p5` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The fifth mlid in the materialized path. See p1.',
  `p6` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The sixth mlid in the materialized path. See p1.',
  `p7` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The seventh mlid in the materialized path. See p1.',
  `p8` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The eighth mlid in the materialized path. See p1.',
  `p9` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The ninth mlid in the materialized path. See p1.',
  `updated` smallint(6) NOT NULL DEFAULT '0' COMMENT 'Flag that indicates that this link was generated during the update from Drupal 5.',
  PRIMARY KEY (`mlid`),
  KEY `path_menu` (`link_path`(128),`menu_name`),
  KEY `menu_plid_expand_child` (`menu_name`,`plid`,`expanded`,`has_children`),
  KEY `menu_parents` (`menu_name`,`p1`,`p2`,`p3`,`p4`,`p5`,`p6`,`p7`,`p8`,`p9`),
  KEY `router_path` (`router_path`(128))
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Contains the individual links within a menu.';

--
-- Table structure for table `menu_router`
--

DROP TABLE IF EXISTS `menu_router`;
CREATE TABLE `menu_router` (
  `path` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: the Drupal path this entry describes',
  `load_functions` blob NOT NULL COMMENT 'A serialized array of function names (like node_load) to be called to load an object corresponding to a part of the current path.',
  `to_arg_functions` blob NOT NULL COMMENT 'A serialized array of function names (like user_uid_optional_to_arg) to be called to replace a part of the router path with another string.',
  `access_callback` varchar(255) NOT NULL DEFAULT '' COMMENT 'The callback which determines the access to this router path. Defaults to user_access.',
  `access_arguments` blob COMMENT 'A serialized array of arguments for the access callback.',
  `page_callback` varchar(255) NOT NULL DEFAULT '' COMMENT 'The name of the function that renders the page.',
  `page_arguments` blob COMMENT 'A serialized array of arguments for the page callback.',
  `delivery_callback` varchar(255) NOT NULL DEFAULT '' COMMENT 'The name of the function that sends the result of the page_callback function to the browser.',
  `fit` int(11) NOT NULL DEFAULT '0' COMMENT 'A numeric representation of how specific the path is.',
  `number_parts` smallint(6) NOT NULL DEFAULT '0' COMMENT 'Number of parts in this router path.',
  `context` int(11) NOT NULL DEFAULT '0' COMMENT 'Only for local tasks (tabs) - the context of a local task to control its placement.',
  `tab_parent` varchar(255) NOT NULL DEFAULT '' COMMENT 'Only for local tasks (tabs) - the router path of the parent page (which may also be a local task).',
  `tab_root` varchar(255) NOT NULL DEFAULT '' COMMENT 'Router path of the closest non-tab parent page. For pages that are not local tasks, this will be the same as the path.',
  `title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The title for the current page, or the title for the tab if this is a local task.',
  `title_callback` varchar(255) NOT NULL DEFAULT '' COMMENT 'A function which will alter the title. Defaults to t()',
  `title_arguments` varchar(255) NOT NULL DEFAULT '' COMMENT 'A serialized array of arguments for the title callback. If empty, the title will be used as the sole argument for the title callback.',
  `theme_callback` varchar(255) NOT NULL DEFAULT '' COMMENT 'A function which returns the name of the theme that will be used to render this page. If left empty, the default theme will be used.',
  `theme_arguments` varchar(255) NOT NULL DEFAULT '' COMMENT 'A serialized array of arguments for the theme callback.',
  `type` int(11) NOT NULL DEFAULT '0' COMMENT 'Numeric representation of the type of the menu item, like MENU_LOCAL_TASK.',
  `description` text NOT NULL COMMENT 'A description of this item.',
  `position` varchar(255) NOT NULL DEFAULT '' COMMENT 'The position of the block (left or right) on the system administration page for this item.',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'Weight of the element. Lighter weights are higher up, heavier weights go down.',
  `include_file` mediumtext COMMENT 'The file to include for this element, usually the page callback function lives in this file.',
  PRIMARY KEY (`path`),
  KEY `fit` (`fit`),
  KEY `tab_parent` (`tab_parent`(64),`weight`,`title`),
  KEY `tab_root_weight_title` (`tab_root`(64),`weight`,`title`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Maps paths to various callbacks (access, page and title)';

--
-- Table structure for table `message`
--

DROP TABLE IF EXISTS `message`;
CREATE TABLE `message` (
  `mid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The Unique ID of the message.',
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT 'Reference to a message a type.',
  `arguments` text COMMENT 'Serialized array with the arguments',
  `uid` int(10) unsigned DEFAULT NULL COMMENT 'The user ID of the acting user.',
  `timestamp` int(10) unsigned NOT NULL COMMENT 'When the message instance was recorded.',
  `language` varchar(12) NOT NULL DEFAULT '' COMMENT 'The languages.language of this message.',
  PRIMARY KEY (`mid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='An instance of a message type (e.g. like a node is an...';

--
-- Table structure for table `message_type`
--

DROP TABLE IF EXISTS `message_type`;
CREATE TABLE `message_type` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Numeric message type ID.',
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'The unified identifier for a message type.',
  `category` varchar(255) NOT NULL DEFAULT 'message_type' COMMENT 'Reference to a message type category.',
  `description` varchar(255) NOT NULL DEFAULT '' COMMENT 'Description for this message type.',
  `argument_keys` text COMMENT 'Serialized array with the argument keys',
  `language` varchar(12) NOT NULL DEFAULT '' COMMENT 'The languages.language of this message type.',
  `status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'The exportable status of the entity.',
  `module` varchar(255) DEFAULT NULL COMMENT 'The name of the providing module if the entity has been defined in code.',
  `arguments` text COMMENT 'Serialized array with the arguments.',
  `data` longtext COMMENT 'Serialized array with general data.',
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Storage for user-defined message templates.';

--
-- Table structure for table `message_type_category`
--

DROP TABLE IF EXISTS `message_type_category`;
CREATE TABLE `message_type_category` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Numeric message type category ID.',
  `category` varchar(255) NOT NULL DEFAULT '' COMMENT 'The unified identifier for a message type category.',
  `description` varchar(255) NOT NULL DEFAULT '' COMMENT 'Description for this message type category.',
  `language` varchar(12) NOT NULL DEFAULT '' COMMENT 'The languages.language of this message type category.',
  `status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'The exportable status of the entity.',
  `module` varchar(255) DEFAULT NULL COMMENT 'The name of the providing module if the entity has been defined in code.',
  PRIMARY KEY (`id`),
  UNIQUE KEY `category` (`category`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Storage for user-defined message category.';

--
-- Table structure for table `metatag`
--

DROP TABLE IF EXISTS `metatag`;
CREATE TABLE `metatag` (
  `entity_type` varchar(32) NOT NULL DEFAULT '' COMMENT 'The entity type this data is attached to',
  `entity_id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The entity id this data is attached to',
  `data` longblob NOT NULL,
  `language` varchar(32) NOT NULL DEFAULT '' COMMENT 'The language of the tag.',
  `revision_id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The revision_id for the entity object this data is attached to.',
  PRIMARY KEY (`entity_type`,`entity_id`,`revision_id`,`language`),
  KEY `type_revision` (`entity_type`,`revision_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `metatag_config`
--

DROP TABLE IF EXISTS `metatag_config`;
CREATE TABLE `metatag_config` (
  `cid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a metatag configuration set.',
  `instance` varchar(255) NOT NULL DEFAULT '' COMMENT 'The machine-name of the configuration, typically entity-type:bundle.',
  `config` longblob NOT NULL COMMENT 'Serialized data containing the meta tag configuration.',
  PRIMARY KEY (`cid`),
  UNIQUE KEY `instance` (`instance`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Storage of meta tag configuration and defaults.';

--
-- Table structure for table `migrate_log`
--

DROP TABLE IF EXISTS `migrate_log`;
CREATE TABLE `migrate_log` (
  `mlid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Primary key for migrate_log table',
  `machine_name` varchar(255) NOT NULL COMMENT 'Unique machine name for migration',
  `process_type` tinyint(3) unsigned NOT NULL COMMENT 'Type of migration process - 1 for import, 2 for rollback',
  `starttime` bigint(20) unsigned NOT NULL COMMENT 'Begin time of a migration process, times 1000',
  `endtime` bigint(20) unsigned DEFAULT NULL COMMENT 'End time of a migration process, times 1000',
  `initialhighwater` varchar(255) NOT NULL COMMENT 'Initial highwater mark',
  `finalhighwater` varchar(255) DEFAULT NULL COMMENT 'Final highwater mark',
  `numprocessed` int(10) unsigned DEFAULT NULL COMMENT 'Number of items processed',
  PRIMARY KEY (`mlid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='History of migration processes';

--
-- Table structure for table `migrate_map_commercekickstartadpush`
--

DROP TABLE IF EXISTS `migrate_map_commercekickstartadpush`;
CREATE TABLE `migrate_map_commercekickstartadpush` (
  `sourceid1` varchar(255) NOT NULL,
  `destid1` int(10) unsigned DEFAULT NULL COMMENT 'ID of destination node',
  `needs_update` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Indicates current status of the source row',
  `last_imported` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'UNIX timestamp of the last time this row was imported',
  `rollback_action` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Flag indicating what to do for this item on rollback',
  PRIMARY KEY (`sourceid1`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Mappings from source key to destination key';

--
-- Table structure for table `migrate_map_commercekickstartnode`
--

DROP TABLE IF EXISTS `migrate_map_commercekickstartnode`;
CREATE TABLE `migrate_map_commercekickstartnode` (
  `sourceid1` varchar(255) NOT NULL,
  `destid1` int(10) unsigned DEFAULT NULL COMMENT 'ID of destination node',
  `needs_update` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Indicates current status of the source row',
  `last_imported` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'UNIX timestamp of the last time this row was imported',
  `rollback_action` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Flag indicating what to do for this item on rollback',
  PRIMARY KEY (`sourceid1`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Mappings from source key to destination key';

--
-- Table structure for table `migrate_map_commercekickstartpages`
--

DROP TABLE IF EXISTS `migrate_map_commercekickstartpages`;
CREATE TABLE `migrate_map_commercekickstartpages` (
  `sourceid1` varchar(255) NOT NULL,
  `destid1` int(10) unsigned DEFAULT NULL COMMENT 'ID of destination node',
  `needs_update` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Indicates current status of the source row',
  `last_imported` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'UNIX timestamp of the last time this row was imported',
  `rollback_action` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Flag indicating what to do for this item on rollback',
  PRIMARY KEY (`sourceid1`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Mappings from source key to destination key';

--
-- Table structure for table `migrate_map_commercekickstartproduct`
--

DROP TABLE IF EXISTS `migrate_map_commercekickstartproduct`;
CREATE TABLE `migrate_map_commercekickstartproduct` (
  `sourceid1` varchar(32) NOT NULL,
  `destid1` int(10) unsigned DEFAULT NULL,
  `destid2` int(10) unsigned DEFAULT NULL,
  `needs_update` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Indicates current status of the source row',
  `last_imported` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'UNIX timestamp of the last time this row was imported',
  `rollback_action` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Flag indicating what to do for this item on rollback',
  PRIMARY KEY (`sourceid1`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Mappings from source key to destination key';

--
-- Table structure for table `migrate_map_commercekickstartslideshow`
--

DROP TABLE IF EXISTS `migrate_map_commercekickstartslideshow`;
CREATE TABLE `migrate_map_commercekickstartslideshow` (
  `sourceid1` varchar(255) NOT NULL,
  `destid1` int(10) unsigned DEFAULT NULL COMMENT 'ID of destination node',
  `needs_update` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Indicates current status of the source row',
  `last_imported` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'UNIX timestamp of the last time this row was imported',
  `rollback_action` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Flag indicating what to do for this item on rollback',
  PRIMARY KEY (`sourceid1`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Mappings from source key to destination key';

--
-- Table structure for table `migrate_message_commercekickstartadpush`
--

DROP TABLE IF EXISTS `migrate_message_commercekickstartadpush`;
CREATE TABLE `migrate_message_commercekickstartadpush` (
  `msgid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `sourceid1` varchar(255) NOT NULL,
  `level` int(10) unsigned NOT NULL DEFAULT '1',
  `message` mediumtext NOT NULL,
  PRIMARY KEY (`msgid`),
  KEY `sourcekey` (`sourceid1`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Messages generated during a migration process';

--
-- Table structure for table `migrate_message_commercekickstartnode`
--

DROP TABLE IF EXISTS `migrate_message_commercekickstartnode`;
CREATE TABLE `migrate_message_commercekickstartnode` (
  `msgid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `sourceid1` varchar(255) NOT NULL,
  `level` int(10) unsigned NOT NULL DEFAULT '1',
  `message` mediumtext NOT NULL,
  PRIMARY KEY (`msgid`),
  KEY `sourcekey` (`sourceid1`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Messages generated during a migration process';

--
-- Table structure for table `migrate_message_commercekickstartpages`
--

DROP TABLE IF EXISTS `migrate_message_commercekickstartpages`;
CREATE TABLE `migrate_message_commercekickstartpages` (
  `msgid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `sourceid1` varchar(255) NOT NULL,
  `level` int(10) unsigned NOT NULL DEFAULT '1',
  `message` mediumtext NOT NULL,
  PRIMARY KEY (`msgid`),
  KEY `sourcekey` (`sourceid1`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Messages generated during a migration process';

--
-- Table structure for table `migrate_message_commercekickstartproduct`
--

DROP TABLE IF EXISTS `migrate_message_commercekickstartproduct`;
CREATE TABLE `migrate_message_commercekickstartproduct` (
  `msgid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `sourceid1` varchar(32) NOT NULL,
  `level` int(10) unsigned NOT NULL DEFAULT '1',
  `message` mediumtext NOT NULL,
  PRIMARY KEY (`msgid`),
  KEY `sourcekey` (`sourceid1`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Messages generated during a migration process';

--
-- Table structure for table `migrate_message_commercekickstartslideshow`
--

DROP TABLE IF EXISTS `migrate_message_commercekickstartslideshow`;
CREATE TABLE `migrate_message_commercekickstartslideshow` (
  `msgid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `sourceid1` varchar(255) NOT NULL,
  `level` int(10) unsigned NOT NULL DEFAULT '1',
  `message` mediumtext NOT NULL,
  PRIMARY KEY (`msgid`),
  KEY `sourcekey` (`sourceid1`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Messages generated during a migration process';

--
-- Table structure for table `migrate_status`
--

DROP TABLE IF EXISTS `migrate_status`;
CREATE TABLE `migrate_status` (
  `machine_name` varchar(255) NOT NULL COMMENT 'Unique machine name for migration',
  `class_name` varchar(255) NOT NULL COMMENT 'Name of class to instantiate for this migration',
  `status` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Current status of migration',
  `highwater` varchar(255) NOT NULL DEFAULT '' COMMENT 'Highwater mark for detecting updated content',
  `arguments` longblob COMMENT 'A serialized array of arguments to the migration constructor',
  PRIMARY KEY (`machine_name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Status information for migrations';

--
-- Table structure for table `node`
--

DROP TABLE IF EXISTS `node`;
CREATE TABLE `node` (
  `nid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a node.',
  `vid` int(10) unsigned DEFAULT NULL COMMENT 'The current node_revision.vid version identifier.',
  `type` varchar(32) NOT NULL DEFAULT '' COMMENT 'The node_type.type of this node.',
  `language` varchar(12) NOT NULL DEFAULT '' COMMENT 'The languages.language of this node.',
  `title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The title of this node, always treated as non-markup plain text.',
  `uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that owns this node; initially, this is the user that created it.',
  `status` int(11) NOT NULL DEFAULT '1' COMMENT 'Boolean indicating whether the node is published (visible to non-administrators).',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the node was created.',
  `changed` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the node was most recently saved.',
  `comment` int(11) NOT NULL DEFAULT '0' COMMENT 'Whether comments are allowed on this node: 0 = no, 1 = closed (read only), 2 = open (read/write).',
  `promote` int(11) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether the node should be displayed on the front page.',
  `sticky` int(11) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether the node should be displayed at the top of lists in which it appears.',
  `tnid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The translation set id for this node, which equals the node id of the source post in each set.',
  `translate` int(11) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this translation page needs to be updated.',
  PRIMARY KEY (`nid`),
  UNIQUE KEY `vid` (`vid`),
  KEY `node_changed` (`changed`),
  KEY `node_created` (`created`),
  KEY `node_frontpage` (`promote`,`status`,`sticky`,`created`),
  KEY `node_status_type` (`status`,`type`,`nid`),
  KEY `node_title_type` (`title`,`type`(4)),
  KEY `node_type` (`type`(4)),
  KEY `uid` (`uid`),
  KEY `tnid` (`tnid`),
  KEY `translate` (`translate`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='The base table for nodes.';

--
-- Table structure for table `node_access`
--

DROP TABLE IF EXISTS `node_access`;
CREATE TABLE `node_access` (
  `nid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The node.nid this record affects.',
  `gid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The grant ID a user must possess in the specified realm to gain this row’s privileges on the node.',
  `realm` varchar(255) NOT NULL DEFAULT '' COMMENT 'The realm in which the user must possess the grant ID. Each node access node can define one or more realms.',
  `grant_view` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether a user with the realm/grant pair can view this node.',
  `grant_update` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether a user with the realm/grant pair can edit this node.',
  `grant_delete` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether a user with the realm/grant pair can delete this node.',
  PRIMARY KEY (`nid`,`gid`,`realm`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Identifies which realm/grant pairs a user must possess in...';

--
-- Table structure for table `node_comment_statistics`
--

DROP TABLE IF EXISTS `node_comment_statistics`;
CREATE TABLE `node_comment_statistics` (
  `nid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The node.nid for which the statistics are compiled.',
  `cid` int(11) NOT NULL DEFAULT '0' COMMENT 'The comment.cid of the last comment.',
  `last_comment_timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp of the last comment that was posted within this node, from comment.changed.',
  `last_comment_name` varchar(60) DEFAULT NULL COMMENT 'The name of the latest author to post a comment on this node, from comment.name.',
  `last_comment_uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The user ID of the latest author to post a comment on this node, from comment.uid.',
  `comment_count` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The total number of comments on this node.',
  PRIMARY KEY (`nid`),
  KEY `node_comment_timestamp` (`last_comment_timestamp`),
  KEY `comment_count` (`comment_count`),
  KEY `last_comment_uid` (`last_comment_uid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Maintains statistics of node and comments posts to show ...';

--
-- Table structure for table `node_counter`
--

DROP TABLE IF EXISTS `node_counter`;
CREATE TABLE `node_counter` (
  `nid` int(11) NOT NULL DEFAULT '0' COMMENT 'The node.nid for these statistics.',
  `totalcount` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT 'The total number of times the node has been viewed.',
  `daycount` mediumint(8) unsigned NOT NULL DEFAULT '0' COMMENT 'The total number of times the node has been viewed today.',
  `timestamp` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The most recent time the node has been viewed.',
  PRIMARY KEY (`nid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Access statistics for nodes.';

--
-- Table structure for table `node_revision`
--

DROP TABLE IF EXISTS `node_revision`;
CREATE TABLE `node_revision` (
  `nid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The node this version belongs to.',
  `vid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for this version.',
  `uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid that created this version.',
  `title` varchar(255) NOT NULL DEFAULT '' COMMENT 'The title of this version.',
  `log` longtext NOT NULL COMMENT 'The log entry explaining the changes in this version.',
  `timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'A Unix timestamp indicating when this version was created.',
  `status` int(11) NOT NULL DEFAULT '1' COMMENT 'Boolean indicating whether the node (at the time of this revision) is published (visible to non-administrators).',
  `comment` int(11) NOT NULL DEFAULT '0' COMMENT 'Whether comments are allowed on this node (at the time of this revision): 0 = no, 1 = closed (read only), 2 = open (read/write).',
  `promote` int(11) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether the node (at the time of this revision) should be displayed on the front page.',
  `sticky` int(11) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether the node (at the time of this revision) should be displayed at the top of lists in which it appears.',
  PRIMARY KEY (`vid`),
  KEY `nid` (`nid`),
  KEY `uid` (`uid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores information about each saved version of a node.';

--
-- Table structure for table `node_spambot`
--

DROP TABLE IF EXISTS `node_spambot`;
CREATE TABLE `node_spambot` (
  `nid` int(10) unsigned NOT NULL DEFAULT '0',
  `uid` int(10) unsigned NOT NULL DEFAULT '0',
  `hostname` varchar(128) DEFAULT NULL,
  PRIMARY KEY (`nid`),
  KEY `uid` (`uid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Node table to track author IP addresses. For use by...';

--
-- Table structure for table `node_type`
--

DROP TABLE IF EXISTS `node_type`;
CREATE TABLE `node_type` (
  `type` varchar(32) NOT NULL COMMENT 'The machine-readable name of this type.',
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'The human-readable name of this type.',
  `base` varchar(255) NOT NULL COMMENT 'The base string used to construct callbacks corresponding to this node type.',
  `module` varchar(255) NOT NULL COMMENT 'The module defining this node type.',
  `description` mediumtext NOT NULL COMMENT 'A brief description of this type.',
  `help` mediumtext NOT NULL COMMENT 'Help information shown to the user when creating a node of this type.',
  `has_title` tinyint(3) unsigned NOT NULL COMMENT 'Boolean indicating whether this type uses the node.title field.',
  `title_label` varchar(255) NOT NULL DEFAULT '' COMMENT 'The label displayed for the title field on the edit form.',
  `custom` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this type is defined by a module (FALSE) or by a user via Add content type (TRUE).',
  `modified` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether this type has been modified by an administrator; currently not used in any way.',
  `locked` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether the administrator can change the machine name of this type.',
  `disabled` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean indicating whether the node type is disabled.',
  `orig_type` varchar(255) NOT NULL DEFAULT '' COMMENT 'The original machine-readable name of this node type. This may be different from the current type name if the locked field is 0.',
  PRIMARY KEY (`type`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores information about all defined node types.';

--
-- Table structure for table `page_title`
--

DROP TABLE IF EXISTS `page_title`;
CREATE TABLE `page_title` (
  `type` varchar(15) NOT NULL DEFAULT 'node',
  `id` int(10) unsigned NOT NULL DEFAULT '0',
  `page_title` varchar(255) NOT NULL DEFAULT '',
  PRIMARY KEY (`type`,`id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `queue`
--

DROP TABLE IF EXISTS `queue`;
CREATE TABLE `queue` (
  `item_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique item ID.',
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'The queue name.',
  `data` longblob COMMENT 'The arbitrary data for the item.',
  `expire` int(11) NOT NULL DEFAULT '0' COMMENT 'Timestamp when the claim lease expires on the item.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'Timestamp when the item was created.',
  PRIMARY KEY (`item_id`),
  KEY `name_created` (`name`,`created`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores items in queues.';

--
-- Table structure for table `redirect`
--

DROP TABLE IF EXISTS `redirect`;
CREATE TABLE `redirect` (
  `rid` int(11) NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique redirect ID.',
  `hash` varchar(64) NOT NULL COMMENT 'A unique hash based on source, source_options, and language.',
  `type` varchar(64) NOT NULL DEFAULT '' COMMENT 'The redirect type; if value is ’redirect’ it is a normal redirect handled by the module.',
  `uid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The users.uid of the user who created the redirect.',
  `source` varchar(255) NOT NULL COMMENT 'The source path to redirect from.',
  `source_options` text NOT NULL COMMENT 'A serialized array of source options.',
  `redirect` varchar(255) NOT NULL COMMENT 'The destination path to redirect to.',
  `redirect_options` text NOT NULL COMMENT 'A serialized array of redirect options.',
  `language` varchar(12) NOT NULL DEFAULT 'und' COMMENT 'The language this redirect is for; if blank, the alias will be used for unknown languages.',
  `status_code` smallint(6) NOT NULL COMMENT 'The HTTP status code to use for the redirect.',
  `count` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The number of times the redirect has been used.',
  `access` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The timestamp of when the redirect was last accessed.',
  PRIMARY KEY (`rid`),
  UNIQUE KEY `hash` (`hash`),
  KEY `expires` (`type`,`access`),
  KEY `source_language` (`source`,`language`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores information on redirects.';

--
-- Table structure for table `registry`
--

DROP TABLE IF EXISTS `registry`;
CREATE TABLE `registry` (
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'The name of the function, class, or interface.',
  `type` varchar(9) NOT NULL DEFAULT '' COMMENT 'Either function or class or interface.',
  `filename` varchar(255) NOT NULL COMMENT 'Name of the file.',
  `module` varchar(255) NOT NULL DEFAULT '' COMMENT 'Name of the module the file belongs to.',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'The order in which this module’s hooks should be invoked relative to other modules. Equal-weighted modules are ordered by name.',
  PRIMARY KEY (`name`,`type`),
  KEY `hook` (`type`,`weight`,`module`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Each record is a function, class, or interface name and...';

--
-- Table structure for table `registry_file`
--

DROP TABLE IF EXISTS `registry_file`;
CREATE TABLE `registry_file` (
  `filename` varchar(255) NOT NULL COMMENT 'Path to the file.',
  `hash` varchar(64) NOT NULL COMMENT 'sha-256 hash of the file’s contents when last parsed.',
  PRIMARY KEY (`filename`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Files parsed to build the registry.';

--
-- Table structure for table `role`
--

DROP TABLE IF EXISTS `role`;
CREATE TABLE `role` (
  `rid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique role ID.',
  `name` varchar(64) NOT NULL DEFAULT '' COMMENT 'Unique role name.',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'The weight of this role in listings and the user interface.',
  PRIMARY KEY (`rid`),
  UNIQUE KEY `name` (`name`),
  KEY `name_weight` (`name`,`weight`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores user roles.';

--
-- Table structure for table `role_permission`
--

DROP TABLE IF EXISTS `role_permission`;
CREATE TABLE `role_permission` (
  `rid` int(10) unsigned NOT NULL COMMENT 'Foreign Key: role.rid.',
  `permission` varchar(128) NOT NULL DEFAULT '' COMMENT 'A single permission granted to the role identified by rid.',
  `module` varchar(255) NOT NULL DEFAULT '' COMMENT 'The module declaring the permission.',
  PRIMARY KEY (`rid`,`permission`),
  KEY `permission` (`permission`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores the permissions assigned to user roles.';

--
-- Table structure for table `rules_config`
--

DROP TABLE IF EXISTS `rules_config`;
CREATE TABLE `rules_config` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'The internal identifier for any configuration.',
  `name` varchar(64) NOT NULL COMMENT 'The name of the configuration.',
  `label` varchar(255) NOT NULL DEFAULT 'unlabeled' COMMENT 'The label of the configuration.',
  `plugin` varchar(127) NOT NULL COMMENT 'The name of the plugin of this configuration.',
  `active` int(11) NOT NULL DEFAULT '1' COMMENT 'Boolean indicating whether the configuration is active. Usage depends on how the using module makes use of it.',
  `weight` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Weight of the configuration. Usage depends on how the using module makes use of it.',
  `status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'The exportable status of the entity.',
  `dirty` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Dirty configurations fail the integrity check, e.g. due to missing dependencies.',
  `module` varchar(255) DEFAULT NULL COMMENT 'The name of the providing module if the entity has been defined in code.',
  `access_exposed` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Whether to use a permission to control access for using components.',
  `data` longblob COMMENT 'Everything else, serialized.',
  `owner` varchar(255) NOT NULL DEFAULT 'rules' COMMENT 'The name of the module via which the rule has been configured.',
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`),
  KEY `plugin` (`plugin`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8;

--
-- Table structure for table `rules_dependencies`
--

DROP TABLE IF EXISTS `rules_dependencies`;
CREATE TABLE `rules_dependencies` (
  `id` int(10) unsigned NOT NULL COMMENT 'The primary identifier of the configuration.',
  `module` varchar(255) NOT NULL COMMENT 'The name of the module that is required for the configuration.',
  PRIMARY KEY (`id`,`module`),
  KEY `module` (`module`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `rules_tags`
--

DROP TABLE IF EXISTS `rules_tags`;
CREATE TABLE `rules_tags` (
  `id` int(10) unsigned NOT NULL COMMENT 'The primary identifier of the configuration.',
  `tag` varchar(255) NOT NULL COMMENT 'The tag string associated with this configuration',
  PRIMARY KEY (`id`,`tag`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `rules_trigger`
--

DROP TABLE IF EXISTS `rules_trigger`;
CREATE TABLE `rules_trigger` (
  `id` int(10) unsigned NOT NULL COMMENT 'The primary identifier of the configuration.',
  `event` varchar(127) NOT NULL DEFAULT '' COMMENT 'The name of the event on which the configuration should be triggered.',
  PRIMARY KEY (`id`,`event`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_custom_product_display_field_product_commerce_pri`
--

DROP TABLE IF EXISTS `search_api_db_custom_product_display_field_product_commerce_pri`;
CREATE TABLE `search_api_db_custom_product_display_field_product_commerce_pri` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_custom_product_display_search_api_language`
--

DROP TABLE IF EXISTS `search_api_db_custom_product_display_search_api_language`;
CREATE TABLE `search_api_db_custom_product_display_search_api_language` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` varchar(255) DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display`
--

DROP TABLE IF EXISTS `search_api_db_product_display`;
CREATE TABLE `search_api_db_product_display` (
  `item_id` bigint(20) NOT NULL COMMENT 'The primary identifier of the item.',
  `day_views` bigint(20) DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `day_views` (`day_views`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_created`
--

DROP TABLE IF EXISTS `search_api_db_product_display_created`;
CREATE TABLE `search_api_db_product_display_created` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_brand`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_brand`;
CREATE TABLE `search_api_db_product_display_field_brand` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` int(11) DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_collection`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_collection`;
CREATE TABLE `search_api_db_product_display_field_collection` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` int(11) DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_category`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_category`;
CREATE TABLE `search_api_db_product_display_field_product_category` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` int(11) DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_commerce_price_am_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_commerce_price_am_1`;
CREATE TABLE `search_api_db_product_display_field_product_commerce_price_am_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_commerce_price_am_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_commerce_price_am_2`;
CREATE TABLE `search_api_db_product_display_field_product_commerce_price_am_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_commerce_price_amou`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_commerce_price_amou`;
CREATE TABLE `search_api_db_product_display_field_product_commerce_price_amou` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_colour`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_colour`;
CREATE TABLE `search_api_db_product_display_field_product_field_colour` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` int(11) NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_01_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_01_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_01_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_01_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_01_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_01_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_01_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_01_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_01_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_02_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_02_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_02_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_02_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_02_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_02_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_02_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_02_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_02_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_03_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_03_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_03_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_03_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_03_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_03_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_03_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_03_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_03_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_04_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_04_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_04_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_04_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_04_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_04_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_04_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_04_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_04_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_05_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_05_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_05_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_05_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_05_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_05_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_05_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_05_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_05_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_06_`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_06_`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_06_` (
  `item_id` bigint(20) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_06_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_06_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_06_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_06_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_06_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_06_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_06_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_06_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_06_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_07_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_07_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_07_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_07_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_07_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_07_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_07_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_07_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_07_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_08_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_08_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_08_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_08_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_08_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_08_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_08_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_08_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_08_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_09_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_09_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_09_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_09_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_09_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_09_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_09_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_09_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_09_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_10_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_10_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_10_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_10_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_10_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_10_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_10_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_10_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_10_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_11_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_11_1`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_11_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_11_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_11_2`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_11_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_11_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_11_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_11_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_price_pl_12_a`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_price_pl_12_a`;
CREATE TABLE `search_api_db_product_display_field_product_field_price_pl_12_a` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` float NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_stock_status`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_stock_status`;
CREATE TABLE `search_api_db_product_display_field_product_field_stock_status` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` int(11) NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_field_trim_colour`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_field_trim_colour`;
CREATE TABLE `search_api_db_product_display_field_product_field_trim_colour` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` int(11) NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_title`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_title`;
CREATE TABLE `search_api_db_product_display_field_product_title` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `word` varchar(50) NOT NULL COMMENT 'The text of the indexed token.',
  `score` float NOT NULL COMMENT 'The score associated with this token.',
  PRIMARY KEY (`item_id`,`word`),
  KEY `word` (`word`(10))
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_title_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_title_1`;
CREATE TABLE `search_api_db_product_display_field_product_title_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `word` varchar(50) NOT NULL COMMENT 'The text of the indexed token.',
  `score` float NOT NULL COMMENT 'The score associated with this token.',
  PRIMARY KEY (`item_id`,`word`),
  KEY `word` (`word`(10))
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_title_2`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_title_2`;
CREATE TABLE `search_api_db_product_display_field_product_title_2` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `word` varchar(50) NOT NULL COMMENT 'The text of the indexed token.',
  `score` float NOT NULL COMMENT 'The score associated with this token.',
  PRIMARY KEY (`item_id`,`word`),
  KEY `word` (`word`(10))
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_field_product_title_3`
--

DROP TABLE IF EXISTS `search_api_db_product_display_field_product_title_3`;
CREATE TABLE `search_api_db_product_display_field_product_title_3` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `word` varchar(50) NOT NULL COMMENT 'The text of the indexed token.',
  `score` float NOT NULL COMMENT 'The score associated with this token.',
  PRIMARY KEY (`item_id`,`word`),
  KEY `word` (`word`(10))
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_search_api_aggregation_1`
--

DROP TABLE IF EXISTS `search_api_db_product_display_search_api_aggregation_1`;
CREATE TABLE `search_api_db_product_display_search_api_aggregation_1` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `word` varchar(50) NOT NULL COMMENT 'The text of the indexed token.',
  `score` float NOT NULL COMMENT 'The score associated with this token.',
  PRIMARY KEY (`item_id`,`word`),
  KEY `word` (`word`(10))
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_search_api_language`
--

DROP TABLE IF EXISTS `search_api_db_product_display_search_api_language`;
CREATE TABLE `search_api_db_product_display_search_api_language` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` varchar(255) COLLATE utf8_bin DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

--
-- Table structure for table `search_api_db_product_display_status`
--

DROP TABLE IF EXISTS `search_api_db_product_display_status`;
CREATE TABLE `search_api_db_product_display_status` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` int(11) DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_display_text`
--

DROP TABLE IF EXISTS `search_api_db_product_display_text`;
CREATE TABLE `search_api_db_product_display_text` (
  `item_id` bigint(20) NOT NULL COMMENT 'The primary identifier of the item.',
  `field_name` varchar(255) COLLATE utf8_bin NOT NULL COMMENT 'The name of the field in which the token appears, or an MD5 hash of the field.',
  `word` varchar(50) COLLATE utf8_bin NOT NULL COMMENT 'The text of the indexed token.',
  `score` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The score associated with this token.',
  PRIMARY KEY (`item_id`,`field_name`,`word`),
  KEY `word_field` (`word`(20),`field_name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

--
-- Table structure for table `search_api_db_product_display_title`
--

DROP TABLE IF EXISTS `search_api_db_product_display_title`;
CREATE TABLE `search_api_db_product_display_title` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` varchar(255) COLLATE utf8_bin DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

--
-- Table structure for table `search_api_db_product_search_field_product_field_colour`
--

DROP TABLE IF EXISTS `search_api_db_product_search_field_product_field_colour`;
CREATE TABLE `search_api_db_product_search_field_product_field_colour` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` int(11) NOT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`,`value`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_search_search_api_language`
--

DROP TABLE IF EXISTS `search_api_db_product_search_search_api_language`;
CREATE TABLE `search_api_db_product_search_search_api_language` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `value` varchar(255) DEFAULT NULL COMMENT 'The field’s value for this item.',
  PRIMARY KEY (`item_id`),
  KEY `value` (`value`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_db_product_search_title_field`
--

DROP TABLE IF EXISTS `search_api_db_product_search_title_field`;
CREATE TABLE `search_api_db_product_search_title_field` (
  `item_id` int(11) NOT NULL COMMENT 'The primary identifier of the item.',
  `word` varchar(50) NOT NULL COMMENT 'The text of the indexed token.',
  `score` float NOT NULL COMMENT 'The score associated with this token.',
  PRIMARY KEY (`item_id`,`word`),
  KEY `word` (`word`(10))
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `search_api_index`
--

DROP TABLE IF EXISTS `search_api_index`;
CREATE TABLE `search_api_index` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'An integer identifying the index.',
  `name` varchar(50) NOT NULL COMMENT 'A name to be displayed for the index.',
  `machine_name` varchar(50) NOT NULL COMMENT 'The machine name of the index.',
  `description` text COMMENT 'A string describing the index’ use to users.',
  `server` varchar(50) DEFAULT NULL COMMENT 'The search_api_server.machine_name with which data should be indexed.',
  `item_type` varchar(50) NOT NULL COMMENT 'The type of items stored in this index.',
  `options` mediumtext NOT NULL COMMENT 'An array of additional arguments configuring this index.',
  `enabled` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'A flag indicating whether this index is enabled.',
  `read_only` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A flag indicating whether to write to this index.',
  `status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'The exportable status of the entity.',
  `module` varchar(255) DEFAULT NULL COMMENT 'The name of the providing module if the entity has been defined in code.',
  PRIMARY KEY (`id`),
  UNIQUE KEY `machine_name` (`machine_name`),
  KEY `item_type` (`item_type`),
  KEY `server` (`server`),
  KEY `enabled` (`enabled`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores all search indexes on a search_api_server.';

--
-- Table structure for table `search_api_item`
--

DROP TABLE IF EXISTS `search_api_item`;
CREATE TABLE `search_api_item` (
  `item_id` int(10) unsigned NOT NULL COMMENT 'The item’s entity id (e.g. node.nid for nodes).',
  `index_id` int(10) unsigned NOT NULL COMMENT 'The search_api_index.id this item belongs to.',
  `changed` bigint(20) NOT NULL DEFAULT '1' COMMENT 'Either a flag or a timestamp to indicate if or when the item was changed since it was last indexed.',
  PRIMARY KEY (`item_id`,`index_id`),
  KEY `indexing` (`index_id`,`changed`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores the items which should be indexed for each index,...';

--
-- Table structure for table `search_api_server`
--

DROP TABLE IF EXISTS `search_api_server`;
CREATE TABLE `search_api_server` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a server.',
  `name` varchar(50) NOT NULL COMMENT 'The displayed name for a server.',
  `machine_name` varchar(50) NOT NULL COMMENT 'The machine name for a server.',
  `description` text COMMENT 'The displayed description for a server.',
  `class` varchar(50) NOT NULL COMMENT 'The id of the service class to use for this server.',
  `options` mediumtext NOT NULL COMMENT 'The options used to configure the service object.',
  `enabled` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'A flag indicating whether the server is enabled.',
  `status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'The exportable status of the entity.',
  `module` varchar(255) DEFAULT NULL COMMENT 'The name of the providing module if the entity has been defined in code.',
  PRIMARY KEY (`id`),
  UNIQUE KEY `machine_name` (`machine_name`),
  KEY `enabled` (`enabled`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores all search servers created through the Search API.';

--
-- Table structure for table `search_api_sort`
--

DROP TABLE IF EXISTS `search_api_sort`;
CREATE TABLE `search_api_sort` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The primary identifier for a sort.',
  `identifier` varchar(255) NOT NULL COMMENT 'The identifier for this sort.',
  `index_id` varchar(50) NOT NULL COMMENT 'The search_api_index.machine_name this sort belongs to.',
  `field` varchar(255) NOT NULL COMMENT 'The index field this sort belongs to.',
  `name` varchar(80) NOT NULL COMMENT 'The human-readable name to be displayed for this sort.',
  `enabled` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'A flag indicating whether the sort is enabled.',
  `default_sort` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A flag indicating whether the sort is the default sort for the index.',
  `default_sort_no_terms` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A flag indicating whether the sort is the default sort for the index when there are no search terms present.',
  `default_order` varchar(4) NOT NULL DEFAULT 'desc' COMMENT 'The default order for this field.',
  `weight` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A weight field determining the sort order of the sorts in the block.',
  `status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'The exportable status of the entity.',
  `module` varchar(255) DEFAULT NULL COMMENT 'The name of the providing module if the entity has been defined in code.',
  PRIMARY KEY (`id`),
  KEY `field` (`index_id`,`field`(50)),
  KEY `enabled` (`enabled`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores all activated or configured sorts of a site.';

--
-- Table structure for table `search_api_task`
--

DROP TABLE IF EXISTS `search_api_task`;
CREATE TABLE `search_api_task` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'An integer identifying this task.',
  `server_id` varchar(50) NOT NULL COMMENT 'The search_api_server.machine_name for which this task should be executed.',
  `type` varchar(50) NOT NULL COMMENT 'A keyword identifying the type of task that should be executed.',
  `index_id` varchar(50) DEFAULT NULL COMMENT 'The search_api_index.machine_name to which this task pertains, if applicable for this type.',
  `data` mediumtext COMMENT 'Some data needed for the task, might be optional depending on the type.',
  PRIMARY KEY (`id`),
  KEY `server` (`server_id`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores pending tasks for servers.';

--
-- Table structure for table `security_review`
--

DROP TABLE IF EXISTS `security_review`;
CREATE TABLE `security_review` (
  `namespace` varchar(160) NOT NULL DEFAULT '',
  `reviewcheck` varchar(160) NOT NULL DEFAULT '',
  `result` int(11) NOT NULL DEFAULT '0',
  `lastrun` int(11) NOT NULL DEFAULT '0',
  `skip` int(11) NOT NULL DEFAULT '0',
  `skiptime` int(11) NOT NULL DEFAULT '0',
  `skipuid` int(11) DEFAULT NULL,
  PRIMARY KEY (`namespace`,`reviewcheck`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;

--
-- Table structure for table `semaphore`
--

DROP TABLE IF EXISTS `semaphore`;
CREATE TABLE `semaphore` (
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique name.',
  `value` varchar(255) NOT NULL DEFAULT '' COMMENT 'A value for the semaphore.',
  `expire` double NOT NULL COMMENT 'A Unix timestamp with microseconds indicating when the semaphore should expire.',
  PRIMARY KEY (`name`),
  KEY `value` (`value`),
  KEY `expire` (`expire`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Table for holding semaphores, locks, flags, etc. that...';

--
-- Table structure for table `sequences`
--

DROP TABLE IF EXISTS `sequences`;
CREATE TABLE `sequences` (
  `value` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The value of the sequence.',
  PRIMARY KEY (`value`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores IDs.';

--
-- Table structure for table `sessions`
--

DROP TABLE IF EXISTS `sessions`;
CREATE TABLE `sessions` (
  `uid` int(10) unsigned NOT NULL COMMENT 'The users.uid corresponding to a session, or 0 for anonymous user.',
  `sid` varchar(128) NOT NULL COMMENT 'A session ID. The value is generated by Drupal’s session handlers.',
  `ssid` varchar(128) NOT NULL DEFAULT '' COMMENT 'Secure session ID. The value is generated by Drupal’s session handlers.',
  `hostname` varchar(128) NOT NULL DEFAULT '' COMMENT 'The IP address that last used this session ID (sid).',
  `timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when this session last requested a page. Old records are purged by PHP automatically.',
  `cache` int(11) NOT NULL DEFAULT '0' COMMENT 'The time of this user’s last post. This is used when the site has specified a minimum_cache_lifetime. See cache_get().',
  `session` longblob COMMENT 'The serialized contents of $_SESSION, an array of name/value pairs that persists across page requests by this session ID. Drupal loads $_SESSION from here at the start of each request and saves it at the end.',
  PRIMARY KEY (`sid`,`ssid`),
  KEY `timestamp` (`timestamp`),
  KEY `uid` (`uid`),
  KEY `ssid` (`ssid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Drupal’s session handlers read and write into the...';

--
-- Table structure for table `site_verify`
--

DROP TABLE IF EXISTS `site_verify`;
CREATE TABLE `site_verify` (
  `svid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique site verification ID.',
  `engine` varchar(32) NOT NULL DEFAULT '',
  `file` varchar(255) DEFAULT '',
  `file_contents` longtext NOT NULL,
  `meta` text NOT NULL,
  PRIMARY KEY (`svid`),
  KEY `engine` (`engine`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8;

--
-- Table structure for table `system`
--

DROP TABLE IF EXISTS `system`;
CREATE TABLE `system` (
  `filename` varchar(255) NOT NULL DEFAULT '' COMMENT 'The path of the primary file for this item, relative to the Drupal root; e.g. modules/node/node.module.',
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'The name of the item; e.g. node.',
  `type` varchar(12) NOT NULL DEFAULT '' COMMENT 'The type of the item, either module, theme, or theme_engine.',
  `owner` varchar(255) NOT NULL DEFAULT '' COMMENT 'A theme’s ’parent’ . Can be either a theme or an engine.',
  `status` int(11) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether or not this item is enabled.',
  `bootstrap` int(11) NOT NULL DEFAULT '0' COMMENT 'Boolean indicating whether this module is loaded during Drupal’s early bootstrapping phase (e.g. even before the page cache is consulted).',
  `schema_version` smallint(6) NOT NULL DEFAULT '-1' COMMENT 'The module’s database schema version number. -1 if the module is not installed (its tables do not exist); 0 or the largest N of the module’s hook_update_N() function that has either been run or existed when the module was first installed.',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'The order in which this module’s hooks should be invoked relative to other modules. Equal-weighted modules are ordered by name.',
  `info` blob COMMENT 'A serialized array containing information from the module’s .info file; keys can include name, description, package, version, core, dependencies, and php.',
  PRIMARY KEY (`filename`),
  KEY `system_list` (`status`,`bootstrap`,`type`,`weight`,`name`),
  KEY `type_name` (`type`,`name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='A list of all modules, themes, and theme engines that are...';

--
-- Table structure for table `taxonomy_index`
--

DROP TABLE IF EXISTS `taxonomy_index`;
CREATE TABLE `taxonomy_index` (
  `nid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The node.nid this record tracks.',
  `tid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The term ID.',
  `sticky` tinyint(4) DEFAULT '0' COMMENT 'Boolean indicating whether the node is sticky.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'The Unix timestamp when the node was created.',
  KEY `term_node` (`tid`,`sticky`,`created`),
  KEY `nid` (`nid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Maintains denormalized information about node/term...';

--
-- Table structure for table `taxonomy_menu`
--

DROP TABLE IF EXISTS `taxonomy_menu`;
CREATE TABLE `taxonomy_menu` (
  `mlid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The taxonomy terms menu_link.mlid.',
  `tid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Tid that is linked to the mlid.',
  `vid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Vid for the tid.',
  PRIMARY KEY (`mlid`,`tid`),
  KEY `vid` (`vid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Links a taxonomy term to a menu item.';

--
-- Table structure for table `taxonomy_term_data`
--

DROP TABLE IF EXISTS `taxonomy_term_data`;
CREATE TABLE `taxonomy_term_data` (
  `tid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique term ID.',
  `vid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The taxonomy_vocabulary.vid of the vocabulary to which the term is assigned.',
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'The term name.',
  `description` longtext COMMENT 'A description of the term.',
  `format` varchar(255) DEFAULT NULL COMMENT 'The filter_format.format of the description.',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'The weight of this term in relation to other terms.',
  PRIMARY KEY (`tid`),
  KEY `taxonomy_tree` (`vid`,`weight`,`name`),
  KEY `vid_name` (`vid`,`name`),
  KEY `name` (`name`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores term information.';

--
-- Table structure for table `taxonomy_term_hierarchy`
--

DROP TABLE IF EXISTS `taxonomy_term_hierarchy`;
CREATE TABLE `taxonomy_term_hierarchy` (
  `tid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Primary Key: The taxonomy_term_data.tid of the term.',
  `parent` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Primary Key: The taxonomy_term_data.tid of the term’s parent. 0 indicates no parent.',
  PRIMARY KEY (`tid`,`parent`),
  KEY `parent` (`parent`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores the hierarchical relationship between terms.';

--
-- Table structure for table `taxonomy_tools_role_access`
--

DROP TABLE IF EXISTS `taxonomy_tools_role_access`;
CREATE TABLE `taxonomy_tools_role_access` (
  `entry_id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Table entry identifier.',
  `tid` int(10) unsigned NOT NULL COMMENT 'Taxonomy term identifier.',
  `rid` int(10) unsigned NOT NULL COMMENT 'User role identifier.',
  PRIMARY KEY (`entry_id`),
  KEY `taxonomy_tools_role_access_tid` (`tid`),
  KEY `taxonomy_tools_role_access_rid` (`rid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='The base table for Taxonomy Role Access records.';

--
-- Table structure for table `taxonomy_vocabulary`
--

DROP TABLE IF EXISTS `taxonomy_vocabulary`;
CREATE TABLE `taxonomy_vocabulary` (
  `vid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique vocabulary ID.',
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT 'Name of the vocabulary.',
  `machine_name` varchar(255) NOT NULL DEFAULT '' COMMENT 'The vocabulary machine name.',
  `description` longtext COMMENT 'Description of the vocabulary.',
  `hierarchy` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'The type of hierarchy allowed within the vocabulary. (0 = disabled, 1 = single, 2 = multiple)',
  `module` varchar(255) NOT NULL DEFAULT '' COMMENT 'The module which created the vocabulary.',
  `weight` int(11) NOT NULL DEFAULT '0' COMMENT 'The weight of this vocabulary in relation to other vocabularies.',
  PRIMARY KEY (`vid`),
  UNIQUE KEY `machine_name` (`machine_name`),
  KEY `list` (`weight`,`name`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores vocabulary information.';

--
-- Table structure for table `url_alias`
--

DROP TABLE IF EXISTS `url_alias`;
CREATE TABLE `url_alias` (
  `pid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'A unique path alias identifier.',
  `source` varchar(255) NOT NULL DEFAULT '' COMMENT 'The Drupal path this alias is for; e.g. node/12.',
  `alias` varchar(255) NOT NULL DEFAULT '' COMMENT 'The alias for this path; e.g. title-of-the-story.',
  `language` varchar(12) NOT NULL DEFAULT '' COMMENT 'The language this alias is for; if ’und’, the alias will be used for unknown languages. Each Drupal path can have an alias for each supported language.',
  PRIMARY KEY (`pid`),
  KEY `alias_language_pid` (`alias`,`language`,`pid`),
  KEY `source_language_pid` (`source`,`language`,`pid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='A list of URL aliases for Drupal paths; a user may visit...';

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
CREATE TABLE `users` (
  `uid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Primary Key: Unique user ID.',
  `name` varchar(60) NOT NULL DEFAULT '' COMMENT 'Unique user name.',
  `pass` varchar(128) NOT NULL DEFAULT '' COMMENT 'User’s password (hashed).',
  `mail` varchar(254) DEFAULT '' COMMENT 'User’s e-mail address.',
  `theme` varchar(255) NOT NULL DEFAULT '' COMMENT 'User’s default theme.',
  `signature` varchar(255) NOT NULL DEFAULT '' COMMENT 'User’s signature.',
  `signature_format` varchar(255) DEFAULT NULL COMMENT 'The filter_format.format of the signature.',
  `created` int(11) NOT NULL DEFAULT '0' COMMENT 'Timestamp for when user was created.',
  `access` int(11) NOT NULL DEFAULT '0' COMMENT 'Timestamp for previous time user accessed the site.',
  `login` int(11) NOT NULL DEFAULT '0' COMMENT 'Timestamp for user’s last login.',
  `status` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'Whether the user is active(1) or blocked(0).',
  `timezone` varchar(32) DEFAULT NULL COMMENT 'User’s time zone.',
  `language` varchar(12) NOT NULL DEFAULT '' COMMENT 'User’s default language.',
  `picture` int(11) NOT NULL DEFAULT '0' COMMENT 'Foreign key: file_managed.fid of user’s picture.',
  `init` varchar(254) DEFAULT '' COMMENT 'E-mail address used for initial account creation.',
  `data` longblob COMMENT 'A serialized array of name value pairs that are related to the user. Any form values posted during user edit are stored and are loaded into the $user object during user_load(). Use of this field is discouraged and it will likely disappear in a future...',
  PRIMARY KEY (`uid`),
  UNIQUE KEY `name` (`name`),
  KEY `access` (`access`),
  KEY `created` (`created`),
  KEY `mail` (`mail`),
  KEY `picture` (`picture`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores user data.';

--
-- Table structure for table `users_roles`
--

DROP TABLE IF EXISTS `users_roles`;
CREATE TABLE `users_roles` (
  `uid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Primary Key: users.uid for user.',
  `rid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Primary Key: role.rid for role.',
  PRIMARY KEY (`uid`,`rid`),
  KEY `rid` (`rid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Maps users to roles.';

--
-- Table structure for table `variable`
--

DROP TABLE IF EXISTS `variable`;
CREATE TABLE `variable` (
  `name` varchar(128) NOT NULL DEFAULT '' COMMENT 'The name of the variable.',
  `value` longblob NOT NULL COMMENT 'The value of the variable.',
  PRIMARY KEY (`name`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Named variable/value pairs created by Drupal core or any...';

--
-- Table structure for table `views_display`
--

DROP TABLE IF EXISTS `views_display`;
CREATE TABLE `views_display` (
  `vid` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The view this display is attached to.',
  `id` varchar(64) NOT NULL DEFAULT '' COMMENT 'An identifier for this display; usually generated from the display_plugin, so should be something like page or page_1 or block_2, etc.',
  `display_title` varchar(64) NOT NULL DEFAULT '' COMMENT 'The title of the display, viewable by the administrator.',
  `display_plugin` varchar(64) NOT NULL DEFAULT '' COMMENT 'The type of the display. Usually page, block or embed, but is pluggable so may be other things.',
  `position` int(11) DEFAULT '0' COMMENT 'The order in which this display is loaded.',
  `display_options` longtext COMMENT 'A serialized array of options for this display; it contains options that are generally only pertinent to that display plugin type.',
  PRIMARY KEY (`vid`,`id`),
  KEY `vid` (`vid`,`position`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='Stores information about each display attached to a view.';

--
-- Table structure for table `views_view`
--

DROP TABLE IF EXISTS `views_view`;
CREATE TABLE `views_view` (
  `vid` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'The view ID of the field, defined by the database.',
  `name` varchar(128) NOT NULL DEFAULT '' COMMENT 'The unique name of the view. This is the primary field views are loaded from, and is used so that views may be internal and not necessarily in the database. May only be alphanumeric characters plus underscores.',
  `description` varchar(255) DEFAULT '' COMMENT 'A description of the view for the admin interface.',
  `tag` varchar(255) DEFAULT '' COMMENT 'A tag used to group/sort views in the admin interface',
  `base_table` varchar(64) NOT NULL DEFAULT '' COMMENT 'What table this view is based on, such as node, user, comment, or term.',
  `human_name` varchar(255) DEFAULT '' COMMENT 'A human readable name used to be displayed in the admin interface',
  `core` int(11) DEFAULT '0' COMMENT 'Stores the drupal core version of the view.',
  PRIMARY KEY (`vid`),
  UNIQUE KEY `name` (`name`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Stores the general data for a view.';

--
-- Table structure for table `watchdog`
--

DROP TABLE IF EXISTS `watchdog`;
CREATE TABLE `watchdog` (
  `wid` int(11) NOT NULL AUTO_INCREMENT COMMENT 'Primary Key: Unique watchdog event ID.',
  `uid` int(11) NOT NULL DEFAULT '0' COMMENT 'The users.uid of the user who triggered the event.',
  `type` varchar(64) NOT NULL DEFAULT '' COMMENT 'Type of log message, for example "user" or "page not found."',
  `message` longtext NOT NULL COMMENT 'Text of log message to be passed into the t() function.',
  `variables` longblob NOT NULL COMMENT 'Serialized array of variables that match the message string and that is passed into the t() function.',
  `severity` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'The severity level of the event; ranges from 0 (Emergency) to 7 (Debug)',
  `link` varchar(255) DEFAULT '' COMMENT 'Link to view the result of the event.',
  `location` text NOT NULL COMMENT 'URL of the origin of the event.',
  `referer` text COMMENT 'URL of referring page.',
  `hostname` varchar(128) NOT NULL DEFAULT '' COMMENT 'Hostname of the user who triggered the event.',
  `timestamp` int(11) NOT NULL DEFAULT '0' COMMENT 'Unix timestamp of when event occurred.',
  PRIMARY KEY (`wid`),
  KEY `type` (`type`),
  KEY `uid` (`uid`)
) ENGINE="FEDERATED" CONNECTION="dc"  DEFAULT CHARSET=utf8 COMMENT='Table that contains logs of all system events.';

--
-- Table structure for table `xmlsitemap`
--

DROP TABLE IF EXISTS `xmlsitemap`;
CREATE TABLE `xmlsitemap` (
  `id` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'Primary key with type; a unique id for the item.',
  `type` varchar(32) NOT NULL DEFAULT '' COMMENT 'Primary key with id; the type of item (e.g. node, user, etc.).',
  `subtype` varchar(128) NOT NULL DEFAULT '' COMMENT 'A sub-type identifier for the link (node type, menu name, term VID, etc.).',
  `loc` varchar(255) NOT NULL DEFAULT '' COMMENT 'The URL to the item relative to the Drupal path.',
  `language` varchar(12) NOT NULL DEFAULT '' COMMENT 'The languages.language of this link or an empty string if it is language-neutral.',
  `access` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'A boolean that represents if the item is viewable by the anonymous user. This field is useful to store the result of node_access() so we can retain changefreq and priority_override information.',
  `status` tinyint(4) NOT NULL DEFAULT '1' COMMENT 'An integer that represents if the item is included in the sitemap.',
  `status_override` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean that if TRUE means that the status field has been overridden from its default value.',
  `lastmod` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The UNIX timestamp of last modification of the item.',
  `priority` float DEFAULT NULL COMMENT 'The priority of this URL relative to other URLs on your site. Valid values range from 0.0 to 1.0.',
  `priority_override` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'A boolean that if TRUE means that the priority field has been overridden from its default value.',
  `changefreq` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The average time in seconds between changes of this item.',
  `changecount` int(10) unsigned NOT NULL DEFAULT '0' COMMENT 'The number of times this item has been changed. Used to help calculate the next changefreq value.',
  PRIMARY KEY (`id`,`type`),
  KEY `loc` (`loc`),
  KEY `access_status_loc` (`access`,`status`,`loc`),
  KEY `type_subtype` (`type`,`subtype`),
  KEY `language` (`language`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8 COMMENT='The base table for xmlsitemap links.';

--
-- Table structure for table `xmlsitemap_sitemap`
--

DROP TABLE IF EXISTS `xmlsitemap_sitemap`;
CREATE TABLE `xmlsitemap_sitemap` (
  `smid` varchar(64) NOT NULL COMMENT 'The sitemap ID (the hashed value of xmlsitemap.context.',
  `context` text NOT NULL COMMENT 'Serialized array with the sitemaps context',
  `updated` int(10) unsigned NOT NULL DEFAULT '0',
  `links` int(10) unsigned NOT NULL DEFAULT '0',
  `chunks` int(10) unsigned NOT NULL DEFAULT '0',
  `max_filesize` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`smid`)
) ENGINE="FEDERATED" CONNECTION="dc" DEFAULT CHARSET=utf8;


-- Dump completed on 2019-06-12  9:43:36
-- * Create view
-- Persistent doesn't allow to use table for a different database
-- We therefore we need to create a view in fa

use fa;
DROP TABLE IF EXISTS `dcx_accesslog`;
DROP VIEW IF EXISTS `dcx_accesslog`;
CREATE VIEW `dcx_accesslog` AS SELECT * FROM commerceX.`accesslog`;


DROP TABLE IF EXISTS `dcx_actions`;
DROP VIEW IF EXISTS `dcx_actions`;
CREATE VIEW `dcx_actions` AS SELECT * FROM commerceX.`actions`;


DROP TABLE IF EXISTS `dcx_advanced_help_index`;
DROP VIEW IF EXISTS `dcx_advanced_help_index`;
CREATE VIEW `dcx_advanced_help_index` AS SELECT * FROM commerceX.`advanced_help_index`;


DROP TABLE IF EXISTS `dcx_authcache_p13n_key_value`;
DROP VIEW IF EXISTS `dcx_authcache_p13n_key_value`;
CREATE VIEW `dcx_authcache_p13n_key_value` AS SELECT * FROM commerceX.`authcache_p13n_key_value`;


DROP TABLE IF EXISTS `dcx_authmap`;
DROP VIEW IF EXISTS `dcx_authmap`;
CREATE VIEW `dcx_authmap` AS SELECT * FROM commerceX.`authmap`;


DROP TABLE IF EXISTS `dcx_batch`;
DROP VIEW IF EXISTS `dcx_batch`;
CREATE VIEW `dcx_batch` AS SELECT * FROM commerceX.`batch`;


DROP TABLE IF EXISTS `dcx_block`;
DROP VIEW IF EXISTS `dcx_block`;
CREATE VIEW `dcx_block` AS SELECT * FROM commerceX.`block`;


DROP TABLE IF EXISTS `dcx_block_current_search`;
DROP VIEW IF EXISTS `dcx_block_current_search`;
CREATE VIEW `dcx_block_current_search` AS SELECT * FROM commerceX.`block_current_search`;


DROP TABLE IF EXISTS `dcx_block_custom`;
DROP VIEW IF EXISTS `dcx_block_custom`;
CREATE VIEW `dcx_block_custom` AS SELECT * FROM commerceX.`block_custom`;


DROP TABLE IF EXISTS `dcx_block_node_type`;
DROP VIEW IF EXISTS `dcx_block_node_type`;
CREATE VIEW `dcx_block_node_type` AS SELECT * FROM commerceX.`block_node_type`;


DROP TABLE IF EXISTS `dcx_block_role`;
DROP VIEW IF EXISTS `dcx_block_role`;
CREATE VIEW `dcx_block_role` AS SELECT * FROM commerceX.`block_role`;


DROP TABLE IF EXISTS `dcx_blocked_ips`;
DROP VIEW IF EXISTS `dcx_blocked_ips`;
CREATE VIEW `dcx_blocked_ips` AS SELECT * FROM commerceX.`blocked_ips`;


DROP TABLE IF EXISTS `dcx_cache`;
DROP VIEW IF EXISTS `dcx_cache`;
CREATE VIEW `dcx_cache` AS SELECT * FROM commerceX.`cache`;


DROP TABLE IF EXISTS `dcx_cache_admin_menu`;
DROP VIEW IF EXISTS `dcx_cache_admin_menu`;
CREATE VIEW `dcx_cache_admin_menu` AS SELECT * FROM commerceX.`cache_admin_menu`;


DROP TABLE IF EXISTS `dcx_cache_authcache_debug`;
DROP VIEW IF EXISTS `dcx_cache_authcache_debug`;
CREATE VIEW `dcx_cache_authcache_debug` AS SELECT * FROM commerceX.`cache_authcache_debug`;


DROP TABLE IF EXISTS `dcx_cache_authcache_key`;
DROP VIEW IF EXISTS `dcx_cache_authcache_key`;
CREATE VIEW `dcx_cache_authcache_key` AS SELECT * FROM commerceX.`cache_authcache_key`;


DROP TABLE IF EXISTS `dcx_cache_authcache_p13n`;
DROP VIEW IF EXISTS `dcx_cache_authcache_p13n`;
CREATE VIEW `dcx_cache_authcache_p13n` AS SELECT * FROM commerceX.`cache_authcache_p13n`;


DROP TABLE IF EXISTS `dcx_cache_block`;
DROP VIEW IF EXISTS `dcx_cache_block`;
CREATE VIEW `dcx_cache_block` AS SELECT * FROM commerceX.`cache_block`;


DROP TABLE IF EXISTS `dcx_cache_bootstrap`;
DROP VIEW IF EXISTS `dcx_cache_bootstrap`;
CREATE VIEW `dcx_cache_bootstrap` AS SELECT * FROM commerceX.`cache_bootstrap`;


DROP TABLE IF EXISTS `dcx_cache_commerce_shipping_rates`;
DROP VIEW IF EXISTS `dcx_cache_commerce_shipping_rates`;
CREATE VIEW `dcx_cache_commerce_shipping_rates` AS SELECT * FROM commerceX.`cache_commerce_shipping_rates`;


DROP TABLE IF EXISTS `dcx_cache_display_cache`;
DROP VIEW IF EXISTS `dcx_cache_display_cache`;
CREATE VIEW `dcx_cache_display_cache` AS SELECT * FROM commerceX.`cache_display_cache`;


DROP TABLE IF EXISTS `dcx_cache_entity_comment`;
DROP VIEW IF EXISTS `dcx_cache_entity_comment`;
CREATE VIEW `dcx_cache_entity_comment` AS SELECT * FROM commerceX.`cache_entity_comment`;


DROP TABLE IF EXISTS `dcx_cache_entity_file`;
DROP VIEW IF EXISTS `dcx_cache_entity_file`;
CREATE VIEW `dcx_cache_entity_file` AS SELECT * FROM commerceX.`cache_entity_file`;


DROP TABLE IF EXISTS `dcx_cache_entity_message`;
DROP VIEW IF EXISTS `dcx_cache_entity_message`;
CREATE VIEW `dcx_cache_entity_message` AS SELECT * FROM commerceX.`cache_entity_message`;


DROP TABLE IF EXISTS `dcx_cache_entity_message_type`;
DROP VIEW IF EXISTS `dcx_cache_entity_message_type`;
CREATE VIEW `dcx_cache_entity_message_type` AS SELECT * FROM commerceX.`cache_entity_message_type`;


DROP TABLE IF EXISTS `dcx_cache_entity_message_type_category`;
DROP VIEW IF EXISTS `dcx_cache_entity_message_type_category`;
CREATE VIEW `dcx_cache_entity_message_type_category` AS SELECT * FROM commerceX.`cache_entity_message_type_category`;


DROP TABLE IF EXISTS `dcx_cache_entity_node`;
DROP VIEW IF EXISTS `dcx_cache_entity_node`;
CREATE VIEW `dcx_cache_entity_node` AS SELECT * FROM commerceX.`cache_entity_node`;


DROP TABLE IF EXISTS `dcx_cache_entity_taxonomy_term`;
DROP VIEW IF EXISTS `dcx_cache_entity_taxonomy_term`;
CREATE VIEW `dcx_cache_entity_taxonomy_term` AS SELECT * FROM commerceX.`cache_entity_taxonomy_term`;


DROP TABLE IF EXISTS `dcx_cache_entity_taxonomy_vocabulary`;
DROP VIEW IF EXISTS `dcx_cache_entity_taxonomy_vocabulary`;
CREATE VIEW `dcx_cache_entity_taxonomy_vocabulary` AS SELECT * FROM commerceX.`cache_entity_taxonomy_vocabulary`;


DROP TABLE IF EXISTS `dcx_cache_entity_user`;
DROP VIEW IF EXISTS `dcx_cache_entity_user`;
CREATE VIEW `dcx_cache_entity_user` AS SELECT * FROM commerceX.`cache_entity_user`;


DROP TABLE IF EXISTS `dcx_cache_field`;
DROP VIEW IF EXISTS `dcx_cache_field`;
CREATE VIEW `dcx_cache_field` AS SELECT * FROM commerceX.`cache_field`;


DROP TABLE IF EXISTS `dcx_cache_filter`;
DROP VIEW IF EXISTS `dcx_cache_filter`;
CREATE VIEW `dcx_cache_filter` AS SELECT * FROM commerceX.`cache_filter`;


DROP TABLE IF EXISTS `dcx_cache_form`;
DROP VIEW IF EXISTS `dcx_cache_form`;
CREATE VIEW `dcx_cache_form` AS SELECT * FROM commerceX.`cache_form`;


DROP TABLE IF EXISTS `dcx_cache_image`;
DROP VIEW IF EXISTS `dcx_cache_image`;
CREATE VIEW `dcx_cache_image` AS SELECT * FROM commerceX.`cache_image`;


DROP TABLE IF EXISTS `dcx_cache_libraries`;
DROP VIEW IF EXISTS `dcx_cache_libraries`;
CREATE VIEW `dcx_cache_libraries` AS SELECT * FROM commerceX.`cache_libraries`;


DROP TABLE IF EXISTS `dcx_cache_menu`;
DROP VIEW IF EXISTS `dcx_cache_menu`;
CREATE VIEW `dcx_cache_menu` AS SELECT * FROM commerceX.`cache_menu`;


DROP TABLE IF EXISTS `dcx_cache_metatag`;
DROP VIEW IF EXISTS `dcx_cache_metatag`;
CREATE VIEW `dcx_cache_metatag` AS SELECT * FROM commerceX.`cache_metatag`;


DROP TABLE IF EXISTS `dcx_cache_page`;
DROP VIEW IF EXISTS `dcx_cache_page`;
CREATE VIEW `dcx_cache_page` AS SELECT * FROM commerceX.`cache_page`;


DROP TABLE IF EXISTS `dcx_cache_path`;
DROP VIEW IF EXISTS `dcx_cache_path`;
CREATE VIEW `dcx_cache_path` AS SELECT * FROM commerceX.`cache_path`;


DROP TABLE IF EXISTS `dcx_cache_path_alias`;
DROP VIEW IF EXISTS `dcx_cache_path_alias`;
CREATE VIEW `dcx_cache_path_alias` AS SELECT * FROM commerceX.`cache_path_alias`;


DROP TABLE IF EXISTS `dcx_cache_path_source`;
DROP VIEW IF EXISTS `dcx_cache_path_source`;
CREATE VIEW `dcx_cache_path_source` AS SELECT * FROM commerceX.`cache_path_source`;


DROP TABLE IF EXISTS `dcx_cache_rules`;
DROP VIEW IF EXISTS `dcx_cache_rules`;
CREATE VIEW `dcx_cache_rules` AS SELECT * FROM commerceX.`cache_rules`;


DROP TABLE IF EXISTS `dcx_cache_token`;
DROP VIEW IF EXISTS `dcx_cache_token`;
CREATE VIEW `dcx_cache_token` AS SELECT * FROM commerceX.`cache_token`;


DROP TABLE IF EXISTS `dcx_cache_update`;
DROP VIEW IF EXISTS `dcx_cache_update`;
CREATE VIEW `dcx_cache_update` AS SELECT * FROM commerceX.`cache_update`;


DROP TABLE IF EXISTS `dcx_cache_views`;
DROP VIEW IF EXISTS `dcx_cache_views`;
CREATE VIEW `dcx_cache_views` AS SELECT * FROM commerceX.`cache_views`;


DROP TABLE IF EXISTS `dcx_cache_views_data`;
DROP VIEW IF EXISTS `dcx_cache_views_data`;
CREATE VIEW `dcx_cache_views_data` AS SELECT * FROM commerceX.`cache_views_data`;


DROP TABLE IF EXISTS `dcx_cmp_menu_perms`;
DROP VIEW IF EXISTS `dcx_cmp_menu_perms`;
CREATE VIEW `dcx_cmp_menu_perms` AS SELECT * FROM commerceX.`cmp_menu_perms`;


DROP TABLE IF EXISTS `dcx_cmp_permissions`;
DROP VIEW IF EXISTS `dcx_cmp_permissions`;
CREATE VIEW `dcx_cmp_permissions` AS SELECT * FROM commerceX.`cmp_permissions`;


DROP TABLE IF EXISTS `dcx_comment`;
DROP VIEW IF EXISTS `dcx_comment`;
CREATE VIEW `dcx_comment` AS SELECT * FROM commerceX.`comment`;


DROP TABLE IF EXISTS `dcx_commerce_addressbook_defaults`;
DROP VIEW IF EXISTS `dcx_commerce_addressbook_defaults`;
CREATE VIEW `dcx_commerce_addressbook_defaults` AS SELECT * FROM commerceX.`commerce_addressbook_defaults`;


DROP TABLE IF EXISTS `dcx_commerce_autosku_patterns`;
DROP VIEW IF EXISTS `dcx_commerce_autosku_patterns`;
CREATE VIEW `dcx_commerce_autosku_patterns` AS SELECT * FROM commerceX.`commerce_autosku_patterns`;


DROP TABLE IF EXISTS `dcx_commerce_calculated_price`;
DROP VIEW IF EXISTS `dcx_commerce_calculated_price`;
CREATE VIEW `dcx_commerce_calculated_price` AS SELECT * FROM commerceX.`commerce_calculated_price`;


DROP TABLE IF EXISTS `dcx_commerce_checkout_pane`;
DROP VIEW IF EXISTS `dcx_commerce_checkout_pane`;
CREATE VIEW `dcx_commerce_checkout_pane` AS SELECT * FROM commerceX.`commerce_checkout_pane`;


DROP TABLE IF EXISTS `dcx_commerce_customer_profile`;
DROP VIEW IF EXISTS `dcx_commerce_customer_profile`;
CREATE VIEW `dcx_commerce_customer_profile` AS SELECT * FROM commerceX.`commerce_customer_profile`;


DROP TABLE IF EXISTS `dcx_commerce_customer_profile_revision`;
DROP VIEW IF EXISTS `dcx_commerce_customer_profile_revision`;
CREATE VIEW `dcx_commerce_customer_profile_revision` AS SELECT * FROM commerceX.`commerce_customer_profile_revision`;


DROP TABLE IF EXISTS `dcx_commerce_discount`;
DROP VIEW IF EXISTS `dcx_commerce_discount`;
CREATE VIEW `dcx_commerce_discount` AS SELECT * FROM commerceX.`commerce_discount`;


DROP TABLE IF EXISTS `dcx_commerce_discount_offer`;
DROP VIEW IF EXISTS `dcx_commerce_discount_offer`;
CREATE VIEW `dcx_commerce_discount_offer` AS SELECT * FROM commerceX.`commerce_discount_offer`;


DROP TABLE IF EXISTS `dcx_commerce_flat_rate_service`;
DROP VIEW IF EXISTS `dcx_commerce_flat_rate_service`;
CREATE VIEW `dcx_commerce_flat_rate_service` AS SELECT * FROM commerceX.`commerce_flat_rate_service`;


DROP TABLE IF EXISTS `dcx_commerce_line_item`;
DROP VIEW IF EXISTS `dcx_commerce_line_item`;
CREATE VIEW `dcx_commerce_line_item` AS SELECT * FROM commerceX.`commerce_line_item`;


DROP TABLE IF EXISTS `dcx_commerce_order`;
DROP VIEW IF EXISTS `dcx_commerce_order`;
CREATE VIEW `dcx_commerce_order` AS SELECT * FROM commerceX.`commerce_order`;


DROP TABLE IF EXISTS `dcx_commerce_order_revision`;
DROP VIEW IF EXISTS `dcx_commerce_order_revision`;
CREATE VIEW `dcx_commerce_order_revision` AS SELECT * FROM commerceX.`commerce_order_revision`;


DROP TABLE IF EXISTS `dcx_commerce_payment_transaction`;
DROP VIEW IF EXISTS `dcx_commerce_payment_transaction`;
CREATE VIEW `dcx_commerce_payment_transaction` AS SELECT * FROM commerceX.`commerce_payment_transaction`;


DROP TABLE IF EXISTS `dcx_commerce_payment_transaction_revision`;
DROP VIEW IF EXISTS `dcx_commerce_payment_transaction_revision`;
CREATE VIEW `dcx_commerce_payment_transaction_revision` AS SELECT * FROM commerceX.`commerce_payment_transaction_revision`;


DROP TABLE IF EXISTS `dcx_commerce_product`;
DROP VIEW IF EXISTS `dcx_commerce_product`;
CREATE VIEW `dcx_commerce_product` AS SELECT * FROM commerceX.`commerce_product`;


DROP TABLE IF EXISTS `dcx_commerce_product_revision`;
DROP VIEW IF EXISTS `dcx_commerce_product_revision`;
CREATE VIEW `dcx_commerce_product_revision` AS SELECT * FROM commerceX.`commerce_product_revision`;


DROP TABLE IF EXISTS `dcx_commerce_product_type`;
DROP VIEW IF EXISTS `dcx_commerce_product_type`;
CREATE VIEW `dcx_commerce_product_type` AS SELECT * FROM commerceX.`commerce_product_type`;


DROP TABLE IF EXISTS `dcx_commerce_tax_rate`;
DROP VIEW IF EXISTS `dcx_commerce_tax_rate`;
CREATE VIEW `dcx_commerce_tax_rate` AS SELECT * FROM commerceX.`commerce_tax_rate`;


DROP TABLE IF EXISTS `dcx_commerce_tax_type`;
DROP VIEW IF EXISTS `dcx_commerce_tax_type`;
CREATE VIEW `dcx_commerce_tax_type` AS SELECT * FROM commerceX.`commerce_tax_type`;


DROP TABLE IF EXISTS `dcx_contact`;
DROP VIEW IF EXISTS `dcx_contact`;
CREATE VIEW `dcx_contact` AS SELECT * FROM commerceX.`contact`;


DROP TABLE IF EXISTS `dcx_ctools_css_cache`;
DROP VIEW IF EXISTS `dcx_ctools_css_cache`;
CREATE VIEW `dcx_ctools_css_cache` AS SELECT * FROM commerceX.`ctools_css_cache`;


DROP TABLE IF EXISTS `dcx_ctools_object_cache`;
DROP VIEW IF EXISTS `dcx_ctools_object_cache`;
CREATE VIEW `dcx_ctools_object_cache` AS SELECT * FROM commerceX.`ctools_object_cache`;


DROP TABLE IF EXISTS `dcx_current_search`;
DROP VIEW IF EXISTS `dcx_current_search`;
CREATE VIEW `dcx_current_search` AS SELECT * FROM commerceX.`current_search`;


DROP TABLE IF EXISTS `dcx_date_format_locale`;
DROP VIEW IF EXISTS `dcx_date_format_locale`;
CREATE VIEW `dcx_date_format_locale` AS SELECT * FROM commerceX.`date_format_locale`;


DROP TABLE IF EXISTS `dcx_date_format_type`;
DROP VIEW IF EXISTS `dcx_date_format_type`;
CREATE VIEW `dcx_date_format_type` AS SELECT * FROM commerceX.`date_format_type`;


DROP TABLE IF EXISTS `dcx_date_formats`;
DROP VIEW IF EXISTS `dcx_date_formats`;
CREATE VIEW `dcx_date_formats` AS SELECT * FROM commerceX.`date_formats`;


DROP TABLE IF EXISTS `dcx_facetapi`;
DROP VIEW IF EXISTS `dcx_facetapi`;
CREATE VIEW `dcx_facetapi` AS SELECT * FROM commerceX.`facetapi`;


DROP TABLE IF EXISTS `dcx_feeds_importer`;
DROP VIEW IF EXISTS `dcx_feeds_importer`;
CREATE VIEW `dcx_feeds_importer` AS SELECT * FROM commerceX.`feeds_importer`;


DROP TABLE IF EXISTS `dcx_feeds_item`;
DROP VIEW IF EXISTS `dcx_feeds_item`;
CREATE VIEW `dcx_feeds_item` AS SELECT * FROM commerceX.`feeds_item`;


DROP TABLE IF EXISTS `dcx_feeds_log`;
DROP VIEW IF EXISTS `dcx_feeds_log`;
CREATE VIEW `dcx_feeds_log` AS SELECT * FROM commerceX.`feeds_log`;


DROP TABLE IF EXISTS `dcx_feeds_push_subscriptions`;
DROP VIEW IF EXISTS `dcx_feeds_push_subscriptions`;
CREATE VIEW `dcx_feeds_push_subscriptions` AS SELECT * FROM commerceX.`feeds_push_subscriptions`;


DROP TABLE IF EXISTS `dcx_feeds_selfnode_processor_item`;
DROP VIEW IF EXISTS `dcx_feeds_selfnode_processor_item`;
CREATE VIEW `dcx_feeds_selfnode_processor_item` AS SELECT * FROM commerceX.`feeds_selfnode_processor_item`;


DROP TABLE IF EXISTS `dcx_feeds_source`;
DROP VIEW IF EXISTS `dcx_feeds_source`;
CREATE VIEW `dcx_feeds_source` AS SELECT * FROM commerceX.`feeds_source`;


DROP TABLE IF EXISTS `dcx_feeds_tamper`;
DROP VIEW IF EXISTS `dcx_feeds_tamper`;
CREATE VIEW `dcx_feeds_tamper` AS SELECT * FROM commerceX.`feeds_tamper`;


DROP TABLE IF EXISTS `dcx_field_config`;
DROP VIEW IF EXISTS `dcx_field_config`;
CREATE VIEW `dcx_field_config` AS SELECT * FROM commerceX.`field_config`;


DROP TABLE IF EXISTS `dcx_field_config_instance`;
DROP VIEW IF EXISTS `dcx_field_config_instance`;
CREATE VIEW `dcx_field_config_instance` AS SELECT * FROM commerceX.`field_config_instance`;


DROP TABLE IF EXISTS `dcx_field_data_body`;
DROP VIEW IF EXISTS `dcx_field_data_body`;
CREATE VIEW `dcx_field_data_body` AS SELECT * FROM commerceX.`field_data_body`;


DROP TABLE IF EXISTS `dcx_field_data_comment_body`;
DROP VIEW IF EXISTS `dcx_field_data_comment_body`;
CREATE VIEW `dcx_field_data_comment_body` AS SELECT * FROM commerceX.`field_data_comment_body`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_customer_address`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_customer_address`;
CREATE VIEW `dcx_field_data_commerce_customer_address` AS SELECT * FROM commerceX.`field_data_commerce_customer_address`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_customer_billing`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_customer_billing`;
CREATE VIEW `dcx_field_data_commerce_customer_billing` AS SELECT * FROM commerceX.`field_data_commerce_customer_billing`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_customer_shipping`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_customer_shipping`;
CREATE VIEW `dcx_field_data_commerce_customer_shipping` AS SELECT * FROM commerceX.`field_data_commerce_customer_shipping`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_discount_date`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_discount_date`;
CREATE VIEW `dcx_field_data_commerce_discount_date` AS SELECT * FROM commerceX.`field_data_commerce_discount_date`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_discount_offer`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_discount_offer`;
CREATE VIEW `dcx_field_data_commerce_discount_offer` AS SELECT * FROM commerceX.`field_data_commerce_discount_offer`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_discounts`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_discounts`;
CREATE VIEW `dcx_field_data_commerce_discounts` AS SELECT * FROM commerceX.`field_data_commerce_discounts`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_display_path`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_display_path`;
CREATE VIEW `dcx_field_data_commerce_display_path` AS SELECT * FROM commerceX.`field_data_commerce_display_path`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_fixed_amount`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_fixed_amount`;
CREATE VIEW `dcx_field_data_commerce_fixed_amount` AS SELECT * FROM commerceX.`field_data_commerce_fixed_amount`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_free_products`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_free_products`;
CREATE VIEW `dcx_field_data_commerce_free_products` AS SELECT * FROM commerceX.`field_data_commerce_free_products`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_free_shipping`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_free_shipping`;
CREATE VIEW `dcx_field_data_commerce_free_shipping` AS SELECT * FROM commerceX.`field_data_commerce_free_shipping`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_line_items`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_line_items`;
CREATE VIEW `dcx_field_data_commerce_line_items` AS SELECT * FROM commerceX.`field_data_commerce_line_items`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_order_total`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_order_total`;
CREATE VIEW `dcx_field_data_commerce_order_total` AS SELECT * FROM commerceX.`field_data_commerce_order_total`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_percentage`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_percentage`;
CREATE VIEW `dcx_field_data_commerce_percentage` AS SELECT * FROM commerceX.`field_data_commerce_percentage`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_price`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_price`;
CREATE VIEW `dcx_field_data_commerce_price` AS SELECT * FROM commerceX.`field_data_commerce_price`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_product`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_product`;
CREATE VIEW `dcx_field_data_commerce_product` AS SELECT * FROM commerceX.`field_data_commerce_product`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_shipping_service`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_shipping_service`;
CREATE VIEW `dcx_field_data_commerce_shipping_service` AS SELECT * FROM commerceX.`field_data_commerce_shipping_service`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_total`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_total`;
CREATE VIEW `dcx_field_data_commerce_total` AS SELECT * FROM commerceX.`field_data_commerce_total`;


DROP TABLE IF EXISTS `dcx_field_data_commerce_unit_price`;
DROP VIEW IF EXISTS `dcx_field_data_commerce_unit_price`;
CREATE VIEW `dcx_field_data_commerce_unit_price` AS SELECT * FROM commerceX.`field_data_commerce_unit_price`;


DROP TABLE IF EXISTS `dcx_field_data_field_availability_date`;
DROP VIEW IF EXISTS `dcx_field_data_field_availability_date`;
CREATE VIEW `dcx_field_data_field_availability_date` AS SELECT * FROM commerceX.`field_data_field_availability_date`;


DROP TABLE IF EXISTS `dcx_field_data_field_availability_period`;
DROP VIEW IF EXISTS `dcx_field_data_field_availability_period`;
CREATE VIEW `dcx_field_data_field_availability_period` AS SELECT * FROM commerceX.`field_data_field_availability_period`;


DROP TABLE IF EXISTS `dcx_field_data_field_brand`;
DROP VIEW IF EXISTS `dcx_field_data_field_brand`;
CREATE VIEW `dcx_field_data_field_brand` AS SELECT * FROM commerceX.`field_data_field_brand`;


DROP TABLE IF EXISTS `dcx_field_data_field_collection`;
DROP VIEW IF EXISTS `dcx_field_data_field_collection`;
CREATE VIEW `dcx_field_data_field_collection` AS SELECT * FROM commerceX.`field_data_field_collection`;


DROP TABLE IF EXISTS `dcx_field_data_field_colour`;
DROP VIEW IF EXISTS `dcx_field_data_field_colour`;
CREATE VIEW `dcx_field_data_field_colour` AS SELECT * FROM commerceX.`field_data_field_colour`;


DROP TABLE IF EXISTS `dcx_field_data_field_colour_code`;
DROP VIEW IF EXISTS `dcx_field_data_field_colour_code`;
CREATE VIEW `dcx_field_data_field_colour_code` AS SELECT * FROM commerceX.`field_data_field_colour_code`;


DROP TABLE IF EXISTS `dcx_field_data_field_headline`;
DROP VIEW IF EXISTS `dcx_field_data_field_headline`;
CREATE VIEW `dcx_field_data_field_headline` AS SELECT * FROM commerceX.`field_data_field_headline`;


DROP TABLE IF EXISTS `dcx_field_data_field_image`;
DROP VIEW IF EXISTS `dcx_field_data_field_image`;
CREATE VIEW `dcx_field_data_field_image` AS SELECT * FROM commerceX.`field_data_field_image`;


DROP TABLE IF EXISTS `dcx_field_data_field_images`;
DROP VIEW IF EXISTS `dcx_field_data_field_images`;
CREATE VIEW `dcx_field_data_field_images` AS SELECT * FROM commerceX.`field_data_field_images`;


DROP TABLE IF EXISTS `dcx_field_data_field_link`;
DROP VIEW IF EXISTS `dcx_field_data_field_link`;
CREATE VIEW `dcx_field_data_field_link` AS SELECT * FROM commerceX.`field_data_field_link`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_01`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_01`;
CREATE VIEW `dcx_field_data_field_price_pl_01` AS SELECT * FROM commerceX.`field_data_field_price_pl_01`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_02`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_02`;
CREATE VIEW `dcx_field_data_field_price_pl_02` AS SELECT * FROM commerceX.`field_data_field_price_pl_02`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_03`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_03`;
CREATE VIEW `dcx_field_data_field_price_pl_03` AS SELECT * FROM commerceX.`field_data_field_price_pl_03`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_04`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_04`;
CREATE VIEW `dcx_field_data_field_price_pl_04` AS SELECT * FROM commerceX.`field_data_field_price_pl_04`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_05`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_05`;
CREATE VIEW `dcx_field_data_field_price_pl_05` AS SELECT * FROM commerceX.`field_data_field_price_pl_05`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_06`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_06`;
CREATE VIEW `dcx_field_data_field_price_pl_06` AS SELECT * FROM commerceX.`field_data_field_price_pl_06`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_07`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_07`;
CREATE VIEW `dcx_field_data_field_price_pl_07` AS SELECT * FROM commerceX.`field_data_field_price_pl_07`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_08`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_08`;
CREATE VIEW `dcx_field_data_field_price_pl_08` AS SELECT * FROM commerceX.`field_data_field_price_pl_08`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_09`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_09`;
CREATE VIEW `dcx_field_data_field_price_pl_09` AS SELECT * FROM commerceX.`field_data_field_price_pl_09`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_10`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_10`;
CREATE VIEW `dcx_field_data_field_price_pl_10` AS SELECT * FROM commerceX.`field_data_field_price_pl_10`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_11`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_11`;
CREATE VIEW `dcx_field_data_field_price_pl_11` AS SELECT * FROM commerceX.`field_data_field_price_pl_11`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_12`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_12`;
CREATE VIEW `dcx_field_data_field_price_pl_12` AS SELECT * FROM commerceX.`field_data_field_price_pl_12`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_13`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_13`;
CREATE VIEW `dcx_field_data_field_price_pl_13` AS SELECT * FROM commerceX.`field_data_field_price_pl_13`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_14`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_14`;
CREATE VIEW `dcx_field_data_field_price_pl_14` AS SELECT * FROM commerceX.`field_data_field_price_pl_14`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_15`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_15`;
CREATE VIEW `dcx_field_data_field_price_pl_15` AS SELECT * FROM commerceX.`field_data_field_price_pl_15`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_16`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_16`;
CREATE VIEW `dcx_field_data_field_price_pl_16` AS SELECT * FROM commerceX.`field_data_field_price_pl_16`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_17`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_17`;
CREATE VIEW `dcx_field_data_field_price_pl_17` AS SELECT * FROM commerceX.`field_data_field_price_pl_17`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_18`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_18`;
CREATE VIEW `dcx_field_data_field_price_pl_18` AS SELECT * FROM commerceX.`field_data_field_price_pl_18`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_19`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_19`;
CREATE VIEW `dcx_field_data_field_price_pl_19` AS SELECT * FROM commerceX.`field_data_field_price_pl_19`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_20`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_20`;
CREATE VIEW `dcx_field_data_field_price_pl_20` AS SELECT * FROM commerceX.`field_data_field_price_pl_20`;


DROP TABLE IF EXISTS `dcx_field_data_field_price_pl_21`;
DROP VIEW IF EXISTS `dcx_field_data_field_price_pl_21`;
CREATE VIEW `dcx_field_data_field_price_pl_21` AS SELECT * FROM commerceX.`field_data_field_price_pl_21`;


DROP TABLE IF EXISTS `dcx_field_data_field_product`;
DROP VIEW IF EXISTS `dcx_field_data_field_product`;
CREATE VIEW `dcx_field_data_field_product` AS SELECT * FROM commerceX.`field_data_field_product`;


DROP TABLE IF EXISTS `dcx_field_data_field_product_category`;
DROP VIEW IF EXISTS `dcx_field_data_field_product_category`;
CREATE VIEW `dcx_field_data_field_product_category` AS SELECT * FROM commerceX.`field_data_field_product_category`;


DROP TABLE IF EXISTS `dcx_field_data_field_required_delivery_date`;
DROP VIEW IF EXISTS `dcx_field_data_field_required_delivery_date`;
CREATE VIEW `dcx_field_data_field_required_delivery_date` AS SELECT * FROM commerceX.`field_data_field_required_delivery_date`;


DROP TABLE IF EXISTS `dcx_field_data_field_rgb`;
DROP VIEW IF EXISTS `dcx_field_data_field_rgb`;
CREATE VIEW `dcx_field_data_field_rgb` AS SELECT * FROM commerceX.`field_data_field_rgb`;


DROP TABLE IF EXISTS `dcx_field_data_field_special_request`;
DROP VIEW IF EXISTS `dcx_field_data_field_special_request`;
CREATE VIEW `dcx_field_data_field_special_request` AS SELECT * FROM commerceX.`field_data_field_special_request`;


DROP TABLE IF EXISTS `dcx_field_data_field_stock_status`;
DROP VIEW IF EXISTS `dcx_field_data_field_stock_status`;
CREATE VIEW `dcx_field_data_field_stock_status` AS SELECT * FROM commerceX.`field_data_field_stock_status`;


DROP TABLE IF EXISTS `dcx_field_data_field_table`;
DROP VIEW IF EXISTS `dcx_field_data_field_table`;
CREATE VIEW `dcx_field_data_field_table` AS SELECT * FROM commerceX.`field_data_field_table`;


DROP TABLE IF EXISTS `dcx_field_data_field_table_`;
DROP VIEW IF EXISTS `dcx_field_data_field_table_`;
CREATE VIEW `dcx_field_data_field_table_` AS SELECT * FROM commerceX.`field_data_field_table_`;


DROP TABLE IF EXISTS `dcx_field_data_field_tagline`;
DROP VIEW IF EXISTS `dcx_field_data_field_tagline`;
CREATE VIEW `dcx_field_data_field_tagline` AS SELECT * FROM commerceX.`field_data_field_tagline`;


DROP TABLE IF EXISTS `dcx_field_data_field_trim_colour`;
DROP VIEW IF EXISTS `dcx_field_data_field_trim_colour`;
CREATE VIEW `dcx_field_data_field_trim_colour` AS SELECT * FROM commerceX.`field_data_field_trim_colour`;


DROP TABLE IF EXISTS `dcx_field_data_field_wholesale_price`;
DROP VIEW IF EXISTS `dcx_field_data_field_wholesale_price`;
CREATE VIEW `dcx_field_data_field_wholesale_price` AS SELECT * FROM commerceX.`field_data_field_wholesale_price`;


DROP TABLE IF EXISTS `dcx_field_data_inline_conditions`;
DROP VIEW IF EXISTS `dcx_field_data_inline_conditions`;
CREATE VIEW `dcx_field_data_inline_conditions` AS SELECT * FROM commerceX.`field_data_inline_conditions`;


DROP TABLE IF EXISTS `dcx_field_data_message_commerce_body`;
DROP VIEW IF EXISTS `dcx_field_data_message_commerce_body`;
CREATE VIEW `dcx_field_data_message_commerce_body` AS SELECT * FROM commerceX.`field_data_message_commerce_body`;


DROP TABLE IF EXISTS `dcx_field_data_message_commerce_line_item`;
DROP VIEW IF EXISTS `dcx_field_data_message_commerce_line_item`;
CREATE VIEW `dcx_field_data_message_commerce_line_item` AS SELECT * FROM commerceX.`field_data_message_commerce_line_item`;


DROP TABLE IF EXISTS `dcx_field_data_message_commerce_order`;
DROP VIEW IF EXISTS `dcx_field_data_message_commerce_order`;
CREATE VIEW `dcx_field_data_message_commerce_order` AS SELECT * FROM commerceX.`field_data_message_commerce_order`;


DROP TABLE IF EXISTS `dcx_field_data_message_commerce_payment`;
DROP VIEW IF EXISTS `dcx_field_data_message_commerce_payment`;
CREATE VIEW `dcx_field_data_message_commerce_payment` AS SELECT * FROM commerceX.`field_data_message_commerce_payment`;


DROP TABLE IF EXISTS `dcx_field_data_message_order_display_name`;
DROP VIEW IF EXISTS `dcx_field_data_message_order_display_name`;
CREATE VIEW `dcx_field_data_message_order_display_name` AS SELECT * FROM commerceX.`field_data_message_order_display_name`;


DROP TABLE IF EXISTS `dcx_field_data_message_text`;
DROP VIEW IF EXISTS `dcx_field_data_message_text`;
CREATE VIEW `dcx_field_data_message_text` AS SELECT * FROM commerceX.`field_data_message_text`;


DROP TABLE IF EXISTS `dcx_field_data_message_text_subject`;
DROP VIEW IF EXISTS `dcx_field_data_message_text_subject`;
CREATE VIEW `dcx_field_data_message_text_subject` AS SELECT * FROM commerceX.`field_data_message_text_subject`;


DROP TABLE IF EXISTS `dcx_field_data_title_field`;
DROP VIEW IF EXISTS `dcx_field_data_title_field`;
CREATE VIEW `dcx_field_data_title_field` AS SELECT * FROM commerceX.`field_data_title_field`;


DROP TABLE IF EXISTS `dcx_field_deleted_data_11`;
DROP VIEW IF EXISTS `dcx_field_deleted_data_11`;
CREATE VIEW `dcx_field_deleted_data_11` AS SELECT * FROM commerceX.`field_deleted_data_11`;


DROP TABLE IF EXISTS `dcx_field_deleted_data_22`;
DROP VIEW IF EXISTS `dcx_field_deleted_data_22`;
CREATE VIEW `dcx_field_deleted_data_22` AS SELECT * FROM commerceX.`field_deleted_data_22`;


DROP TABLE IF EXISTS `dcx_field_deleted_data_23`;
DROP VIEW IF EXISTS `dcx_field_deleted_data_23`;
CREATE VIEW `dcx_field_deleted_data_23` AS SELECT * FROM commerceX.`field_deleted_data_23`;


DROP TABLE IF EXISTS `dcx_field_deleted_data_24`;
DROP VIEW IF EXISTS `dcx_field_deleted_data_24`;
CREATE VIEW `dcx_field_deleted_data_24` AS SELECT * FROM commerceX.`field_deleted_data_24`;


DROP TABLE IF EXISTS `dcx_field_deleted_data_25`;
DROP VIEW IF EXISTS `dcx_field_deleted_data_25`;
CREATE VIEW `dcx_field_deleted_data_25` AS SELECT * FROM commerceX.`field_deleted_data_25`;


DROP TABLE IF EXISTS `dcx_field_deleted_revision_11`;
DROP VIEW IF EXISTS `dcx_field_deleted_revision_11`;
CREATE VIEW `dcx_field_deleted_revision_11` AS SELECT * FROM commerceX.`field_deleted_revision_11`;


DROP TABLE IF EXISTS `dcx_field_deleted_revision_22`;
DROP VIEW IF EXISTS `dcx_field_deleted_revision_22`;
CREATE VIEW `dcx_field_deleted_revision_22` AS SELECT * FROM commerceX.`field_deleted_revision_22`;


DROP TABLE IF EXISTS `dcx_field_deleted_revision_23`;
DROP VIEW IF EXISTS `dcx_field_deleted_revision_23`;
CREATE VIEW `dcx_field_deleted_revision_23` AS SELECT * FROM commerceX.`field_deleted_revision_23`;


DROP TABLE IF EXISTS `dcx_field_deleted_revision_24`;
DROP VIEW IF EXISTS `dcx_field_deleted_revision_24`;
CREATE VIEW `dcx_field_deleted_revision_24` AS SELECT * FROM commerceX.`field_deleted_revision_24`;


DROP TABLE IF EXISTS `dcx_field_deleted_revision_25`;
DROP VIEW IF EXISTS `dcx_field_deleted_revision_25`;
CREATE VIEW `dcx_field_deleted_revision_25` AS SELECT * FROM commerceX.`field_deleted_revision_25`;


DROP TABLE IF EXISTS `dcx_field_group`;
DROP VIEW IF EXISTS `dcx_field_group`;
CREATE VIEW `dcx_field_group` AS SELECT * FROM commerceX.`field_group`;


DROP TABLE IF EXISTS `dcx_field_revision_body`;
DROP VIEW IF EXISTS `dcx_field_revision_body`;
CREATE VIEW `dcx_field_revision_body` AS SELECT * FROM commerceX.`field_revision_body`;


DROP TABLE IF EXISTS `dcx_field_revision_comment_body`;
DROP VIEW IF EXISTS `dcx_field_revision_comment_body`;
CREATE VIEW `dcx_field_revision_comment_body` AS SELECT * FROM commerceX.`field_revision_comment_body`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_customer_address`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_customer_address`;
CREATE VIEW `dcx_field_revision_commerce_customer_address` AS SELECT * FROM commerceX.`field_revision_commerce_customer_address`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_customer_billing`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_customer_billing`;
CREATE VIEW `dcx_field_revision_commerce_customer_billing` AS SELECT * FROM commerceX.`field_revision_commerce_customer_billing`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_customer_shipping`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_customer_shipping`;
CREATE VIEW `dcx_field_revision_commerce_customer_shipping` AS SELECT * FROM commerceX.`field_revision_commerce_customer_shipping`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_discount_date`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_discount_date`;
CREATE VIEW `dcx_field_revision_commerce_discount_date` AS SELECT * FROM commerceX.`field_revision_commerce_discount_date`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_discount_offer`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_discount_offer`;
CREATE VIEW `dcx_field_revision_commerce_discount_offer` AS SELECT * FROM commerceX.`field_revision_commerce_discount_offer`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_discounts`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_discounts`;
CREATE VIEW `dcx_field_revision_commerce_discounts` AS SELECT * FROM commerceX.`field_revision_commerce_discounts`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_display_path`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_display_path`;
CREATE VIEW `dcx_field_revision_commerce_display_path` AS SELECT * FROM commerceX.`field_revision_commerce_display_path`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_fixed_amount`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_fixed_amount`;
CREATE VIEW `dcx_field_revision_commerce_fixed_amount` AS SELECT * FROM commerceX.`field_revision_commerce_fixed_amount`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_free_products`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_free_products`;
CREATE VIEW `dcx_field_revision_commerce_free_products` AS SELECT * FROM commerceX.`field_revision_commerce_free_products`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_free_shipping`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_free_shipping`;
CREATE VIEW `dcx_field_revision_commerce_free_shipping` AS SELECT * FROM commerceX.`field_revision_commerce_free_shipping`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_line_items`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_line_items`;
CREATE VIEW `dcx_field_revision_commerce_line_items` AS SELECT * FROM commerceX.`field_revision_commerce_line_items`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_order_total`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_order_total`;
CREATE VIEW `dcx_field_revision_commerce_order_total` AS SELECT * FROM commerceX.`field_revision_commerce_order_total`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_percentage`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_percentage`;
CREATE VIEW `dcx_field_revision_commerce_percentage` AS SELECT * FROM commerceX.`field_revision_commerce_percentage`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_price`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_price`;
CREATE VIEW `dcx_field_revision_commerce_price` AS SELECT * FROM commerceX.`field_revision_commerce_price`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_product`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_product`;
CREATE VIEW `dcx_field_revision_commerce_product` AS SELECT * FROM commerceX.`field_revision_commerce_product`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_shipping_service`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_shipping_service`;
CREATE VIEW `dcx_field_revision_commerce_shipping_service` AS SELECT * FROM commerceX.`field_revision_commerce_shipping_service`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_total`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_total`;
CREATE VIEW `dcx_field_revision_commerce_total` AS SELECT * FROM commerceX.`field_revision_commerce_total`;


DROP TABLE IF EXISTS `dcx_field_revision_commerce_unit_price`;
DROP VIEW IF EXISTS `dcx_field_revision_commerce_unit_price`;
CREATE VIEW `dcx_field_revision_commerce_unit_price` AS SELECT * FROM commerceX.`field_revision_commerce_unit_price`;


DROP TABLE IF EXISTS `dcx_field_revision_field_availability_date`;
DROP VIEW IF EXISTS `dcx_field_revision_field_availability_date`;
CREATE VIEW `dcx_field_revision_field_availability_date` AS SELECT * FROM commerceX.`field_revision_field_availability_date`;


DROP TABLE IF EXISTS `dcx_field_revision_field_availability_period`;
DROP VIEW IF EXISTS `dcx_field_revision_field_availability_period`;
CREATE VIEW `dcx_field_revision_field_availability_period` AS SELECT * FROM commerceX.`field_revision_field_availability_period`;


DROP TABLE IF EXISTS `dcx_field_revision_field_brand`;
DROP VIEW IF EXISTS `dcx_field_revision_field_brand`;
CREATE VIEW `dcx_field_revision_field_brand` AS SELECT * FROM commerceX.`field_revision_field_brand`;


DROP TABLE IF EXISTS `dcx_field_revision_field_collection`;
DROP VIEW IF EXISTS `dcx_field_revision_field_collection`;
CREATE VIEW `dcx_field_revision_field_collection` AS SELECT * FROM commerceX.`field_revision_field_collection`;


DROP TABLE IF EXISTS `dcx_field_revision_field_colour`;
DROP VIEW IF EXISTS `dcx_field_revision_field_colour`;
CREATE VIEW `dcx_field_revision_field_colour` AS SELECT * FROM commerceX.`field_revision_field_colour`;


DROP TABLE IF EXISTS `dcx_field_revision_field_colour_code`;
DROP VIEW IF EXISTS `dcx_field_revision_field_colour_code`;
CREATE VIEW `dcx_field_revision_field_colour_code` AS SELECT * FROM commerceX.`field_revision_field_colour_code`;


DROP TABLE IF EXISTS `dcx_field_revision_field_headline`;
DROP VIEW IF EXISTS `dcx_field_revision_field_headline`;
CREATE VIEW `dcx_field_revision_field_headline` AS SELECT * FROM commerceX.`field_revision_field_headline`;


DROP TABLE IF EXISTS `dcx_field_revision_field_image`;
DROP VIEW IF EXISTS `dcx_field_revision_field_image`;
CREATE VIEW `dcx_field_revision_field_image` AS SELECT * FROM commerceX.`field_revision_field_image`;


DROP TABLE IF EXISTS `dcx_field_revision_field_images`;
DROP VIEW IF EXISTS `dcx_field_revision_field_images`;
CREATE VIEW `dcx_field_revision_field_images` AS SELECT * FROM commerceX.`field_revision_field_images`;


DROP TABLE IF EXISTS `dcx_field_revision_field_link`;
DROP VIEW IF EXISTS `dcx_field_revision_field_link`;
CREATE VIEW `dcx_field_revision_field_link` AS SELECT * FROM commerceX.`field_revision_field_link`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_01`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_01`;
CREATE VIEW `dcx_field_revision_field_price_pl_01` AS SELECT * FROM commerceX.`field_revision_field_price_pl_01`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_02`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_02`;
CREATE VIEW `dcx_field_revision_field_price_pl_02` AS SELECT * FROM commerceX.`field_revision_field_price_pl_02`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_03`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_03`;
CREATE VIEW `dcx_field_revision_field_price_pl_03` AS SELECT * FROM commerceX.`field_revision_field_price_pl_03`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_04`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_04`;
CREATE VIEW `dcx_field_revision_field_price_pl_04` AS SELECT * FROM commerceX.`field_revision_field_price_pl_04`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_05`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_05`;
CREATE VIEW `dcx_field_revision_field_price_pl_05` AS SELECT * FROM commerceX.`field_revision_field_price_pl_05`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_06`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_06`;
CREATE VIEW `dcx_field_revision_field_price_pl_06` AS SELECT * FROM commerceX.`field_revision_field_price_pl_06`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_07`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_07`;
CREATE VIEW `dcx_field_revision_field_price_pl_07` AS SELECT * FROM commerceX.`field_revision_field_price_pl_07`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_08`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_08`;
CREATE VIEW `dcx_field_revision_field_price_pl_08` AS SELECT * FROM commerceX.`field_revision_field_price_pl_08`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_09`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_09`;
CREATE VIEW `dcx_field_revision_field_price_pl_09` AS SELECT * FROM commerceX.`field_revision_field_price_pl_09`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_10`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_10`;
CREATE VIEW `dcx_field_revision_field_price_pl_10` AS SELECT * FROM commerceX.`field_revision_field_price_pl_10`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_11`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_11`;
CREATE VIEW `dcx_field_revision_field_price_pl_11` AS SELECT * FROM commerceX.`field_revision_field_price_pl_11`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_12`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_12`;
CREATE VIEW `dcx_field_revision_field_price_pl_12` AS SELECT * FROM commerceX.`field_revision_field_price_pl_12`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_13`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_13`;
CREATE VIEW `dcx_field_revision_field_price_pl_13` AS SELECT * FROM commerceX.`field_revision_field_price_pl_13`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_14`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_14`;
CREATE VIEW `dcx_field_revision_field_price_pl_14` AS SELECT * FROM commerceX.`field_revision_field_price_pl_14`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_15`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_15`;
CREATE VIEW `dcx_field_revision_field_price_pl_15` AS SELECT * FROM commerceX.`field_revision_field_price_pl_15`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_16`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_16`;
CREATE VIEW `dcx_field_revision_field_price_pl_16` AS SELECT * FROM commerceX.`field_revision_field_price_pl_16`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_17`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_17`;
CREATE VIEW `dcx_field_revision_field_price_pl_17` AS SELECT * FROM commerceX.`field_revision_field_price_pl_17`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_18`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_18`;
CREATE VIEW `dcx_field_revision_field_price_pl_18` AS SELECT * FROM commerceX.`field_revision_field_price_pl_18`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_19`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_19`;
CREATE VIEW `dcx_field_revision_field_price_pl_19` AS SELECT * FROM commerceX.`field_revision_field_price_pl_19`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_20`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_20`;
CREATE VIEW `dcx_field_revision_field_price_pl_20` AS SELECT * FROM commerceX.`field_revision_field_price_pl_20`;


DROP TABLE IF EXISTS `dcx_field_revision_field_price_pl_21`;
DROP VIEW IF EXISTS `dcx_field_revision_field_price_pl_21`;
CREATE VIEW `dcx_field_revision_field_price_pl_21` AS SELECT * FROM commerceX.`field_revision_field_price_pl_21`;


DROP TABLE IF EXISTS `dcx_field_revision_field_product`;
DROP VIEW IF EXISTS `dcx_field_revision_field_product`;
CREATE VIEW `dcx_field_revision_field_product` AS SELECT * FROM commerceX.`field_revision_field_product`;


DROP TABLE IF EXISTS `dcx_field_revision_field_product_category`;
DROP VIEW IF EXISTS `dcx_field_revision_field_product_category`;
CREATE VIEW `dcx_field_revision_field_product_category` AS SELECT * FROM commerceX.`field_revision_field_product_category`;


DROP TABLE IF EXISTS `dcx_field_revision_field_required_delivery_date`;
DROP VIEW IF EXISTS `dcx_field_revision_field_required_delivery_date`;
CREATE VIEW `dcx_field_revision_field_required_delivery_date` AS SELECT * FROM commerceX.`field_revision_field_required_delivery_date`;


DROP TABLE IF EXISTS `dcx_field_revision_field_rgb`;
DROP VIEW IF EXISTS `dcx_field_revision_field_rgb`;
CREATE VIEW `dcx_field_revision_field_rgb` AS SELECT * FROM commerceX.`field_revision_field_rgb`;


DROP TABLE IF EXISTS `dcx_field_revision_field_special_request`;
DROP VIEW IF EXISTS `dcx_field_revision_field_special_request`;
CREATE VIEW `dcx_field_revision_field_special_request` AS SELECT * FROM commerceX.`field_revision_field_special_request`;


DROP TABLE IF EXISTS `dcx_field_revision_field_stock_status`;
DROP VIEW IF EXISTS `dcx_field_revision_field_stock_status`;
CREATE VIEW `dcx_field_revision_field_stock_status` AS SELECT * FROM commerceX.`field_revision_field_stock_status`;


DROP TABLE IF EXISTS `dcx_field_revision_field_table`;
DROP VIEW IF EXISTS `dcx_field_revision_field_table`;
CREATE VIEW `dcx_field_revision_field_table` AS SELECT * FROM commerceX.`field_revision_field_table`;


DROP TABLE IF EXISTS `dcx_field_revision_field_table_`;
DROP VIEW IF EXISTS `dcx_field_revision_field_table_`;
CREATE VIEW `dcx_field_revision_field_table_` AS SELECT * FROM commerceX.`field_revision_field_table_`;


DROP TABLE IF EXISTS `dcx_field_revision_field_tagline`;
DROP VIEW IF EXISTS `dcx_field_revision_field_tagline`;
CREATE VIEW `dcx_field_revision_field_tagline` AS SELECT * FROM commerceX.`field_revision_field_tagline`;


DROP TABLE IF EXISTS `dcx_field_revision_field_trim_colour`;
DROP VIEW IF EXISTS `dcx_field_revision_field_trim_colour`;
CREATE VIEW `dcx_field_revision_field_trim_colour` AS SELECT * FROM commerceX.`field_revision_field_trim_colour`;


DROP TABLE IF EXISTS `dcx_field_revision_field_wholesale_price`;
DROP VIEW IF EXISTS `dcx_field_revision_field_wholesale_price`;
CREATE VIEW `dcx_field_revision_field_wholesale_price` AS SELECT * FROM commerceX.`field_revision_field_wholesale_price`;


DROP TABLE IF EXISTS `dcx_field_revision_inline_conditions`;
DROP VIEW IF EXISTS `dcx_field_revision_inline_conditions`;
CREATE VIEW `dcx_field_revision_inline_conditions` AS SELECT * FROM commerceX.`field_revision_inline_conditions`;


DROP TABLE IF EXISTS `dcx_field_revision_message_commerce_body`;
DROP VIEW IF EXISTS `dcx_field_revision_message_commerce_body`;
CREATE VIEW `dcx_field_revision_message_commerce_body` AS SELECT * FROM commerceX.`field_revision_message_commerce_body`;


DROP TABLE IF EXISTS `dcx_field_revision_message_commerce_line_item`;
DROP VIEW IF EXISTS `dcx_field_revision_message_commerce_line_item`;
CREATE VIEW `dcx_field_revision_message_commerce_line_item` AS SELECT * FROM commerceX.`field_revision_message_commerce_line_item`;


DROP TABLE IF EXISTS `dcx_field_revision_message_commerce_order`;
DROP VIEW IF EXISTS `dcx_field_revision_message_commerce_order`;
CREATE VIEW `dcx_field_revision_message_commerce_order` AS SELECT * FROM commerceX.`field_revision_message_commerce_order`;


DROP TABLE IF EXISTS `dcx_field_revision_message_commerce_payment`;
DROP VIEW IF EXISTS `dcx_field_revision_message_commerce_payment`;
CREATE VIEW `dcx_field_revision_message_commerce_payment` AS SELECT * FROM commerceX.`field_revision_message_commerce_payment`;


DROP TABLE IF EXISTS `dcx_field_revision_message_order_display_name`;
DROP VIEW IF EXISTS `dcx_field_revision_message_order_display_name`;
CREATE VIEW `dcx_field_revision_message_order_display_name` AS SELECT * FROM commerceX.`field_revision_message_order_display_name`;


DROP TABLE IF EXISTS `dcx_field_revision_message_text`;
DROP VIEW IF EXISTS `dcx_field_revision_message_text`;
CREATE VIEW `dcx_field_revision_message_text` AS SELECT * FROM commerceX.`field_revision_message_text`;


DROP TABLE IF EXISTS `dcx_field_revision_message_text_subject`;
DROP VIEW IF EXISTS `dcx_field_revision_message_text_subject`;
CREATE VIEW `dcx_field_revision_message_text_subject` AS SELECT * FROM commerceX.`field_revision_message_text_subject`;


DROP TABLE IF EXISTS `dcx_field_revision_title_field`;
DROP VIEW IF EXISTS `dcx_field_revision_title_field`;
CREATE VIEW `dcx_field_revision_title_field` AS SELECT * FROM commerceX.`field_revision_title_field`;


DROP TABLE IF EXISTS `dcx_file_managed`;
DROP VIEW IF EXISTS `dcx_file_managed`;
CREATE VIEW `dcx_file_managed` AS SELECT * FROM commerceX.`file_managed`;


DROP TABLE IF EXISTS `dcx_file_usage`;
DROP VIEW IF EXISTS `dcx_file_usage`;
CREATE VIEW `dcx_file_usage` AS SELECT * FROM commerceX.`file_usage`;


DROP TABLE IF EXISTS `dcx_filter`;
DROP VIEW IF EXISTS `dcx_filter`;
CREATE VIEW `dcx_filter` AS SELECT * FROM commerceX.`filter`;


DROP TABLE IF EXISTS `dcx_filter_format`;
DROP VIEW IF EXISTS `dcx_filter_format`;
CREATE VIEW `dcx_filter_format` AS SELECT * FROM commerceX.`filter_format`;


DROP TABLE IF EXISTS `dcx_flood`;
DROP VIEW IF EXISTS `dcx_flood`;
CREATE VIEW `dcx_flood` AS SELECT * FROM commerceX.`flood`;


DROP TABLE IF EXISTS `dcx_history`;
DROP VIEW IF EXISTS `dcx_history`;
CREATE VIEW `dcx_history` AS SELECT * FROM commerceX.`history`;


DROP TABLE IF EXISTS `dcx_honeypot_user`;
DROP VIEW IF EXISTS `dcx_honeypot_user`;
CREATE VIEW `dcx_honeypot_user` AS SELECT * FROM commerceX.`honeypot_user`;


DROP TABLE IF EXISTS `dcx_image_effects`;
DROP VIEW IF EXISTS `dcx_image_effects`;
CREATE VIEW `dcx_image_effects` AS SELECT * FROM commerceX.`image_effects`;


DROP TABLE IF EXISTS `dcx_image_styles`;
DROP VIEW IF EXISTS `dcx_image_styles`;
CREATE VIEW `dcx_image_styles` AS SELECT * FROM commerceX.`image_styles`;


DROP TABLE IF EXISTS `dcx_job_schedule`;
DROP VIEW IF EXISTS `dcx_job_schedule`;
CREATE VIEW `dcx_job_schedule` AS SELECT * FROM commerceX.`job_schedule`;


DROP TABLE IF EXISTS `dcx_masquerade`;
DROP VIEW IF EXISTS `dcx_masquerade`;
CREATE VIEW `dcx_masquerade` AS SELECT * FROM commerceX.`masquerade`;


DROP TABLE IF EXISTS `dcx_masquerade_users`;
DROP VIEW IF EXISTS `dcx_masquerade_users`;
CREATE VIEW `dcx_masquerade_users` AS SELECT * FROM commerceX.`masquerade_users`;


DROP TABLE IF EXISTS `dcx_megamenu`;
DROP VIEW IF EXISTS `dcx_megamenu`;
CREATE VIEW `dcx_megamenu` AS SELECT * FROM commerceX.`megamenu`;


DROP TABLE IF EXISTS `dcx_menu_custom`;
DROP VIEW IF EXISTS `dcx_menu_custom`;
CREATE VIEW `dcx_menu_custom` AS SELECT * FROM commerceX.`menu_custom`;


DROP TABLE IF EXISTS `dcx_menu_links`;
DROP VIEW IF EXISTS `dcx_menu_links`;
CREATE VIEW `dcx_menu_links` AS SELECT * FROM commerceX.`menu_links`;


DROP TABLE IF EXISTS `dcx_menu_router`;
DROP VIEW IF EXISTS `dcx_menu_router`;
CREATE VIEW `dcx_menu_router` AS SELECT * FROM commerceX.`menu_router`;


DROP TABLE IF EXISTS `dcx_message`;
DROP VIEW IF EXISTS `dcx_message`;
CREATE VIEW `dcx_message` AS SELECT * FROM commerceX.`message`;


DROP TABLE IF EXISTS `dcx_message_type`;
DROP VIEW IF EXISTS `dcx_message_type`;
CREATE VIEW `dcx_message_type` AS SELECT * FROM commerceX.`message_type`;


DROP TABLE IF EXISTS `dcx_message_type_category`;
DROP VIEW IF EXISTS `dcx_message_type_category`;
CREATE VIEW `dcx_message_type_category` AS SELECT * FROM commerceX.`message_type_category`;


DROP TABLE IF EXISTS `dcx_metatag`;
DROP VIEW IF EXISTS `dcx_metatag`;
CREATE VIEW `dcx_metatag` AS SELECT * FROM commerceX.`metatag`;


DROP TABLE IF EXISTS `dcx_metatag_config`;
DROP VIEW IF EXISTS `dcx_metatag_config`;
CREATE VIEW `dcx_metatag_config` AS SELECT * FROM commerceX.`metatag_config`;


DROP TABLE IF EXISTS `dcx_migrate_log`;
DROP VIEW IF EXISTS `dcx_migrate_log`;
CREATE VIEW `dcx_migrate_log` AS SELECT * FROM commerceX.`migrate_log`;


DROP TABLE IF EXISTS `dcx_migrate_map_commercekickstartadpush`;
DROP VIEW IF EXISTS `dcx_migrate_map_commercekickstartadpush`;
CREATE VIEW `dcx_migrate_map_commercekickstartadpush` AS SELECT * FROM commerceX.`migrate_map_commercekickstartadpush`;


DROP TABLE IF EXISTS `dcx_migrate_map_commercekickstartnode`;
DROP VIEW IF EXISTS `dcx_migrate_map_commercekickstartnode`;
CREATE VIEW `dcx_migrate_map_commercekickstartnode` AS SELECT * FROM commerceX.`migrate_map_commercekickstartnode`;


DROP TABLE IF EXISTS `dcx_migrate_map_commercekickstartpages`;
DROP VIEW IF EXISTS `dcx_migrate_map_commercekickstartpages`;
CREATE VIEW `dcx_migrate_map_commercekickstartpages` AS SELECT * FROM commerceX.`migrate_map_commercekickstartpages`;


DROP TABLE IF EXISTS `dcx_migrate_map_commercekickstartproduct`;
DROP VIEW IF EXISTS `dcx_migrate_map_commercekickstartproduct`;
CREATE VIEW `dcx_migrate_map_commercekickstartproduct` AS SELECT * FROM commerceX.`migrate_map_commercekickstartproduct`;


DROP TABLE IF EXISTS `dcx_migrate_map_commercekickstartslideshow`;
DROP VIEW IF EXISTS `dcx_migrate_map_commercekickstartslideshow`;
CREATE VIEW `dcx_migrate_map_commercekickstartslideshow` AS SELECT * FROM commerceX.`migrate_map_commercekickstartslideshow`;


DROP TABLE IF EXISTS `dcx_migrate_message_commercekickstartadpush`;
DROP VIEW IF EXISTS `dcx_migrate_message_commercekickstartadpush`;
CREATE VIEW `dcx_migrate_message_commercekickstartadpush` AS SELECT * FROM commerceX.`migrate_message_commercekickstartadpush`;


DROP TABLE IF EXISTS `dcx_migrate_message_commercekickstartnode`;
DROP VIEW IF EXISTS `dcx_migrate_message_commercekickstartnode`;
CREATE VIEW `dcx_migrate_message_commercekickstartnode` AS SELECT * FROM commerceX.`migrate_message_commercekickstartnode`;


DROP TABLE IF EXISTS `dcx_migrate_message_commercekickstartpages`;
DROP VIEW IF EXISTS `dcx_migrate_message_commercekickstartpages`;
CREATE VIEW `dcx_migrate_message_commercekickstartpages` AS SELECT * FROM commerceX.`migrate_message_commercekickstartpages`;


DROP TABLE IF EXISTS `dcx_migrate_message_commercekickstartproduct`;
DROP VIEW IF EXISTS `dcx_migrate_message_commercekickstartproduct`;
CREATE VIEW `dcx_migrate_message_commercekickstartproduct` AS SELECT * FROM commerceX.`migrate_message_commercekickstartproduct`;


DROP TABLE IF EXISTS `dcx_migrate_message_commercekickstartslideshow`;
DROP VIEW IF EXISTS `dcx_migrate_message_commercekickstartslideshow`;
CREATE VIEW `dcx_migrate_message_commercekickstartslideshow` AS SELECT * FROM commerceX.`migrate_message_commercekickstartslideshow`;


DROP TABLE IF EXISTS `dcx_migrate_status`;
DROP VIEW IF EXISTS `dcx_migrate_status`;
CREATE VIEW `dcx_migrate_status` AS SELECT * FROM commerceX.`migrate_status`;


DROP TABLE IF EXISTS `dcx_node`;
DROP VIEW IF EXISTS `dcx_node`;
CREATE VIEW `dcx_node` AS SELECT * FROM commerceX.`node`;


DROP TABLE IF EXISTS `dcx_node_access`;
DROP VIEW IF EXISTS `dcx_node_access`;
CREATE VIEW `dcx_node_access` AS SELECT * FROM commerceX.`node_access`;


DROP TABLE IF EXISTS `dcx_node_comment_statistics`;
DROP VIEW IF EXISTS `dcx_node_comment_statistics`;
CREATE VIEW `dcx_node_comment_statistics` AS SELECT * FROM commerceX.`node_comment_statistics`;


DROP TABLE IF EXISTS `dcx_node_counter`;
DROP VIEW IF EXISTS `dcx_node_counter`;
CREATE VIEW `dcx_node_counter` AS SELECT * FROM commerceX.`node_counter`;


DROP TABLE IF EXISTS `dcx_node_revision`;
DROP VIEW IF EXISTS `dcx_node_revision`;
CREATE VIEW `dcx_node_revision` AS SELECT * FROM commerceX.`node_revision`;


DROP TABLE IF EXISTS `dcx_node_spambot`;
DROP VIEW IF EXISTS `dcx_node_spambot`;
CREATE VIEW `dcx_node_spambot` AS SELECT * FROM commerceX.`node_spambot`;


DROP TABLE IF EXISTS `dcx_node_type`;
DROP VIEW IF EXISTS `dcx_node_type`;
CREATE VIEW `dcx_node_type` AS SELECT * FROM commerceX.`node_type`;


DROP TABLE IF EXISTS `dcx_page_title`;
DROP VIEW IF EXISTS `dcx_page_title`;
CREATE VIEW `dcx_page_title` AS SELECT * FROM commerceX.`page_title`;


DROP TABLE IF EXISTS `dcx_queue`;
DROP VIEW IF EXISTS `dcx_queue`;
CREATE VIEW `dcx_queue` AS SELECT * FROM commerceX.`queue`;


DROP TABLE IF EXISTS `dcx_redirect`;
DROP VIEW IF EXISTS `dcx_redirect`;
CREATE VIEW `dcx_redirect` AS SELECT * FROM commerceX.`redirect`;


DROP TABLE IF EXISTS `dcx_registry`;
DROP VIEW IF EXISTS `dcx_registry`;
CREATE VIEW `dcx_registry` AS SELECT * FROM commerceX.`registry`;


DROP TABLE IF EXISTS `dcx_registry_file`;
DROP VIEW IF EXISTS `dcx_registry_file`;
CREATE VIEW `dcx_registry_file` AS SELECT * FROM commerceX.`registry_file`;


DROP TABLE IF EXISTS `dcx_role`;
DROP VIEW IF EXISTS `dcx_role`;
CREATE VIEW `dcx_role` AS SELECT * FROM commerceX.`role`;


DROP TABLE IF EXISTS `dcx_role_permission`;
DROP VIEW IF EXISTS `dcx_role_permission`;
CREATE VIEW `dcx_role_permission` AS SELECT * FROM commerceX.`role_permission`;


DROP TABLE IF EXISTS `dcx_rules_config`;
DROP VIEW IF EXISTS `dcx_rules_config`;
CREATE VIEW `dcx_rules_config` AS SELECT * FROM commerceX.`rules_config`;


DROP TABLE IF EXISTS `dcx_rules_dependencies`;
DROP VIEW IF EXISTS `dcx_rules_dependencies`;
CREATE VIEW `dcx_rules_dependencies` AS SELECT * FROM commerceX.`rules_dependencies`;


DROP TABLE IF EXISTS `dcx_rules_tags`;
DROP VIEW IF EXISTS `dcx_rules_tags`;
CREATE VIEW `dcx_rules_tags` AS SELECT * FROM commerceX.`rules_tags`;


DROP TABLE IF EXISTS `dcx_rules_trigger`;
DROP VIEW IF EXISTS `dcx_rules_trigger`;
CREATE VIEW `dcx_rules_trigger` AS SELECT * FROM commerceX.`rules_trigger`;


DROP TABLE IF EXISTS `dcx_search_api_db_custom_product_display_field_product_commerce_pri`;
DROP VIEW IF EXISTS `dcx_search_api_db_custom_product_display_field_product_commerce_pri`;
CREATE VIEW `dcx_search_api_db_custom_product_display_field_product_commerce_pri` AS SELECT * FROM commerceX.`search_api_db_custom_product_display_field_product_commerce_pri`;


DROP TABLE IF EXISTS `dcx_search_api_db_custom_product_display_search_api_language`;
DROP VIEW IF EXISTS `dcx_search_api_db_custom_product_display_search_api_language`;
CREATE VIEW `dcx_search_api_db_custom_product_display_search_api_language` AS SELECT * FROM commerceX.`search_api_db_custom_product_display_search_api_language`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display`;
CREATE VIEW `dcx_search_api_db_product_display` AS SELECT * FROM commerceX.`search_api_db_product_display`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_created`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_created`;
CREATE VIEW `dcx_search_api_db_product_display_created` AS SELECT * FROM commerceX.`search_api_db_product_display_created`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_brand`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_brand`;
CREATE VIEW `dcx_search_api_db_product_display_field_brand` AS SELECT * FROM commerceX.`search_api_db_product_display_field_brand`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_collection`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_collection`;
CREATE VIEW `dcx_search_api_db_product_display_field_collection` AS SELECT * FROM commerceX.`search_api_db_product_display_field_collection`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_category`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_category`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_category` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_category`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_commerce_price_am_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_commerce_price_am_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_commerce_price_am_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_commerce_price_am_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_commerce_price_am_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_commerce_price_am_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_commerce_price_am_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_commerce_price_am_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_commerce_price_amou`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_commerce_price_amou`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_commerce_price_amou` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_commerce_price_amou`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_colour`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_colour`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_colour` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_colour`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_01_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_01_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_01_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_01_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_01_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_01_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_01_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_01_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_01_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_01_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_01_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_01_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_02_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_02_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_02_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_02_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_02_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_02_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_02_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_02_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_02_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_02_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_02_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_02_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_03_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_03_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_03_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_03_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_03_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_03_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_03_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_03_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_03_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_03_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_03_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_03_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_04_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_04_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_04_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_04_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_04_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_04_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_04_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_04_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_04_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_04_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_04_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_04_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_05_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_05_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_05_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_05_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_05_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_05_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_05_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_05_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_05_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_05_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_05_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_05_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_06_`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_06_`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_06_` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_06_`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_06_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_06_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_06_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_06_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_06_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_06_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_06_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_06_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_06_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_06_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_06_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_06_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_07_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_07_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_07_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_07_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_07_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_07_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_07_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_07_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_07_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_07_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_07_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_07_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_08_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_08_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_08_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_08_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_08_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_08_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_08_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_08_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_08_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_08_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_08_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_08_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_09_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_09_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_09_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_09_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_09_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_09_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_09_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_09_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_09_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_09_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_09_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_09_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_10_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_10_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_10_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_10_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_10_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_10_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_10_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_10_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_10_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_10_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_10_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_10_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_11_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_11_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_11_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_11_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_11_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_11_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_11_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_11_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_11_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_11_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_11_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_11_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_12_a`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_price_pl_12_a`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_price_pl_12_a` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_price_pl_12_a`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_stock_status`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_stock_status`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_stock_status` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_stock_status`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_field_trim_colour`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_field_trim_colour`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_field_trim_colour` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_field_trim_colour`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_title`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_title`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_title` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_title`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_title_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_title_1`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_title_1` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_title_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_title_2`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_title_2`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_title_2` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_title_2`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_field_product_title_3`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_field_product_title_3`;
CREATE VIEW `dcx_search_api_db_product_display_field_product_title_3` AS SELECT * FROM commerceX.`search_api_db_product_display_field_product_title_3`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_search_api_aggregation_1`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_search_api_aggregation_1`;
CREATE VIEW `dcx_search_api_db_product_display_search_api_aggregation_1` AS SELECT * FROM commerceX.`search_api_db_product_display_search_api_aggregation_1`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_search_api_language`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_search_api_language`;
CREATE VIEW `dcx_search_api_db_product_display_search_api_language` AS SELECT * FROM commerceX.`search_api_db_product_display_search_api_language`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_status`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_status`;
CREATE VIEW `dcx_search_api_db_product_display_status` AS SELECT * FROM commerceX.`search_api_db_product_display_status`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_text`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_text`;
CREATE VIEW `dcx_search_api_db_product_display_text` AS SELECT * FROM commerceX.`search_api_db_product_display_text`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_display_title`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_display_title`;
CREATE VIEW `dcx_search_api_db_product_display_title` AS SELECT * FROM commerceX.`search_api_db_product_display_title`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_search_field_product_field_colour`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_search_field_product_field_colour`;
CREATE VIEW `dcx_search_api_db_product_search_field_product_field_colour` AS SELECT * FROM commerceX.`search_api_db_product_search_field_product_field_colour`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_search_search_api_language`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_search_search_api_language`;
CREATE VIEW `dcx_search_api_db_product_search_search_api_language` AS SELECT * FROM commerceX.`search_api_db_product_search_search_api_language`;


DROP TABLE IF EXISTS `dcx_search_api_db_product_search_title_field`;
DROP VIEW IF EXISTS `dcx_search_api_db_product_search_title_field`;
CREATE VIEW `dcx_search_api_db_product_search_title_field` AS SELECT * FROM commerceX.`search_api_db_product_search_title_field`;


DROP TABLE IF EXISTS `dcx_search_api_index`;
DROP VIEW IF EXISTS `dcx_search_api_index`;
CREATE VIEW `dcx_search_api_index` AS SELECT * FROM commerceX.`search_api_index`;


DROP TABLE IF EXISTS `dcx_search_api_item`;
DROP VIEW IF EXISTS `dcx_search_api_item`;
CREATE VIEW `dcx_search_api_item` AS SELECT * FROM commerceX.`search_api_item`;


DROP TABLE IF EXISTS `dcx_search_api_server`;
DROP VIEW IF EXISTS `dcx_search_api_server`;
CREATE VIEW `dcx_search_api_server` AS SELECT * FROM commerceX.`search_api_server`;


DROP TABLE IF EXISTS `dcx_search_api_sort`;
DROP VIEW IF EXISTS `dcx_search_api_sort`;
CREATE VIEW `dcx_search_api_sort` AS SELECT * FROM commerceX.`search_api_sort`;


DROP TABLE IF EXISTS `dcx_search_api_task`;
DROP VIEW IF EXISTS `dcx_search_api_task`;
CREATE VIEW `dcx_search_api_task` AS SELECT * FROM commerceX.`search_api_task`;


DROP TABLE IF EXISTS `dcx_security_review`;
DROP VIEW IF EXISTS `dcx_security_review`;
CREATE VIEW `dcx_security_review` AS SELECT * FROM commerceX.`security_review`;


DROP TABLE IF EXISTS `dcx_semaphore`;
DROP VIEW IF EXISTS `dcx_semaphore`;
CREATE VIEW `dcx_semaphore` AS SELECT * FROM commerceX.`semaphore`;


DROP TABLE IF EXISTS `dcx_sequences`;
DROP VIEW IF EXISTS `dcx_sequences`;
CREATE VIEW `dcx_sequences` AS SELECT * FROM commerceX.`sequences`;


DROP TABLE IF EXISTS `dcx_sessions`;
DROP VIEW IF EXISTS `dcx_sessions`;
CREATE VIEW `dcx_sessions` AS SELECT * FROM commerceX.`sessions`;


DROP TABLE IF EXISTS `dcx_site_verify`;
DROP VIEW IF EXISTS `dcx_site_verify`;
CREATE VIEW `dcx_site_verify` AS SELECT * FROM commerceX.`site_verify`;


DROP TABLE IF EXISTS `dcx_system`;
DROP VIEW IF EXISTS `dcx_system`;
CREATE VIEW `dcx_system` AS SELECT * FROM commerceX.`system`;


DROP TABLE IF EXISTS `dcx_taxonomy_index`;
DROP VIEW IF EXISTS `dcx_taxonomy_index`;
CREATE VIEW `dcx_taxonomy_index` AS SELECT * FROM commerceX.`taxonomy_index`;


DROP TABLE IF EXISTS `dcx_taxonomy_menu`;
DROP VIEW IF EXISTS `dcx_taxonomy_menu`;
CREATE VIEW `dcx_taxonomy_menu` AS SELECT * FROM commerceX.`taxonomy_menu`;


DROP TABLE IF EXISTS `dcx_taxonomy_term_data`;
DROP VIEW IF EXISTS `dcx_taxonomy_term_data`;
CREATE VIEW `dcx_taxonomy_term_data` AS SELECT * FROM commerceX.`taxonomy_term_data`;


DROP TABLE IF EXISTS `dcx_taxonomy_term_hierarchy`;
DROP VIEW IF EXISTS `dcx_taxonomy_term_hierarchy`;
CREATE VIEW `dcx_taxonomy_term_hierarchy` AS SELECT * FROM commerceX.`taxonomy_term_hierarchy`;


DROP TABLE IF EXISTS `dcx_taxonomy_tools_role_access`;
DROP VIEW IF EXISTS `dcx_taxonomy_tools_role_access`;
CREATE VIEW `dcx_taxonomy_tools_role_access` AS SELECT * FROM commerceX.`taxonomy_tools_role_access`;


DROP TABLE IF EXISTS `dcx_taxonomy_vocabulary`;
DROP VIEW IF EXISTS `dcx_taxonomy_vocabulary`;
CREATE VIEW `dcx_taxonomy_vocabulary` AS SELECT * FROM commerceX.`taxonomy_vocabulary`;


DROP TABLE IF EXISTS `dcx_url_alias`;
DROP VIEW IF EXISTS `dcx_url_alias`;
CREATE VIEW `dcx_url_alias` AS SELECT * FROM commerceX.`url_alias`;


DROP TABLE IF EXISTS `dcx_users`;
DROP VIEW IF EXISTS `dcx_users`;
CREATE VIEW `dcx_users` AS SELECT * FROM commerceX.`users`;


DROP TABLE IF EXISTS `dcx_users_roles`;
DROP VIEW IF EXISTS `dcx_users_roles`;
CREATE VIEW `dcx_users_roles` AS SELECT * FROM commerceX.`users_roles`;


DROP TABLE IF EXISTS `dcx_variable`;
DROP VIEW IF EXISTS `dcx_variable`;
CREATE VIEW `dcx_variable` AS SELECT * FROM commerceX.`variable`;


DROP TABLE IF EXISTS `dcx_views_display`;
DROP VIEW IF EXISTS `dcx_views_display`;
CREATE VIEW `dcx_views_display` AS SELECT * FROM commerceX.`views_display`;


DROP TABLE IF EXISTS `dcx_views_view`;
DROP VIEW IF EXISTS `dcx_views_view`;
CREATE VIEW `dcx_views_view` AS SELECT * FROM commerceX.`views_view`;


DROP TABLE IF EXISTS `dcx_watchdog`;
DROP VIEW IF EXISTS `dcx_watchdog`;
CREATE VIEW `dcx_watchdog` AS SELECT * FROM commerceX.`watchdog`;


DROP TABLE IF EXISTS `dcx_xmlsitemap`;
DROP VIEW IF EXISTS `dcx_xmlsitemap`;
CREATE VIEW `dcx_xmlsitemap` AS SELECT * FROM commerceX.`xmlsitemap`;


DROP TABLE IF EXISTS `dcx_xmlsitemap_sitemap`;
DROP VIEW IF EXISTS `dcx_xmlsitemap_sitemap`;
CREATE VIEW `dcx_xmlsitemap_sitemap` AS SELECT * FROM commerceX.`xmlsitemap_sitemap`;


