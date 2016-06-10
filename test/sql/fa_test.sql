-- MySQL dump 10.13  Distrib 5.6.30, for Linux (x86_64)
--
-- Host: localhost    Database: Fames_test
-- ------------------------------------------------------
-- Server version	5.6.30

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `0_areas`
--

DROP TABLE IF EXISTS `0_areas`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_areas` (
  `area_code` int(11) NOT NULL AUTO_INCREMENT,
  `description` varchar(60) NOT NULL DEFAULT '',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`area_code`),
  UNIQUE KEY `description` (`description`)
) ENGINE=MyISAM AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_areas`
--

LOCK TABLES `0_areas` WRITE;
/*!40000 ALTER TABLE `0_areas` DISABLE KEYS */;
INSERT INTO `0_areas` VALUES (1,'UK',0);
/*!40000 ALTER TABLE `0_areas` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_attachments`
--

DROP TABLE IF EXISTS `0_attachments`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_attachments` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `description` varchar(60) NOT NULL DEFAULT '',
  `type_no` int(11) NOT NULL DEFAULT '0',
  `trans_no` int(11) NOT NULL DEFAULT '0',
  `unique_name` varchar(60) NOT NULL DEFAULT '',
  `tran_date` date NOT NULL DEFAULT '0000-00-00',
  `filename` varchar(60) NOT NULL DEFAULT '',
  `filesize` int(11) NOT NULL DEFAULT '0',
  `filetype` varchar(60) NOT NULL DEFAULT '',
  PRIMARY KEY (`id`),
  KEY `type_no` (`type_no`,`trans_no`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_attachments`
--

LOCK TABLES `0_attachments` WRITE;
/*!40000 ALTER TABLE `0_attachments` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_attachments` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_audit_trail`
--

DROP TABLE IF EXISTS `0_audit_trail`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_audit_trail` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `type` smallint(6) unsigned NOT NULL DEFAULT '0',
  `trans_no` int(11) unsigned NOT NULL DEFAULT '0',
  `user` smallint(6) unsigned NOT NULL DEFAULT '0',
  `stamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `description` varchar(60) DEFAULT NULL,
  `fiscal_year` int(11) NOT NULL,
  `gl_date` date NOT NULL DEFAULT '0000-00-00',
  `gl_seq` int(11) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `Seq` (`fiscal_year`,`gl_date`,`gl_seq`),
  KEY `Type_and_Number` (`type`,`trans_no`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_audit_trail`
--

LOCK TABLES `0_audit_trail` WRITE;
/*!40000 ALTER TABLE `0_audit_trail` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_audit_trail` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_bank_accounts`
--

DROP TABLE IF EXISTS `0_bank_accounts`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_bank_accounts` (
  `account_code` varchar(15) NOT NULL DEFAULT '',
  `account_type` smallint(6) NOT NULL DEFAULT '0',
  `bank_account_name` varchar(60) NOT NULL DEFAULT '',
  `bank_account_number` varchar(100) NOT NULL DEFAULT '',
  `bank_name` varchar(60) NOT NULL DEFAULT '',
  `bank_address` tinytext,
  `bank_curr_code` char(3) NOT NULL DEFAULT '',
  `dflt_curr_act` tinyint(1) NOT NULL DEFAULT '0',
  `id` smallint(6) NOT NULL AUTO_INCREMENT,
  `last_reconciled_date` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `ending_reconcile_balance` double NOT NULL DEFAULT '0',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `bank_account_name` (`bank_account_name`),
  KEY `bank_account_number` (`bank_account_number`),
  KEY `account_code` (`account_code`)
) ENGINE=MyISAM AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_bank_accounts`
--

LOCK TABLES `0_bank_accounts` WRITE;
/*!40000 ALTER TABLE `0_bank_accounts` DISABLE KEYS */;
INSERT INTO `0_bank_accounts` VALUES ('1200',0,'Current account','N/A','N/A',NULL,'GBP',0,1,'0000-00-00 00:00:00',0,0),('1230',3,'Petty Cash account','N/A','N/A',NULL,'GBP',0,2,'0000-00-00 00:00:00',0,0);
/*!40000 ALTER TABLE `0_bank_accounts` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_bank_trans`
--

DROP TABLE IF EXISTS `0_bank_trans`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_bank_trans` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `type` smallint(6) DEFAULT NULL,
  `trans_no` int(11) DEFAULT NULL,
  `bank_act` varchar(15) NOT NULL DEFAULT '',
  `ref` varchar(40) DEFAULT NULL,
  `trans_date` date NOT NULL DEFAULT '0000-00-00',
  `bank_trans_type_id` int(10) unsigned DEFAULT NULL,
  `amount` double DEFAULT NULL,
  `dimension_id` int(11) NOT NULL DEFAULT '0',
  `dimension2_id` int(11) NOT NULL DEFAULT '0',
  `person_type_id` int(11) NOT NULL DEFAULT '0',
  `person_id` tinyblob,
  `reconciled` date DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `bank_act` (`bank_act`,`ref`),
  KEY `type` (`type`,`trans_no`),
  KEY `bank_act_2` (`bank_act`,`reconciled`),
  KEY `bank_act_3` (`bank_act`,`trans_date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_bank_trans`
--

LOCK TABLES `0_bank_trans` WRITE;
/*!40000 ALTER TABLE `0_bank_trans` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_bank_trans` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_bom`
--

DROP TABLE IF EXISTS `0_bom`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_bom` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `parent` char(20) NOT NULL DEFAULT '',
  `component` char(20) NOT NULL DEFAULT '',
  `workcentre_added` int(11) NOT NULL DEFAULT '0',
  `loc_code` char(5) NOT NULL DEFAULT '',
  `quantity` double NOT NULL DEFAULT '1',
  PRIMARY KEY (`parent`,`component`,`workcentre_added`,`loc_code`),
  KEY `component` (`component`),
  KEY `id` (`id`),
  KEY `loc_code` (`loc_code`),
  KEY `parent` (`parent`,`loc_code`),
  KEY `workcentre_added` (`workcentre_added`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_bom`
--

LOCK TABLES `0_bom` WRITE;
/*!40000 ALTER TABLE `0_bom` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_bom` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_budget_trans`
--

DROP TABLE IF EXISTS `0_budget_trans`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_budget_trans` (
  `counter` int(11) NOT NULL AUTO_INCREMENT,
  `type` smallint(6) NOT NULL DEFAULT '0',
  `type_no` bigint(16) NOT NULL DEFAULT '1',
  `tran_date` date NOT NULL DEFAULT '0000-00-00',
  `account` varchar(15) NOT NULL DEFAULT '',
  `memo_` tinytext NOT NULL,
  `amount` double NOT NULL DEFAULT '0',
  `dimension_id` int(11) DEFAULT '0',
  `dimension2_id` int(11) DEFAULT '0',
  `person_type_id` int(11) DEFAULT NULL,
  `person_id` tinyblob,
  PRIMARY KEY (`counter`),
  KEY `Type_and_Number` (`type`,`type_no`),
  KEY `Account` (`account`,`tran_date`,`dimension_id`,`dimension2_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_budget_trans`
--

LOCK TABLES `0_budget_trans` WRITE;
/*!40000 ALTER TABLE `0_budget_trans` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_budget_trans` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_chart_class`
--

DROP TABLE IF EXISTS `0_chart_class`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_chart_class` (
  `cid` varchar(3) NOT NULL,
  `class_name` varchar(60) NOT NULL DEFAULT '',
  `ctype` tinyint(1) NOT NULL DEFAULT '0',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`cid`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_chart_class`
--

LOCK TABLES `0_chart_class` WRITE;
/*!40000 ALTER TABLE `0_chart_class` DISABLE KEYS */;
INSERT INTO `0_chart_class` VALUES ('1','Assets',1,0),('2','Liabilities',2,0),('3','Income',4,0),('4','Costs',6,0);
/*!40000 ALTER TABLE `0_chart_class` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_chart_master`
--

DROP TABLE IF EXISTS `0_chart_master`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_chart_master` (
  `account_code` varchar(15) NOT NULL DEFAULT '',
  `account_code2` varchar(15) NOT NULL DEFAULT '',
  `account_name` varchar(60) NOT NULL DEFAULT '',
  `account_type` varchar(10) NOT NULL DEFAULT '0',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`account_code`),
  KEY `account_name` (`account_name`),
  KEY `accounts_by_type` (`account_type`,`account_code`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_chart_master`
--

LOCK TABLES `0_chart_master` WRITE;
/*!40000 ALTER TABLE `0_chart_master` DISABLE KEYS */;
INSERT INTO `0_chart_master` VALUES ('0010','','Freehold Property','1',0),('0011','','Goodwill','1',0),('0012','','Goodwill Amortisation','1',0),('0020','','Plant and Machinery','1',0),('0021','','Plant/Machinery Depreciation','1',0),('0030','','Office Equipment','1',0),('0031','','Office Equipment Depreciation','1',0),('0040','','Furniture and Fixtures','1',0),('0041','','Furniture/Fixture Depreciation','1',0),('0050','','Motor Vehicles','1',0),('0051','','Motor Vehicles Depreciation','1',0),('1001','','Stock','2',0),('1002','','Work in Progress','2',0),('1100','','Debtors Control Account','3',0),('1102','','Other Debtors','3',0),('1103','','Prepayments','3',0),('1200','','Bank Current Account','3',0),('1210','','Bank Deposit Account','3',0),('1220','','Building Society Account','3',0),('1230','','Petty Cash','3',0),('1240','','Company Credit Card','3',0),('2100','','Creditors Control Account','4',0),('2102','','Other Creditors','4',0),('2109','','Accruals','4',0),('2200','','VAT (17.5%)','4',0),('2205','','VAT (5%)','4',0),('2210','','P.A.Y.E. &amp; National Insurance','4',0),('2220','','Net Wages','4',0),('2250','','Corporation Tax','4',0),('2300','','Bank Loan','5',0),('2305','','Directors loan account','5',0),('2310','','Hire Purchase','5',0),('2330','','Mortgages','5',0),('3000','','Ordinary Shares','6',0),('3010','','Preference Shares','6',0),('3100','','Share Premium Account','6',0),('3200','','Retained Earnings','6',0),('4000','','Sales','7',0),('4010','','Export Sales','7',0),('4009','','Discounts Allowed','7',0),('4900','','Miscellaneous Income','7',0),('4904','','Rent Income','7',0),('4906','','Interest received','7',0),('4910','','Shipping','7',0),('4920','','Foreign Exchange Gain','7',0),('5000','','Materials Purchased','8',0),('5001','','Materials Imported','8',0),('5002','','Opening Stock','8',0),('5003','','Closing Stock','8',0),('5200','','Packaging','8',0),('5201','','Discounts Taken','8',0),('5202','','Carriage','8',0),('5203','','Import Duty','8',0),('5204','','Transport Insurance','8',0),('5205','','Equipment Hire','8',0),('5220','','Foreign Exchange Loss','8',0),('6000','','Productive Labour','9',0),('6001','','Cost of Sales Labour','9',0),('6002','','Sub-Contractors','9',0),('7000','','Staff wages &amp; salaries','9',0),('7002','','Directors Remuneration','9',0),('7006','','Employers N.I.','9',0),('7007','','Employers Pensions','9',0),('7008','','Recruitment Expenses','9',0),('7100','','Rent','10',0),('7102','','Water Rates','10',0),('7103','','General Rates','10',0),('7104','','Premises Insurance','10',0),('7200','','Light &amp; heat','10',0),('7300','','Motor expenses','10',0),('7350','','Travelling','10',0),('7400','','Advertising','10',0),('7402','','P.R. (Literature &amp; Brochures)','10',0),('7403','','U.K. Entertainment','10',0),('7404','','Overseas Entertainment','10',0),('7500','','Postage and Carriage','10',0),('7501','','Office Stationery','10',0),('7502','','Telephone','10',0),('7506','','Web Site costs','10',0),('7600','','Legal Fees','10',0),('7601','','Audit and Accountancy Fees','10',0),('7603','','Professional Fees','10',0),('7701','','Office Machine Maintenance','10',0),('7710','','Computer expenses','10',0),('7800','','Repairs and Renewals','10',0),('7801','','Cleaning','10',0),('7802','','Laundry','10',0),('7900','','Bank Interest Paid','10',0),('7901','','Bank Charges','10',0),('7903','','Loan Interest Paid','10',0),('7904','','H.P. Interest','10',0),('8000','','Depreciation','10',0),('8005','','Goodwill Amortisation','10',0),('8100','','Bad Debt Write Off','10',0),('8201','','Subscriptions','10',0),('8202','','Clothing Costs','10',0),('8203','','Training Costs','10',0),('8204','','Insurance','10',0),('8205','','Refreshments','10',0),('8500','','Dividends','10',0),('8600','','Corporation Tax','10',0),('8601','','Profit/Loss Year','10',0),('9999','','Suspense Account','10',0);
/*!40000 ALTER TABLE `0_chart_master` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_chart_types`
--

DROP TABLE IF EXISTS `0_chart_types`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_chart_types` (
  `id` varchar(10) NOT NULL,
  `name` varchar(60) NOT NULL DEFAULT '',
  `class_id` varchar(3) NOT NULL DEFAULT '',
  `parent` varchar(10) NOT NULL DEFAULT '-1',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `name` (`name`),
  KEY `class_id` (`class_id`)
) ENGINE=MyISAM AUTO_INCREMENT=53 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_chart_types`
--

LOCK TABLES `0_chart_types` WRITE;
/*!40000 ALTER TABLE `0_chart_types` DISABLE KEYS */;
INSERT INTO `0_chart_types` VALUES ('1','CAPITAL ASSETS','1','',0),('2','INVENTORY ASSETS','1','',0),('3','CURRENT ASSETS','1','',0),('4','CURRENT LIABILITIES','2','',0),('5','LONG TERM LIABILITIES','2','',0),('6','SHARE CAPITAL','2','',0),('7','SALES REVENUE','3','',0),('8','COST OF GOODS SOLD','4','',0),('9','PAYROLL EXPENSES','4','',0),('10','GENERAL &amp; ADMINISTRATIVE EXPENSES','4','',0);
/*!40000 ALTER TABLE `0_chart_types` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_comments`
--

DROP TABLE IF EXISTS `0_comments`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_comments` (
  `type` int(11) NOT NULL DEFAULT '0',
  `id` int(11) NOT NULL DEFAULT '0',
  `date_` date DEFAULT '0000-00-00',
  `memo_` tinytext,
  KEY `type_and_id` (`type`,`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_comments`
--

LOCK TABLES `0_comments` WRITE;
/*!40000 ALTER TABLE `0_comments` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_comments` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_credit_status`
--

DROP TABLE IF EXISTS `0_credit_status`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_credit_status` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `reason_description` char(100) NOT NULL DEFAULT '',
  `dissallow_invoices` tinyint(1) NOT NULL DEFAULT '0',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `reason_description` (`reason_description`)
) ENGINE=MyISAM AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_credit_status`
--

LOCK TABLES `0_credit_status` WRITE;
/*!40000 ALTER TABLE `0_credit_status` DISABLE KEYS */;
INSERT INTO `0_credit_status` VALUES (1,'Good History',0,0),(3,'No more work until payment received',1,0),(4,'In liquidation',1,0);
/*!40000 ALTER TABLE `0_credit_status` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_crm_categories`
--

DROP TABLE IF EXISTS `0_crm_categories`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_crm_categories` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'pure technical key',
  `type` varchar(20) NOT NULL COMMENT 'contact type e.g. customer',
  `action` varchar(20) NOT NULL COMMENT 'detailed usage e.g. department',
  `name` varchar(30) NOT NULL COMMENT 'for category selector',
  `description` tinytext NOT NULL COMMENT 'usage description',
  `system` tinyint(1) NOT NULL DEFAULT '0' COMMENT 'nonzero for core system usage',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `type` (`type`,`action`),
  UNIQUE KEY `type_2` (`type`,`name`)
) ENGINE=InnoDB AUTO_INCREMENT=13 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_crm_categories`
--

LOCK TABLES `0_crm_categories` WRITE;
/*!40000 ALTER TABLE `0_crm_categories` DISABLE KEYS */;
INSERT INTO `0_crm_categories` VALUES (1,'cust_branch','general','General','General contact data for customer branch (overrides company setting)',1,0),(2,'cust_branch','invoice','Invoices','Invoice posting (overrides company setting)',1,0),(3,'cust_branch','order','Orders','Order confirmation (overrides company setting)',1,0),(4,'cust_branch','delivery','Deliveries','Delivery coordination (overrides company setting)',1,0),(5,'customer','general','General','General contact data for customer',1,0),(6,'customer','order','Orders','Order confirmation',1,0),(7,'customer','delivery','Deliveries','Delivery coordination',1,0),(8,'customer','invoice','Invoices','Invoice posting',1,0),(9,'supplier','general','General','General contact data for supplier',1,0),(10,'supplier','order','Orders','Order confirmation',1,0),(11,'supplier','delivery','Deliveries','Delivery coordination',1,0),(12,'supplier','invoice','Invoices','Invoice posting',1,0);
/*!40000 ALTER TABLE `0_crm_categories` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_crm_contacts`
--

DROP TABLE IF EXISTS `0_crm_contacts`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_crm_contacts` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `person_id` int(11) NOT NULL DEFAULT '0' COMMENT 'foreign key to crm_contacts',
  `type` varchar(20) NOT NULL COMMENT 'foreign key to crm_categories',
  `action` varchar(20) NOT NULL COMMENT 'foreign key to crm_categories',
  `entity_id` varchar(11) DEFAULT NULL COMMENT 'entity id in related class table',
  PRIMARY KEY (`id`),
  KEY `type` (`type`,`action`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_crm_contacts`
--

LOCK TABLES `0_crm_contacts` WRITE;
/*!40000 ALTER TABLE `0_crm_contacts` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_crm_contacts` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_crm_persons`
--

DROP TABLE IF EXISTS `0_crm_persons`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_crm_persons` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `ref` varchar(30) NOT NULL,
  `name` varchar(60) NOT NULL,
  `name2` varchar(60) DEFAULT NULL,
  `address` tinytext,
  `phone` varchar(30) DEFAULT NULL,
  `phone2` varchar(30) DEFAULT NULL,
  `fax` varchar(30) DEFAULT NULL,
  `email` varchar(100) DEFAULT NULL,
  `lang` char(5) DEFAULT NULL,
  `notes` tinytext NOT NULL,
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `ref` (`ref`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_crm_persons`
--

LOCK TABLES `0_crm_persons` WRITE;
/*!40000 ALTER TABLE `0_crm_persons` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_crm_persons` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_currencies`
--

DROP TABLE IF EXISTS `0_currencies`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_currencies` (
  `currency` varchar(60) NOT NULL DEFAULT '',
  `curr_abrev` char(3) NOT NULL DEFAULT '',
  `curr_symbol` varchar(10) NOT NULL DEFAULT '',
  `country` varchar(100) NOT NULL DEFAULT '',
  `hundreds_name` varchar(15) NOT NULL DEFAULT '',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  `auto_update` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`curr_abrev`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_currencies`
--

LOCK TABLES `0_currencies` WRITE;
/*!40000 ALTER TABLE `0_currencies` DISABLE KEYS */;
INSERT INTO `0_currencies` VALUES ('Pounds','GBP','?','England','Pence',0,1),('US Dollars','USD','$','United States','Cents',0,1),('Euro','EUR','?','Europe','Cents',0,1);
/*!40000 ALTER TABLE `0_currencies` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_cust_allocations`
--

DROP TABLE IF EXISTS `0_cust_allocations`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_cust_allocations` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `amt` double unsigned DEFAULT NULL,
  `date_alloc` date NOT NULL DEFAULT '0000-00-00',
  `trans_no_from` int(11) DEFAULT NULL,
  `trans_type_from` int(11) DEFAULT NULL,
  `trans_no_to` int(11) DEFAULT NULL,
  `trans_type_to` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `From` (`trans_type_from`,`trans_no_from`),
  KEY `To` (`trans_type_to`,`trans_no_to`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_cust_allocations`
--

LOCK TABLES `0_cust_allocations` WRITE;
/*!40000 ALTER TABLE `0_cust_allocations` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_cust_allocations` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_cust_branch`
--

DROP TABLE IF EXISTS `0_cust_branch`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_cust_branch` (
  `branch_code` int(11) NOT NULL AUTO_INCREMENT,
  `debtor_no` int(11) NOT NULL DEFAULT '0',
  `br_name` varchar(60) NOT NULL DEFAULT '',
  `br_address` tinytext NOT NULL,
  `area` int(11) DEFAULT NULL,
  `salesman` int(11) NOT NULL DEFAULT '0',
  `contact_name` varchar(60) NOT NULL DEFAULT '',
  `default_location` varchar(5) NOT NULL DEFAULT '',
  `tax_group_id` int(11) DEFAULT NULL,
  `sales_account` varchar(15) NOT NULL DEFAULT '',
  `sales_discount_account` varchar(15) NOT NULL DEFAULT '',
  `receivables_account` varchar(15) NOT NULL DEFAULT '',
  `payment_discount_account` varchar(15) NOT NULL DEFAULT '',
  `default_ship_via` int(11) NOT NULL DEFAULT '1',
  `disable_trans` tinyint(4) NOT NULL DEFAULT '0',
  `br_post_address` tinytext NOT NULL,
  `group_no` int(11) NOT NULL DEFAULT '0',
  `notes` tinytext,
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  `branch_ref` varchar(30) NOT NULL,
  PRIMARY KEY (`branch_code`,`debtor_no`),
  KEY `branch_code` (`branch_code`),
  KEY `branch_ref` (`branch_ref`),
  KEY `group_no` (`group_no`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_cust_branch`
--

LOCK TABLES `0_cust_branch` WRITE;
/*!40000 ALTER TABLE `0_cust_branch` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_cust_branch` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_debtor_trans`
--

DROP TABLE IF EXISTS `0_debtor_trans`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_debtor_trans` (
  `trans_no` int(11) unsigned NOT NULL DEFAULT '0',
  `type` smallint(6) unsigned NOT NULL DEFAULT '0',
  `version` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `debtor_no` int(11) unsigned DEFAULT NULL,
  `branch_code` int(11) NOT NULL DEFAULT '-1',
  `tran_date` date NOT NULL DEFAULT '0000-00-00',
  `due_date` date NOT NULL DEFAULT '0000-00-00',
  `reference` varchar(60) NOT NULL DEFAULT '',
  `tpe` int(11) NOT NULL DEFAULT '0',
  `order_` int(11) NOT NULL DEFAULT '0',
  `ov_amount` double NOT NULL DEFAULT '0',
  `ov_gst` double NOT NULL DEFAULT '0',
  `ov_freight` double NOT NULL DEFAULT '0',
  `ov_freight_tax` double NOT NULL DEFAULT '0',
  `ov_discount` double NOT NULL DEFAULT '0',
  `alloc` double NOT NULL DEFAULT '0',
  `rate` double NOT NULL DEFAULT '1',
  `ship_via` int(11) DEFAULT NULL,
  `dimension_id` int(11) NOT NULL DEFAULT '0',
  `dimension2_id` int(11) NOT NULL DEFAULT '0',
  `payment_terms` int(11) DEFAULT NULL,
  PRIMARY KEY (`type`,`trans_no`),
  KEY `debtor_no` (`debtor_no`,`branch_code`),
  KEY `tran_date` (`tran_date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_debtor_trans`
--

LOCK TABLES `0_debtor_trans` WRITE;
/*!40000 ALTER TABLE `0_debtor_trans` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_debtor_trans` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_debtor_trans_details`
--

DROP TABLE IF EXISTS `0_debtor_trans_details`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_debtor_trans_details` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `debtor_trans_no` int(11) DEFAULT NULL,
  `debtor_trans_type` int(11) DEFAULT NULL,
  `stock_id` varchar(20) NOT NULL DEFAULT '',
  `description` tinytext,
  `unit_price` double NOT NULL DEFAULT '0',
  `unit_tax` double NOT NULL DEFAULT '0',
  `quantity` double NOT NULL DEFAULT '0',
  `discount_percent` double NOT NULL DEFAULT '0',
  `standard_cost` double NOT NULL DEFAULT '0',
  `qty_done` double NOT NULL DEFAULT '0',
  `src_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `Transaction` (`debtor_trans_type`,`debtor_trans_no`),
  KEY `src_id` (`src_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_debtor_trans_details`
--

LOCK TABLES `0_debtor_trans_details` WRITE;
/*!40000 ALTER TABLE `0_debtor_trans_details` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_debtor_trans_details` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_debtors_master`
--

DROP TABLE IF EXISTS `0_debtors_master`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_debtors_master` (
  `debtor_no` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(100) NOT NULL DEFAULT '',
  `address` tinytext,
  `tax_id` varchar(55) NOT NULL DEFAULT '',
  `curr_code` char(3) NOT NULL DEFAULT '',
  `sales_type` int(11) NOT NULL DEFAULT '1',
  `dimension_id` int(11) NOT NULL DEFAULT '0',
  `dimension2_id` int(11) NOT NULL DEFAULT '0',
  `credit_status` int(11) NOT NULL DEFAULT '0',
  `payment_terms` int(11) DEFAULT NULL,
  `discount` double NOT NULL DEFAULT '0',
  `pymt_discount` double NOT NULL DEFAULT '0',
  `credit_limit` float NOT NULL DEFAULT '1000',
  `notes` tinytext,
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  `debtor_ref` varchar(30) NOT NULL,
  PRIMARY KEY (`debtor_no`),
  UNIQUE KEY `debtor_ref` (`debtor_ref`),
  KEY `name` (`name`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_debtors_master`
--

LOCK TABLES `0_debtors_master` WRITE;
/*!40000 ALTER TABLE `0_debtors_master` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_debtors_master` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_dimensions`
--

DROP TABLE IF EXISTS `0_dimensions`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_dimensions` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `reference` varchar(60) NOT NULL DEFAULT '',
  `name` varchar(60) NOT NULL DEFAULT '',
  `type_` tinyint(1) NOT NULL DEFAULT '1',
  `closed` tinyint(1) NOT NULL DEFAULT '0',
  `date_` date NOT NULL DEFAULT '0000-00-00',
  `due_date` date NOT NULL DEFAULT '0000-00-00',
  PRIMARY KEY (`id`),
  UNIQUE KEY `reference` (`reference`),
  KEY `date_` (`date_`),
  KEY `due_date` (`due_date`),
  KEY `type_` (`type_`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_dimensions`
--

LOCK TABLES `0_dimensions` WRITE;
/*!40000 ALTER TABLE `0_dimensions` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_dimensions` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_exchange_rates`
--

DROP TABLE IF EXISTS `0_exchange_rates`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_exchange_rates` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `curr_code` char(3) NOT NULL DEFAULT '',
  `rate_buy` double NOT NULL DEFAULT '0',
  `rate_sell` double NOT NULL DEFAULT '0',
  `date_` date NOT NULL DEFAULT '0000-00-00',
  PRIMARY KEY (`id`),
  UNIQUE KEY `curr_code` (`curr_code`,`date_`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_exchange_rates`
--

LOCK TABLES `0_exchange_rates` WRITE;
/*!40000 ALTER TABLE `0_exchange_rates` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_exchange_rates` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_fiscal_year`
--

DROP TABLE IF EXISTS `0_fiscal_year`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_fiscal_year` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `begin` date DEFAULT '0000-00-00',
  `end` date DEFAULT '0000-00-00',
  `closed` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `begin` (`begin`),
  UNIQUE KEY `end` (`end`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_fiscal_year`
--

LOCK TABLES `0_fiscal_year` WRITE;
/*!40000 ALTER TABLE `0_fiscal_year` DISABLE KEYS */;
INSERT INTO `0_fiscal_year` VALUES (1,'2008-01-01','2008-12-31',0);
/*!40000 ALTER TABLE `0_fiscal_year` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_gl_trans`
--

DROP TABLE IF EXISTS `0_gl_trans`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_gl_trans` (
  `counter` int(11) NOT NULL AUTO_INCREMENT,
  `type` smallint(6) NOT NULL DEFAULT '0',
  `type_no` bigint(16) NOT NULL DEFAULT '1',
  `tran_date` date NOT NULL DEFAULT '0000-00-00',
  `account` varchar(15) NOT NULL DEFAULT '',
  `memo_` tinytext NOT NULL,
  `amount` double NOT NULL DEFAULT '0',
  `dimension_id` int(11) NOT NULL DEFAULT '0',
  `dimension2_id` int(11) NOT NULL DEFAULT '0',
  `person_type_id` int(11) DEFAULT NULL,
  `person_id` tinyblob,
  PRIMARY KEY (`counter`),
  KEY `Type_and_Number` (`type`,`type_no`),
  KEY `dimension_id` (`dimension_id`),
  KEY `dimension2_id` (`dimension2_id`),
  KEY `tran_date` (`tran_date`),
  KEY `account_and_tran_date` (`account`,`tran_date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_gl_trans`
--

LOCK TABLES `0_gl_trans` WRITE;
/*!40000 ALTER TABLE `0_gl_trans` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_gl_trans` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_grn_batch`
--

DROP TABLE IF EXISTS `0_grn_batch`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_grn_batch` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `supplier_id` int(11) NOT NULL DEFAULT '0',
  `purch_order_no` int(11) DEFAULT NULL,
  `reference` varchar(60) NOT NULL DEFAULT '',
  `delivery_date` date NOT NULL DEFAULT '0000-00-00',
  `loc_code` varchar(5) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `delivery_date` (`delivery_date`),
  KEY `purch_order_no` (`purch_order_no`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_grn_batch`
--

LOCK TABLES `0_grn_batch` WRITE;
/*!40000 ALTER TABLE `0_grn_batch` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_grn_batch` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_grn_items`
--

DROP TABLE IF EXISTS `0_grn_items`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_grn_items` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `grn_batch_id` int(11) DEFAULT NULL,
  `po_detail_item` int(11) NOT NULL DEFAULT '0',
  `item_code` varchar(20) NOT NULL DEFAULT '',
  `description` tinytext,
  `qty_recd` double NOT NULL DEFAULT '0',
  `quantity_inv` double NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `grn_batch_id` (`grn_batch_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_grn_items`
--

LOCK TABLES `0_grn_items` WRITE;
/*!40000 ALTER TABLE `0_grn_items` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_grn_items` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_groups`
--

DROP TABLE IF EXISTS `0_groups`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_groups` (
  `id` smallint(6) unsigned NOT NULL AUTO_INCREMENT,
  `description` varchar(60) NOT NULL DEFAULT '',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `description` (`description`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_groups`
--

LOCK TABLES `0_groups` WRITE;
/*!40000 ALTER TABLE `0_groups` DISABLE KEYS */;
INSERT INTO `0_groups` VALUES (1,'Small',0),(2,'Medium',0),(3,'Large',0);
/*!40000 ALTER TABLE `0_groups` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_item_codes`
--

DROP TABLE IF EXISTS `0_item_codes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_item_codes` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `item_code` varchar(20) NOT NULL,
  `stock_id` varchar(20) NOT NULL,
  `description` varchar(200) NOT NULL DEFAULT '',
  `category_id` smallint(6) unsigned NOT NULL,
  `quantity` double NOT NULL DEFAULT '1',
  `is_foreign` tinyint(1) NOT NULL DEFAULT '0',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `stock_id` (`stock_id`,`item_code`),
  KEY `item_code` (`item_code`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_item_codes`
--

LOCK TABLES `0_item_codes` WRITE;
/*!40000 ALTER TABLE `0_item_codes` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_item_codes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_item_tax_type_exemptions`
--

DROP TABLE IF EXISTS `0_item_tax_type_exemptions`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_item_tax_type_exemptions` (
  `item_tax_type_id` int(11) NOT NULL DEFAULT '0',
  `tax_type_id` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`item_tax_type_id`,`tax_type_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_item_tax_type_exemptions`
--

LOCK TABLES `0_item_tax_type_exemptions` WRITE;
/*!40000 ALTER TABLE `0_item_tax_type_exemptions` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_item_tax_type_exemptions` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_item_tax_types`
--

DROP TABLE IF EXISTS `0_item_tax_types`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_item_tax_types` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(60) NOT NULL DEFAULT '',
  `exempt` tinyint(1) NOT NULL DEFAULT '0',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_item_tax_types`
--

LOCK TABLES `0_item_tax_types` WRITE;
/*!40000 ALTER TABLE `0_item_tax_types` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_item_tax_types` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_item_units`
--

DROP TABLE IF EXISTS `0_item_units`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_item_units` (
  `abbr` varchar(20) NOT NULL,
  `name` varchar(40) NOT NULL,
  `decimals` tinyint(2) NOT NULL,
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`abbr`),
  UNIQUE KEY `name` (`name`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_item_units`
--

LOCK TABLES `0_item_units` WRITE;
/*!40000 ALTER TABLE `0_item_units` DISABLE KEYS */;
INSERT INTO `0_item_units` VALUES ('ea.','Each',0,0);
/*!40000 ALTER TABLE `0_item_units` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_loc_stock`
--

DROP TABLE IF EXISTS `0_loc_stock`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_loc_stock` (
  `loc_code` char(5) NOT NULL DEFAULT '',
  `stock_id` char(20) NOT NULL DEFAULT '',
  `reorder_level` bigint(20) NOT NULL DEFAULT '0',
  PRIMARY KEY (`loc_code`,`stock_id`),
  KEY `stock_id` (`stock_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_loc_stock`
--

LOCK TABLES `0_loc_stock` WRITE;
/*!40000 ALTER TABLE `0_loc_stock` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_loc_stock` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_locations`
--

DROP TABLE IF EXISTS `0_locations`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_locations` (
  `loc_code` varchar(5) NOT NULL DEFAULT '',
  `location_name` varchar(60) NOT NULL DEFAULT '',
  `delivery_address` tinytext NOT NULL,
  `phone` varchar(30) NOT NULL DEFAULT '',
  `phone2` varchar(30) NOT NULL DEFAULT '',
  `fax` varchar(30) NOT NULL DEFAULT '',
  `email` varchar(100) NOT NULL DEFAULT '',
  `contact` varchar(30) NOT NULL DEFAULT '',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`loc_code`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_locations`
--

LOCK TABLES `0_locations` WRITE;
/*!40000 ALTER TABLE `0_locations` DISABLE KEYS */;
INSERT INTO `0_locations` VALUES ('DEF','Default','N/A','','','','','',0);
/*!40000 ALTER TABLE `0_locations` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_movement_types`
--

DROP TABLE IF EXISTS `0_movement_types`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_movement_types` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(60) NOT NULL DEFAULT '',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=MyISAM AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_movement_types`
--

LOCK TABLES `0_movement_types` WRITE;
/*!40000 ALTER TABLE `0_movement_types` DISABLE KEYS */;
INSERT INTO `0_movement_types` VALUES (1,'Adjustment',0);
/*!40000 ALTER TABLE `0_movement_types` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_payment_terms`
--

DROP TABLE IF EXISTS `0_payment_terms`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_payment_terms` (
  `terms_indicator` int(11) NOT NULL AUTO_INCREMENT,
  `terms` char(80) NOT NULL DEFAULT '',
  `days_before_due` smallint(6) NOT NULL DEFAULT '0',
  `day_in_following_month` smallint(6) NOT NULL DEFAULT '0',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`terms_indicator`),
  UNIQUE KEY `terms` (`terms`)
) ENGINE=MyISAM AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_payment_terms`
--

LOCK TABLES `0_payment_terms` WRITE;
/*!40000 ALTER TABLE `0_payment_terms` DISABLE KEYS */;
INSERT INTO `0_payment_terms` VALUES (1,'Due 15th Of the Following Month',0,17,0),(2,'Due By End Of The Following Month',0,30,0),(3,'Payment due within 10 days',10,0,0),(4,'Cash Only',1,0,0);
/*!40000 ALTER TABLE `0_payment_terms` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_prices`
--

DROP TABLE IF EXISTS `0_prices`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_prices` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `stock_id` varchar(20) NOT NULL DEFAULT '',
  `sales_type_id` int(11) NOT NULL DEFAULT '0',
  `curr_abrev` char(3) NOT NULL DEFAULT '',
  `price` double NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `price` (`stock_id`,`sales_type_id`,`curr_abrev`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_prices`
--

LOCK TABLES `0_prices` WRITE;
/*!40000 ALTER TABLE `0_prices` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_prices` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_print_profiles`
--

DROP TABLE IF EXISTS `0_print_profiles`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_print_profiles` (
  `id` smallint(6) unsigned NOT NULL AUTO_INCREMENT,
  `profile` varchar(30) NOT NULL,
  `report` varchar(5) DEFAULT NULL,
  `printer` tinyint(3) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `profile` (`profile`,`report`)
) ENGINE=MyISAM AUTO_INCREMENT=10 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_print_profiles`
--

LOCK TABLES `0_print_profiles` WRITE;
/*!40000 ALTER TABLE `0_print_profiles` DISABLE KEYS */;
INSERT INTO `0_print_profiles` VALUES (1,'Out of office',NULL,0),(2,'Sales Department',NULL,0),(3,'Central',NULL,2),(4,'Sales Department','104',2),(5,'Sales Department','105',2),(6,'Sales Department','107',2),(7,'Sales Department','109',2),(8,'Sales Department','110',2),(9,'Sales Department','201',2);
/*!40000 ALTER TABLE `0_print_profiles` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_printers`
--

DROP TABLE IF EXISTS `0_printers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_printers` (
  `id` tinyint(3) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) NOT NULL,
  `description` varchar(60) NOT NULL,
  `queue` varchar(20) NOT NULL,
  `host` varchar(40) NOT NULL,
  `port` smallint(11) unsigned NOT NULL,
  `timeout` tinyint(3) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_printers`
--

LOCK TABLES `0_printers` WRITE;
/*!40000 ALTER TABLE `0_printers` DISABLE KEYS */;
INSERT INTO `0_printers` VALUES (1,'QL500','Label printer','QL500','server',127,20),(2,'Samsung','Main network printer','scx4521F','server',515,5),(3,'Local','Local print server at user IP','lp','',515,10);
/*!40000 ALTER TABLE `0_printers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_purch_data`
--

DROP TABLE IF EXISTS `0_purch_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_purch_data` (
  `supplier_id` int(11) NOT NULL DEFAULT '0',
  `stock_id` char(20) NOT NULL DEFAULT '',
  `price` double NOT NULL DEFAULT '0',
  `suppliers_uom` char(50) NOT NULL DEFAULT '',
  `conversion_factor` double NOT NULL DEFAULT '1',
  `supplier_description` char(50) NOT NULL DEFAULT '',
  PRIMARY KEY (`supplier_id`,`stock_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_purch_data`
--

LOCK TABLES `0_purch_data` WRITE;
/*!40000 ALTER TABLE `0_purch_data` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_purch_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_purch_order_details`
--

DROP TABLE IF EXISTS `0_purch_order_details`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_purch_order_details` (
  `po_detail_item` int(11) NOT NULL AUTO_INCREMENT,
  `order_no` int(11) NOT NULL DEFAULT '0',
  `item_code` varchar(20) NOT NULL DEFAULT '',
  `description` tinytext,
  `delivery_date` date NOT NULL DEFAULT '0000-00-00',
  `qty_invoiced` double NOT NULL DEFAULT '0',
  `unit_price` double NOT NULL DEFAULT '0',
  `act_price` double NOT NULL DEFAULT '0',
  `std_cost_unit` double NOT NULL DEFAULT '0',
  `quantity_ordered` double NOT NULL DEFAULT '0',
  `quantity_received` double NOT NULL DEFAULT '0',
  PRIMARY KEY (`po_detail_item`),
  KEY `order` (`order_no`,`po_detail_item`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_purch_order_details`
--

LOCK TABLES `0_purch_order_details` WRITE;
/*!40000 ALTER TABLE `0_purch_order_details` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_purch_order_details` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_purch_orders`
--

DROP TABLE IF EXISTS `0_purch_orders`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_purch_orders` (
  `order_no` int(11) NOT NULL AUTO_INCREMENT,
  `supplier_id` int(11) NOT NULL DEFAULT '0',
  `comments` tinytext,
  `ord_date` date NOT NULL DEFAULT '0000-00-00',
  `reference` tinytext NOT NULL,
  `requisition_no` tinytext,
  `into_stock_location` varchar(5) NOT NULL DEFAULT '',
  `delivery_address` tinytext NOT NULL,
  `total` double NOT NULL DEFAULT '0',
  `tax_included` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`order_no`),
  KEY `ord_date` (`ord_date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_purch_orders`
--

LOCK TABLES `0_purch_orders` WRITE;
/*!40000 ALTER TABLE `0_purch_orders` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_purch_orders` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_quick_entries`
--

DROP TABLE IF EXISTS `0_quick_entries`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_quick_entries` (
  `id` smallint(6) unsigned NOT NULL AUTO_INCREMENT,
  `type` tinyint(1) NOT NULL DEFAULT '0',
  `description` varchar(60) NOT NULL,
  `base_amount` double NOT NULL DEFAULT '0',
  `base_desc` varchar(60) DEFAULT NULL,
  `bal_type` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `description` (`description`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_quick_entries`
--

LOCK TABLES `0_quick_entries` WRITE;
/*!40000 ALTER TABLE `0_quick_entries` DISABLE KEYS */;
INSERT INTO `0_quick_entries` VALUES (1,1,'Maintenance',0,'Amount',0),(2,1,'Phone',0,'Amount',0),(3,2,'Cash Sales',0,'Amount',0);
/*!40000 ALTER TABLE `0_quick_entries` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_quick_entry_lines`
--

DROP TABLE IF EXISTS `0_quick_entry_lines`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_quick_entry_lines` (
  `id` smallint(6) unsigned NOT NULL AUTO_INCREMENT,
  `qid` smallint(6) unsigned NOT NULL,
  `amount` double DEFAULT '0',
  `action` varchar(2) NOT NULL,
  `dest_id` varchar(15) NOT NULL DEFAULT '',
  `dimension_id` smallint(6) unsigned DEFAULT NULL,
  `dimension2_id` smallint(6) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `qid` (`qid`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_quick_entry_lines`
--

LOCK TABLES `0_quick_entry_lines` WRITE;
/*!40000 ALTER TABLE `0_quick_entry_lines` DISABLE KEYS */;
INSERT INTO `0_quick_entry_lines` VALUES (1,1,0,'=','6600',0,0),(2,2,0,'=','6730',0,0),(3,3,0,'=','3000',0,0);
/*!40000 ALTER TABLE `0_quick_entry_lines` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_recurrent_invoices`
--

DROP TABLE IF EXISTS `0_recurrent_invoices`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_recurrent_invoices` (
  `id` smallint(6) unsigned NOT NULL AUTO_INCREMENT,
  `description` varchar(60) NOT NULL DEFAULT '',
  `order_no` int(11) unsigned NOT NULL,
  `debtor_no` int(11) unsigned DEFAULT NULL,
  `group_no` smallint(6) unsigned DEFAULT NULL,
  `days` int(11) NOT NULL DEFAULT '0',
  `monthly` int(11) NOT NULL DEFAULT '0',
  `begin` date NOT NULL DEFAULT '0000-00-00',
  `end` date NOT NULL DEFAULT '0000-00-00',
  `last_sent` date NOT NULL DEFAULT '0000-00-00',
  PRIMARY KEY (`id`),
  UNIQUE KEY `description` (`description`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_recurrent_invoices`
--

LOCK TABLES `0_recurrent_invoices` WRITE;
/*!40000 ALTER TABLE `0_recurrent_invoices` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_recurrent_invoices` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_refs`
--

DROP TABLE IF EXISTS `0_refs`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_refs` (
  `id` int(11) NOT NULL DEFAULT '0',
  `type` int(11) NOT NULL DEFAULT '0',
  `reference` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`id`,`type`),
  KEY `Type_and_Reference` (`type`,`reference`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_refs`
--

LOCK TABLES `0_refs` WRITE;
/*!40000 ALTER TABLE `0_refs` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_refs` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_sales_order_details`
--

DROP TABLE IF EXISTS `0_sales_order_details`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_sales_order_details` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `order_no` int(11) NOT NULL DEFAULT '0',
  `trans_type` smallint(6) NOT NULL DEFAULT '30',
  `stk_code` varchar(20) NOT NULL DEFAULT '',
  `description` tinytext,
  `qty_sent` double NOT NULL DEFAULT '0',
  `unit_price` double NOT NULL DEFAULT '0',
  `quantity` double NOT NULL DEFAULT '0',
  `discount_percent` double NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `sorder` (`trans_type`,`order_no`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_sales_order_details`
--

LOCK TABLES `0_sales_order_details` WRITE;
/*!40000 ALTER TABLE `0_sales_order_details` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_sales_order_details` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_sales_orders`
--

DROP TABLE IF EXISTS `0_sales_orders`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_sales_orders` (
  `order_no` int(11) NOT NULL,
  `trans_type` smallint(6) NOT NULL DEFAULT '30',
  `version` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `type` tinyint(1) NOT NULL DEFAULT '0',
  `debtor_no` int(11) NOT NULL DEFAULT '0',
  `branch_code` int(11) NOT NULL DEFAULT '0',
  `reference` varchar(100) NOT NULL DEFAULT '',
  `customer_ref` tinytext NOT NULL,
  `comments` tinytext,
  `ord_date` date NOT NULL DEFAULT '0000-00-00',
  `order_type` int(11) NOT NULL DEFAULT '0',
  `ship_via` int(11) NOT NULL DEFAULT '0',
  `delivery_address` tinytext NOT NULL,
  `contact_phone` varchar(30) DEFAULT NULL,
  `contact_email` varchar(100) DEFAULT NULL,
  `deliver_to` tinytext NOT NULL,
  `freight_cost` double NOT NULL DEFAULT '0',
  `from_stk_loc` varchar(5) NOT NULL DEFAULT '',
  `delivery_date` date NOT NULL DEFAULT '0000-00-00',
  `payment_terms` int(11) DEFAULT NULL,
  `total` double NOT NULL DEFAULT '0',
  PRIMARY KEY (`trans_type`,`order_no`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_sales_orders`
--

LOCK TABLES `0_sales_orders` WRITE;
/*!40000 ALTER TABLE `0_sales_orders` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_sales_orders` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_sales_pos`
--

DROP TABLE IF EXISTS `0_sales_pos`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_sales_pos` (
  `id` smallint(6) unsigned NOT NULL AUTO_INCREMENT,
  `pos_name` varchar(30) NOT NULL,
  `cash_sale` tinyint(1) NOT NULL,
  `credit_sale` tinyint(1) NOT NULL,
  `pos_location` varchar(5) NOT NULL,
  `pos_account` smallint(6) unsigned NOT NULL,
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `pos_name` (`pos_name`)
) ENGINE=MyISAM AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_sales_pos`
--

LOCK TABLES `0_sales_pos` WRITE;
/*!40000 ALTER TABLE `0_sales_pos` DISABLE KEYS */;
INSERT INTO `0_sales_pos` VALUES (1,'Default',1,1,'DEF',2,0);
/*!40000 ALTER TABLE `0_sales_pos` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_sales_types`
--

DROP TABLE IF EXISTS `0_sales_types`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_sales_types` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `sales_type` char(50) NOT NULL DEFAULT '',
  `tax_included` int(1) NOT NULL DEFAULT '0',
  `factor` double NOT NULL DEFAULT '1',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `sales_type` (`sales_type`)
) ENGINE=MyISAM AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_sales_types`
--

LOCK TABLES `0_sales_types` WRITE;
/*!40000 ALTER TABLE `0_sales_types` DISABLE KEYS */;
INSERT INTO `0_sales_types` VALUES (1,'Retail',0,1,0),(2,'Wholesale',0,1,0);
/*!40000 ALTER TABLE `0_sales_types` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_salesman`
--

DROP TABLE IF EXISTS `0_salesman`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_salesman` (
  `salesman_code` int(11) NOT NULL AUTO_INCREMENT,
  `salesman_name` char(60) NOT NULL DEFAULT '',
  `salesman_phone` char(30) NOT NULL DEFAULT '',
  `salesman_fax` char(30) NOT NULL DEFAULT '',
  `salesman_email` varchar(100) NOT NULL DEFAULT '',
  `provision` double NOT NULL DEFAULT '0',
  `break_pt` double NOT NULL DEFAULT '0',
  `provision2` double NOT NULL DEFAULT '0',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`salesman_code`),
  UNIQUE KEY `salesman_name` (`salesman_name`)
) ENGINE=MyISAM AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_salesman`
--

LOCK TABLES `0_salesman` WRITE;
/*!40000 ALTER TABLE `0_salesman` DISABLE KEYS */;
INSERT INTO `0_salesman` VALUES (1,'Sales Person','','','',5,1000,4,0);
/*!40000 ALTER TABLE `0_salesman` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_security_roles`
--

DROP TABLE IF EXISTS `0_security_roles`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_security_roles` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `role` varchar(30) NOT NULL,
  `description` varchar(50) DEFAULT NULL,
  `sections` text,
  `areas` text,
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `role` (`role`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_security_roles`
--

LOCK TABLES `0_security_roles` WRITE;
/*!40000 ALTER TABLE `0_security_roles` DISABLE KEYS */;
INSERT INTO `0_security_roles` VALUES (1,'FA 2.1 Inquiries','Inquiries','768;2816;3072;3328;5632;5888;8192;8448;10752;11008;13312;15872;16128','773;774;2818;2822;3073;3073;3075;3076;3077;3081;3329;3330;3330;3330;3331;3331;3332;3333;3334;3335;5633;5633;5640;5889;5891;8193;8194;8194;8450;8451;10753;11009;11010;11012;13313;13315;15873;15882;16129;16130;16131;16132',0),(2,'FA 2.1 Accountant','Accountant','512;768;2816;3072;3328;5376;5632;5888;7936;8192;8448;10496;10752;11008;13312;15616;15872;16128','513;519;520;521;522;523;524;525;769;771;772;773;774;2817;2818;2819;2820;2821;2822;2823;3073;3073;3074;3075;3076;3077;3078;3079;3080;3081;3081;3329;3330;3330;3330;3331;3331;3332;3333;3334;3335;5377;5633;5633;5634;5635;5636;5637;5638;5639;5640;5640;5889;5891;7937;7938;7939;7940;8193;8194;8194;8195;8196;8197;8449;8450;8451;10497;10753;10753;10754;10755;10756;10757;11009;11010;11010;11012;13313;13313;13314;13315;15617;15618;15619;15620;15621;15622;15623;15624;15624;15625;15626;15873;15873;15874;15875;15876;15877;15878;15879;15880;15881;15882;16129;16129;16130;16130;16131;16132',0),(3,'FA 2.1 System Administrator','System Administrator','256;512;768;2816;3072;3328;5376;5632;5888;7936;8192;8448;10496;10752;11008;13056;13312;15616;15872;16128','257;258;259;260;513;514;515;516;517;518;519;520;521;522;523;524;525;769;770;771;772;773;774;2817;2818;2819;2820;2821;2822;2823;3073;3073;3074;3075;3076;3077;3078;3079;3080;3081;3081;3329;3330;3330;3330;3331;3331;3332;3333;3334;3335;5377;5633;5633;5634;5635;5636;5637;5638;5639;5640;5640;5889;5891;7937;7938;7939;7940;8193;8194;8194;8195;8196;8197;8449;8450;8451;10497;10753;10753;10754;10755;10756;10757;11009;11010;11010;11012;13057;13313;13313;13314;13315;15617;15618;15619;15620;15621;15622;15623;15624;15624;15625;15626;15627;15873;15873;15874;15875;15876;15877;15878;15879;15880;15881;15882;16129;16129;16130;16130;16131;16132',0);
/*!40000 ALTER TABLE `0_security_roles` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_shippers`
--

DROP TABLE IF EXISTS `0_shippers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_shippers` (
  `shipper_id` int(11) NOT NULL AUTO_INCREMENT,
  `shipper_name` varchar(60) NOT NULL DEFAULT '',
  `phone` varchar(30) NOT NULL DEFAULT '',
  `phone2` varchar(30) NOT NULL DEFAULT '',
  `contact` tinytext NOT NULL,
  `address` tinytext NOT NULL,
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`shipper_id`),
  UNIQUE KEY `name` (`shipper_name`)
) ENGINE=MyISAM AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_shippers`
--

LOCK TABLES `0_shippers` WRITE;
/*!40000 ALTER TABLE `0_shippers` DISABLE KEYS */;
INSERT INTO `0_shippers` VALUES (1,'Default','','','','',0);
/*!40000 ALTER TABLE `0_shippers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_sql_trail`
--

DROP TABLE IF EXISTS `0_sql_trail`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_sql_trail` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `sql` text NOT NULL,
  `result` tinyint(1) NOT NULL,
  `msg` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_sql_trail`
--

LOCK TABLES `0_sql_trail` WRITE;
/*!40000 ALTER TABLE `0_sql_trail` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_sql_trail` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_stock_category`
--

DROP TABLE IF EXISTS `0_stock_category`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_stock_category` (
  `category_id` int(11) NOT NULL AUTO_INCREMENT,
  `description` varchar(60) NOT NULL DEFAULT '',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  `dflt_tax_type` int(11) NOT NULL DEFAULT '1',
  `dflt_units` varchar(20) NOT NULL DEFAULT 'each',
  `dflt_mb_flag` char(1) NOT NULL DEFAULT 'B',
  `dflt_sales_act` varchar(15) NOT NULL DEFAULT '',
  `dflt_cogs_act` varchar(15) NOT NULL DEFAULT '',
  `dflt_inventory_act` varchar(15) NOT NULL DEFAULT '',
  `dflt_adjustment_act` varchar(15) NOT NULL DEFAULT '',
  `dflt_assembly_act` varchar(15) NOT NULL DEFAULT '',
  `dflt_dim1` int(11) DEFAULT NULL,
  `dflt_dim2` int(11) DEFAULT NULL,
  `dflt_no_sale` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`category_id`),
  UNIQUE KEY `description` (`description`)
) ENGINE=MyISAM AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_stock_category`
--

LOCK TABLES `0_stock_category` WRITE;
/*!40000 ALTER TABLE `0_stock_category` DISABLE KEYS */;
INSERT INTO `0_stock_category` VALUES (1,'Components',0,1,'each','B','4000','5000','1001','1001','1001',0,0,0),(2,'Charges',0,1,'each','B','4000','5000','1001','1001','1001',0,0,0),(3,'Systems',0,1,'each','B','4000','5000','1001','1001','1001',0,0,0),(4,'Services',0,1,'each','B','4000','5000','1001','1001','1001',0,0,0);
/*!40000 ALTER TABLE `0_stock_category` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_stock_master`
--

DROP TABLE IF EXISTS `0_stock_master`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_stock_master` (
  `stock_id` varchar(20) NOT NULL DEFAULT '',
  `category_id` int(11) NOT NULL DEFAULT '0',
  `tax_type_id` int(11) NOT NULL DEFAULT '0',
  `description` varchar(200) NOT NULL DEFAULT '',
  `long_description` tinytext NOT NULL,
  `units` varchar(20) NOT NULL DEFAULT 'each',
  `mb_flag` char(1) NOT NULL DEFAULT 'B',
  `sales_account` varchar(15) NOT NULL DEFAULT '',
  `cogs_account` varchar(15) NOT NULL DEFAULT '',
  `inventory_account` varchar(15) NOT NULL DEFAULT '',
  `adjustment_account` varchar(15) NOT NULL DEFAULT '',
  `assembly_account` varchar(15) NOT NULL DEFAULT '',
  `dimension_id` int(11) DEFAULT NULL,
  `dimension2_id` int(11) DEFAULT NULL,
  `actual_cost` double NOT NULL DEFAULT '0',
  `last_cost` double NOT NULL DEFAULT '0',
  `material_cost` double NOT NULL DEFAULT '0',
  `labour_cost` double NOT NULL DEFAULT '0',
  `overhead_cost` double NOT NULL DEFAULT '0',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  `no_sale` tinyint(1) NOT NULL DEFAULT '0',
  `editable` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`stock_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_stock_master`
--

LOCK TABLES `0_stock_master` WRITE;
/*!40000 ALTER TABLE `0_stock_master` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_stock_master` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_stock_moves`
--

DROP TABLE IF EXISTS `0_stock_moves`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_stock_moves` (
  `trans_id` int(11) NOT NULL AUTO_INCREMENT,
  `trans_no` int(11) NOT NULL DEFAULT '0',
  `stock_id` char(20) NOT NULL DEFAULT '',
  `type` smallint(6) NOT NULL DEFAULT '0',
  `loc_code` char(5) NOT NULL DEFAULT '',
  `tran_date` date NOT NULL DEFAULT '0000-00-00',
  `person_id` int(11) DEFAULT NULL,
  `price` double NOT NULL DEFAULT '0',
  `reference` char(40) NOT NULL DEFAULT '',
  `qty` double NOT NULL DEFAULT '1',
  `discount_percent` double NOT NULL DEFAULT '0',
  `standard_cost` double NOT NULL DEFAULT '0',
  `visible` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`trans_id`),
  KEY `type` (`type`,`trans_no`),
  KEY `Move` (`stock_id`,`loc_code`,`tran_date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_stock_moves`
--

LOCK TABLES `0_stock_moves` WRITE;
/*!40000 ALTER TABLE `0_stock_moves` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_stock_moves` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_supp_allocations`
--

DROP TABLE IF EXISTS `0_supp_allocations`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_supp_allocations` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `amt` double unsigned DEFAULT NULL,
  `date_alloc` date NOT NULL DEFAULT '0000-00-00',
  `trans_no_from` int(11) DEFAULT NULL,
  `trans_type_from` int(11) DEFAULT NULL,
  `trans_no_to` int(11) DEFAULT NULL,
  `trans_type_to` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `From` (`trans_type_from`,`trans_no_from`),
  KEY `To` (`trans_type_to`,`trans_no_to`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_supp_allocations`
--

LOCK TABLES `0_supp_allocations` WRITE;
/*!40000 ALTER TABLE `0_supp_allocations` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_supp_allocations` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_supp_invoice_items`
--

DROP TABLE IF EXISTS `0_supp_invoice_items`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_supp_invoice_items` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `supp_trans_no` int(11) DEFAULT NULL,
  `supp_trans_type` int(11) DEFAULT NULL,
  `gl_code` varchar(15) NOT NULL DEFAULT '',
  `grn_item_id` int(11) DEFAULT NULL,
  `po_detail_item_id` int(11) DEFAULT NULL,
  `stock_id` varchar(20) NOT NULL DEFAULT '',
  `description` tinytext,
  `quantity` double NOT NULL DEFAULT '0',
  `unit_price` double NOT NULL DEFAULT '0',
  `unit_tax` double NOT NULL DEFAULT '0',
  `memo_` tinytext,
  PRIMARY KEY (`id`),
  KEY `Transaction` (`supp_trans_type`,`supp_trans_no`,`stock_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_supp_invoice_items`
--

LOCK TABLES `0_supp_invoice_items` WRITE;
/*!40000 ALTER TABLE `0_supp_invoice_items` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_supp_invoice_items` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_supp_trans`
--

DROP TABLE IF EXISTS `0_supp_trans`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_supp_trans` (
  `trans_no` int(11) unsigned NOT NULL DEFAULT '0',
  `type` smallint(6) unsigned NOT NULL DEFAULT '0',
  `supplier_id` int(11) unsigned DEFAULT NULL,
  `reference` tinytext NOT NULL,
  `supp_reference` varchar(60) NOT NULL DEFAULT '',
  `tran_date` date NOT NULL DEFAULT '0000-00-00',
  `due_date` date NOT NULL DEFAULT '0000-00-00',
  `ov_amount` double NOT NULL DEFAULT '0',
  `ov_discount` double NOT NULL DEFAULT '0',
  `ov_gst` double NOT NULL DEFAULT '0',
  `rate` double NOT NULL DEFAULT '1',
  `alloc` double NOT NULL DEFAULT '0',
  `tax_included` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`type`,`trans_no`),
  KEY `supplier_id` (`supplier_id`),
  KEY `SupplierID_2` (`supplier_id`,`supp_reference`),
  KEY `type` (`type`),
  KEY `tran_date` (`tran_date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_supp_trans`
--

LOCK TABLES `0_supp_trans` WRITE;
/*!40000 ALTER TABLE `0_supp_trans` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_supp_trans` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_suppliers`
--

DROP TABLE IF EXISTS `0_suppliers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_suppliers` (
  `supplier_id` int(11) NOT NULL AUTO_INCREMENT,
  `supp_name` varchar(60) NOT NULL DEFAULT '',
  `address` tinytext NOT NULL,
  `supp_address` tinytext NOT NULL,
  `gst_no` varchar(25) NOT NULL DEFAULT '',
  `contact` varchar(60) NOT NULL DEFAULT '',
  `supp_account_no` varchar(40) NOT NULL DEFAULT '',
  `website` varchar(100) NOT NULL DEFAULT '',
  `bank_account` varchar(60) NOT NULL DEFAULT '',
  `curr_code` char(3) DEFAULT NULL,
  `payment_terms` int(11) DEFAULT NULL,
  `tax_included` tinyint(1) NOT NULL DEFAULT '0',
  `dimension_id` int(11) DEFAULT '0',
  `dimension2_id` int(11) DEFAULT '0',
  `tax_group_id` int(11) DEFAULT NULL,
  `credit_limit` double NOT NULL DEFAULT '0',
  `purchase_account` varchar(15) NOT NULL DEFAULT '',
  `payable_account` varchar(15) NOT NULL DEFAULT '',
  `payment_discount_account` varchar(15) NOT NULL DEFAULT '',
  `notes` tinytext NOT NULL,
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  `supp_ref` varchar(30) NOT NULL,
  PRIMARY KEY (`supplier_id`),
  KEY `supp_ref` (`supp_ref`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_suppliers`
--

LOCK TABLES `0_suppliers` WRITE;
/*!40000 ALTER TABLE `0_suppliers` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_suppliers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_sys_prefs`
--

DROP TABLE IF EXISTS `0_sys_prefs`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_sys_prefs` (
  `name` varchar(35) NOT NULL DEFAULT '',
  `category` varchar(30) DEFAULT NULL,
  `type` varchar(20) NOT NULL DEFAULT '',
  `length` smallint(6) DEFAULT NULL,
  `value` tinytext,
  PRIMARY KEY (`name`),
  KEY `category` (`category`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_sys_prefs`
--

LOCK TABLES `0_sys_prefs` WRITE;
/*!40000 ALTER TABLE `0_sys_prefs` DISABLE KEYS */;
INSERT INTO `0_sys_prefs` VALUES ('coy_name','setup.company','varchar',60,'Fames Test'),('gst_no','setup.company','varchar',25,NULL),('coy_no','setup.company','varchar',25,NULL),('tax_prd','setup.company','int',11,'1'),('tax_last','setup.company','int',11,'1'),('postal_address','setup.company','tinytext',0,'N/A'),('phone','setup.company','varchar',30,NULL),('fax','setup.company','varchar',30,NULL),('email','setup.company','varchar',100,NULL),('coy_logo','setup.company','varchar',100,NULL),('domicile','setup.company','varchar',55,NULL),('curr_default','setup.company','char',3,'GBP'),('use_dimension','setup.company','tinyint',1,'1'),('f_year','setup.company','int',11,'1'),('no_item_list','setup.company','tinyint',1,'0'),('no_customer_list','setup.company','tinyint',1,'0'),('no_supplier_list','setup.company','tinyint',1,'0'),('base_sales','setup.company','int',11,'-1'),('time_zone','setup.company','tinyint',1,'0'),('add_pct','setup.company','int',5,'-1'),('round_to','setup.company','int',5,'1'),('login_tout','setup.company','smallint',6,'600'),('past_due_days','glsetup.general','int',11,'30'),('profit_loss_year_act','glsetup.general','varchar',15,'8601'),('retained_earnings_act','glsetup.general','varchar',15,'3200'),('bank_charge_act','glsetup.general','varchar',15,'7901'),('exchange_diff_act','glsetup.general','varchar',15,'4920'),('default_credit_limit','glsetup.customer','int',11,'1000'),('accumulate_shipping','glsetup.customer','tinyint',1,'0'),('legal_text','glsetup.customer','tinytext',0,NULL),('freight_act','glsetup.customer','varchar',15,'4910'),('debtors_act','glsetup.sales','varchar',15,'1100'),('default_sales_act','glsetup.sales','varchar',15,'4000'),('default_sales_discount_act','glsetup.sales','varchar',15,'4000'),('default_prompt_payment_act','glsetup.sales','varchar',15,'4000'),('default_delivery_required','glsetup.sales','smallint',6,'1'),('default_dim_required','glsetup.dims','int',11,'20'),('pyt_discount_act','glsetup.purchase','varchar',15,'5000'),('creditors_act','glsetup.purchase','varchar',15,'2100'),('po_over_receive','glsetup.purchase','int',11,'10'),('po_over_charge','glsetup.purchase','int',11,'10'),('allow_negative_stock','glsetup.inventory','tinyint',1,'0'),('default_inventory_act','glsetup.items','varchar',15,'1001'),('default_cogs_act','glsetup.items','varchar',15,'5000'),('default_adj_act','glsetup.items','varchar',15,'1001'),('default_inv_sales_act','glsetup.items','varchar',15,'4000'),('default_assembly_act','glsetup.items','varchar',15,'1001'),('default_workorder_required','glsetup.manuf','int',11,'20'),('version_id','system','varchar',11,'2.3rc'),('auto_curr_reval','setup.company','smallint',6,'1');
/*!40000 ALTER TABLE `0_sys_prefs` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_sys_types`
--

DROP TABLE IF EXISTS `0_sys_types`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_sys_types` (
  `type_id` smallint(6) NOT NULL DEFAULT '0',
  `type_no` int(11) NOT NULL DEFAULT '1',
  `next_reference` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`type_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_sys_types`
--

LOCK TABLES `0_sys_types` WRITE;
/*!40000 ALTER TABLE `0_sys_types` DISABLE KEYS */;
INSERT INTO `0_sys_types` VALUES (0,17,'1'),(1,7,'1'),(2,4,'1'),(4,3,'1'),(10,16,'1'),(11,2,'1'),(12,6,'1'),(13,1,'1'),(16,2,'1'),(17,2,'1'),(18,1,'1'),(20,6,'1'),(21,1,'1'),(22,3,'1'),(25,1,'1'),(26,1,'1'),(28,1,'1'),(29,1,'1'),(30,0,'1'),(32,0,'1'),(35,1,'1'),(40,1,'1');
/*!40000 ALTER TABLE `0_sys_types` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_tag_associations`
--

DROP TABLE IF EXISTS `0_tag_associations`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_tag_associations` (
  `record_id` varchar(15) NOT NULL,
  `tag_id` int(11) NOT NULL,
  UNIQUE KEY `record_id` (`record_id`,`tag_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_tag_associations`
--

LOCK TABLES `0_tag_associations` WRITE;
/*!40000 ALTER TABLE `0_tag_associations` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_tag_associations` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_tags`
--

DROP TABLE IF EXISTS `0_tags`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_tags` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `type` smallint(6) NOT NULL,
  `name` varchar(30) NOT NULL,
  `description` varchar(60) DEFAULT NULL,
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `type` (`type`,`name`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_tags`
--

LOCK TABLES `0_tags` WRITE;
/*!40000 ALTER TABLE `0_tags` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_tags` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_tax_group_items`
--

DROP TABLE IF EXISTS `0_tax_group_items`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_tax_group_items` (
  `tax_group_id` int(11) NOT NULL DEFAULT '0',
  `tax_type_id` int(11) NOT NULL DEFAULT '0',
  `rate` double NOT NULL DEFAULT '0',
  PRIMARY KEY (`tax_group_id`,`tax_type_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_tax_group_items`
--

LOCK TABLES `0_tax_group_items` WRITE;
/*!40000 ALTER TABLE `0_tax_group_items` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_tax_group_items` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_tax_groups`
--

DROP TABLE IF EXISTS `0_tax_groups`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_tax_groups` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(60) NOT NULL DEFAULT '',
  `tax_shipping` tinyint(1) NOT NULL DEFAULT '0',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_tax_groups`
--

LOCK TABLES `0_tax_groups` WRITE;
/*!40000 ALTER TABLE `0_tax_groups` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_tax_groups` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_tax_types`
--

DROP TABLE IF EXISTS `0_tax_types`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_tax_types` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `rate` double NOT NULL DEFAULT '0',
  `sales_gl_code` varchar(15) NOT NULL DEFAULT '',
  `purchasing_gl_code` varchar(15) NOT NULL DEFAULT '',
  `name` varchar(60) NOT NULL DEFAULT '',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`,`rate`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_tax_types`
--

LOCK TABLES `0_tax_types` WRITE;
/*!40000 ALTER TABLE `0_tax_types` DISABLE KEYS */;
INSERT INTO `0_tax_types` VALUES (1,17.5,'2200','2200','VAT',0),(2,5,'2205','2205','VAT',0);
/*!40000 ALTER TABLE `0_tax_types` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_trans_tax_details`
--

DROP TABLE IF EXISTS `0_trans_tax_details`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_trans_tax_details` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `trans_type` smallint(6) DEFAULT NULL,
  `trans_no` int(11) DEFAULT NULL,
  `tran_date` date NOT NULL,
  `tax_type_id` int(11) NOT NULL DEFAULT '0',
  `rate` double NOT NULL DEFAULT '0',
  `ex_rate` double NOT NULL DEFAULT '1',
  `included_in_price` tinyint(1) NOT NULL DEFAULT '0',
  `net_amount` double NOT NULL DEFAULT '0',
  `amount` double NOT NULL DEFAULT '0',
  `memo` tinytext,
  PRIMARY KEY (`id`),
  KEY `Type_and_Number` (`trans_type`,`trans_no`),
  KEY `tran_date` (`tran_date`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_trans_tax_details`
--

LOCK TABLES `0_trans_tax_details` WRITE;
/*!40000 ALTER TABLE `0_trans_tax_details` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_trans_tax_details` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_useronline`
--

DROP TABLE IF EXISTS `0_useronline`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_useronline` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `timestamp` int(15) NOT NULL DEFAULT '0',
  `ip` varchar(40) NOT NULL DEFAULT '',
  `file` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`id`),
  KEY `timestamp` (`timestamp`),
  KEY `ip` (`ip`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_useronline`
--

LOCK TABLES `0_useronline` WRITE;
/*!40000 ALTER TABLE `0_useronline` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_useronline` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_users`
--

DROP TABLE IF EXISTS `0_users`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_users` (
  `id` smallint(6) NOT NULL AUTO_INCREMENT,
  `user_id` varchar(60) NOT NULL DEFAULT '',
  `password` varchar(100) NOT NULL DEFAULT '',
  `real_name` varchar(100) NOT NULL DEFAULT '',
  `role_id` int(11) NOT NULL DEFAULT '1',
  `phone` varchar(30) NOT NULL DEFAULT '',
  `email` varchar(100) DEFAULT NULL,
  `language` varchar(20) DEFAULT NULL,
  `date_format` tinyint(1) NOT NULL DEFAULT '0',
  `date_sep` tinyint(1) NOT NULL DEFAULT '0',
  `tho_sep` tinyint(1) NOT NULL DEFAULT '0',
  `dec_sep` tinyint(1) NOT NULL DEFAULT '0',
  `theme` varchar(20) NOT NULL DEFAULT 'default',
  `page_size` varchar(20) NOT NULL DEFAULT 'A4',
  `prices_dec` smallint(6) NOT NULL DEFAULT '2',
  `qty_dec` smallint(6) NOT NULL DEFAULT '2',
  `rates_dec` smallint(6) NOT NULL DEFAULT '4',
  `percent_dec` smallint(6) NOT NULL DEFAULT '1',
  `show_gl` tinyint(1) NOT NULL DEFAULT '1',
  `show_codes` tinyint(1) NOT NULL DEFAULT '0',
  `show_hints` tinyint(1) NOT NULL DEFAULT '0',
  `last_visit_date` datetime DEFAULT NULL,
  `query_size` tinyint(1) DEFAULT '10',
  `graphic_links` tinyint(1) DEFAULT '1',
  `pos` smallint(6) DEFAULT '1',
  `print_profile` varchar(30) NOT NULL DEFAULT '1',
  `rep_popup` tinyint(1) DEFAULT '1',
  `sticky_doc_date` tinyint(1) DEFAULT '0',
  `startup_tab` varchar(20) NOT NULL DEFAULT 'orders',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `user_id` (`user_id`)
) ENGINE=MyISAM AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_users`
--

LOCK TABLES `0_users` WRITE;
/*!40000 ALTER TABLE `0_users` DISABLE KEYS */;
INSERT INTO `0_users` VALUES (1,'admin','ef6a96105801ff6c1e5da22397cd528f','Administrator',3,'','adm@adm.com','',1,0,0,0,'default','A4',2,2,4,1,1,0,0,'2008-04-04 12:34:29',10,1,1,'1',1,0,'orders',0);
/*!40000 ALTER TABLE `0_users` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_voided`
--

DROP TABLE IF EXISTS `0_voided`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_voided` (
  `type` int(11) NOT NULL DEFAULT '0',
  `id` int(11) NOT NULL DEFAULT '0',
  `date_` date NOT NULL DEFAULT '0000-00-00',
  `memo_` tinytext NOT NULL,
  UNIQUE KEY `id` (`type`,`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_voided`
--

LOCK TABLES `0_voided` WRITE;
/*!40000 ALTER TABLE `0_voided` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_voided` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_wo_issue_items`
--

DROP TABLE IF EXISTS `0_wo_issue_items`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_wo_issue_items` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `stock_id` varchar(40) DEFAULT NULL,
  `issue_id` int(11) DEFAULT NULL,
  `qty_issued` double DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_wo_issue_items`
--

LOCK TABLES `0_wo_issue_items` WRITE;
/*!40000 ALTER TABLE `0_wo_issue_items` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_wo_issue_items` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_wo_issues`
--

DROP TABLE IF EXISTS `0_wo_issues`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_wo_issues` (
  `issue_no` int(11) NOT NULL AUTO_INCREMENT,
  `workorder_id` int(11) NOT NULL DEFAULT '0',
  `reference` varchar(100) DEFAULT NULL,
  `issue_date` date DEFAULT NULL,
  `loc_code` varchar(5) DEFAULT NULL,
  `workcentre_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`issue_no`),
  KEY `workorder_id` (`workorder_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_wo_issues`
--

LOCK TABLES `0_wo_issues` WRITE;
/*!40000 ALTER TABLE `0_wo_issues` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_wo_issues` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_wo_manufacture`
--

DROP TABLE IF EXISTS `0_wo_manufacture`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_wo_manufacture` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `reference` varchar(100) DEFAULT NULL,
  `workorder_id` int(11) NOT NULL DEFAULT '0',
  `quantity` double NOT NULL DEFAULT '0',
  `date_` date NOT NULL DEFAULT '0000-00-00',
  PRIMARY KEY (`id`),
  KEY `workorder_id` (`workorder_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_wo_manufacture`
--

LOCK TABLES `0_wo_manufacture` WRITE;
/*!40000 ALTER TABLE `0_wo_manufacture` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_wo_manufacture` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_wo_requirements`
--

DROP TABLE IF EXISTS `0_wo_requirements`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_wo_requirements` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `workorder_id` int(11) NOT NULL DEFAULT '0',
  `stock_id` char(20) NOT NULL DEFAULT '',
  `workcentre` int(11) NOT NULL DEFAULT '0',
  `units_req` double NOT NULL DEFAULT '1',
  `std_cost` double NOT NULL DEFAULT '0',
  `loc_code` char(5) NOT NULL DEFAULT '',
  `units_issued` double NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `workorder_id` (`workorder_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_wo_requirements`
--

LOCK TABLES `0_wo_requirements` WRITE;
/*!40000 ALTER TABLE `0_wo_requirements` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_wo_requirements` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_workcentres`
--

DROP TABLE IF EXISTS `0_workcentres`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_workcentres` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` char(40) NOT NULL DEFAULT '',
  `description` char(50) NOT NULL DEFAULT '',
  `inactive` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_workcentres`
--

LOCK TABLES `0_workcentres` WRITE;
/*!40000 ALTER TABLE `0_workcentres` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_workcentres` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `0_workorders`
--

DROP TABLE IF EXISTS `0_workorders`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `0_workorders` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `wo_ref` varchar(60) NOT NULL DEFAULT '',
  `loc_code` varchar(5) NOT NULL DEFAULT '',
  `units_reqd` double NOT NULL DEFAULT '1',
  `stock_id` varchar(20) NOT NULL DEFAULT '',
  `date_` date NOT NULL DEFAULT '0000-00-00',
  `type` tinyint(4) NOT NULL DEFAULT '0',
  `required_by` date NOT NULL DEFAULT '0000-00-00',
  `released_date` date NOT NULL DEFAULT '0000-00-00',
  `units_issued` double NOT NULL DEFAULT '0',
  `closed` tinyint(1) NOT NULL DEFAULT '0',
  `released` tinyint(1) NOT NULL DEFAULT '0',
  `additional_costs` double NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `wo_ref` (`wo_ref`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `0_workorders`
--

LOCK TABLES `0_workorders` WRITE;
/*!40000 ALTER TABLE `0_workorders` DISABLE KEYS */;
/*!40000 ALTER TABLE `0_workorders` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2016-06-10 16:55:11
