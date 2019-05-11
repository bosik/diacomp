delimiter $$

CREATE DATABASE `compensation` /*!40100 DEFAULT CHARACTER SET utf8 */$$

CREATE TABLE `diary2` (
  `_GUID` char(32) COLLATE utf8_unicode_ci NOT NULL,
  `_UserID` int(10) unsigned NOT NULL,
  `_Hash` char(32) COLLATE utf8_unicode_ci NOT NULL,
  `_TimeStamp` datetime NOT NULL,
  `_Version` int(11) NOT NULL,
  `_Deleted` int(1) DEFAULT '0',
  `_Content` text COLLATE utf8_unicode_ci,
  `_TimeCache` datetime NOT NULL,
  PRIMARY KEY (`_GUID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci$$

CREATE TABLE `dishbase2` (
  `_GUID` char(32) NOT NULL,
  `_UserID` int(11) NOT NULL,
  `_Hash` char(32) NOT NULL,
  `_TimeStamp` datetime NOT NULL,
  `_Version` int(11) NOT NULL,
  `_Deleted` int(1) NOT NULL DEFAULT '0',
  `_Content` text NOT NULL,
  `_NameCache` varchar(100) NOT NULL,
  PRIMARY KEY (`_GUID`),
  UNIQUE KEY `_GUID_UNIQUE` (`_GUID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8$$

CREATE TABLE `foodbase2` (
  `_GUID` char(32) NOT NULL,
  `_UserID` int(11) NOT NULL,
  `_Hash` char(32) NOT NULL,
  `_TimeStamp` datetime NOT NULL,
  `_Version` int(11) NOT NULL,
  `_Deleted` int(1) NOT NULL DEFAULT '0',
  `_Content` text NOT NULL,
  `_NameCache` varchar(100) NOT NULL,
  PRIMARY KEY (`_GUID`,`_UserID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8$$

CREATE TABLE `foodset` (
  `_ID` char(32) NOT NULL,
  `_Description` varchar(50) NOT NULL,
  `_Size` int(11) NOT NULL,
  `_Data` mediumtext NOT NULL,
  PRIMARY KEY (`_ID`),
  UNIQUE KEY `_ID_UNIQUE` (`_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8$$

CREATE TABLE `preferences` (
  `_UserID` int(10) unsigned NOT NULL,
  `_Key` char(32) COLLATE utf8_unicode_ci NOT NULL,
  `_Value` varchar(1024) COLLATE utf8_unicode_ci DEFAULT NULL,
  `_Version` int(11) unsigned NOT NULL,
  PRIMARY KEY (`_UserID`,`_Key`),
  KEY `user-key` (`_UserID`,`_Key`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci COMMENT='Stores user preferences as versioned key-value pairs'$$

CREATE TABLE `user` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Login` varchar(50) COLLATE utf8_unicode_ci NOT NULL,
  `HashPass` char(70) NOT NULL,
  `ActivationKey` char(64) DEFAULT NULL,
  `RestoreKey` char(64) COLLATE utf8_unicode_ci DEFAULT NULL,
  `DateReg` datetime NOT NULL,
  `DateLogin` datetime DEFAULT NULL,
  PRIMARY KEY (`ID`),
  UNIQUE KEY `Login_UNIQUE` (`Login`),
  UNIQUE KEY `ActivationKey_UNIQUE` (`ActivationKey`),
  UNIQUE KEY `RestoreKey_UNIQUE` (`RestoreKey`)
) ENGINE=InnoDB AUTO_INCREMENT=790 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci$$
