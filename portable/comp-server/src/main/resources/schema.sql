delimiter $$

CREATE DATABASE `compensation` /*!40100 DEFAULT CHARACTER SET utf8 */$$

CREATE TABLE `diary2` (
  `_GUID` char(32) NOT NULL,
  `_UserID` int(10) unsigned NOT NULL,
  `_Hash` char(32) NOT NULL,
  `_TimeStamp` datetime NOT NULL,
  `_Version` int(11) NOT NULL,
  `_Deleted` int(1) DEFAULT '0',
  `_Content` text,
  `_TimeCache` datetime NOT NULL,
  PRIMARY KEY (`_GUID`)
) ENGINE=InnoDB AUTO_INCREMENT=1526 DEFAULT CHARSET=utf8$$

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
  PRIMARY KEY (`_GUID`),
  UNIQUE KEY `_GUID_UNIQUE` (`_GUID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8$$

CREATE TABLE `user` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Login` varchar(40) COLLATE utf8_unicode_ci NOT NULL,
  `HashPass` char(70) NOT NULL,
  `ActivationKey` char(64) DEFAULT NULL,
  `DateReg` datetime NOT NULL,
  `DateLogin` datetime DEFAULT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci$$

INSERT INTO `user` (
  `ID`,
  `Login`,
  `HashPass`,
  `DateReg`,
  `DateLogin`)
VALUES (
  1,
  "developer",
  "1024:13a65e564d9d854245331816d8d8eaac:069859f6562795f6b8527dec06bf2e64",
  "2012-01-05 14:41:01",
  "2014-02-25 20:10:34"
);
