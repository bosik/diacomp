delimiter $$

CREATE DATABASE `compensation` /*!40100 DEFAULT CHARACTER SET cp1251 */$$

delimiter $$

CREATE TABLE `diary2` (
  `_GUID` char(32) CHARACTER SET armscii8 NOT NULL,
  `_UserID` int(10) unsigned NOT NULL,
  `_TimeStamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `_Version` int(11) NOT NULL,
  `_Deleted` int(1) DEFAULT '0',
  `_Content` text,
  `_TimeCache` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`_GUID`)
) ENGINE=MyISAM AUTO_INCREMENT=1526 DEFAULT CHARSET=utf8$$


delimiter $$

CREATE TABLE `dishbase2` (
  `_GUID` char(32) NOT NULL,
  `_UserID` int(11) NOT NULL,
  `_TimeStamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `_Version` int(11) NOT NULL,
  `_Deleted` int(1) NOT NULL DEFAULT '0',
  `_Content` text NOT NULL,
  `_NameCache` varchar(100) NOT NULL,
  PRIMARY KEY (`_GUID`),
  UNIQUE KEY `_GUID_UNIQUE` (`_GUID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8$$


delimiter $$

CREATE TABLE `foodbase2` (
  `_GUID` char(32) NOT NULL,
  `_UserID` int(11) NOT NULL,
  `_TimeStamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `_Version` int(11) NOT NULL,
  `_Deleted` int(1) NOT NULL DEFAULT '0',
  `_Content` text NOT NULL,
  `_NameCache` varchar(100) NOT NULL,
  PRIMARY KEY (`_GUID`),
  UNIQUE KEY `_GUID_UNIQUE` (`_GUID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8$$


delimiter $$

CREATE TABLE `user` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Login` varchar(40) CHARACTER SET cp1251 NOT NULL,
  `HashPass` char(32) CHARACTER SET cp1251 COLLATE cp1251_general_cs NOT NULL,
  `DateReg` datetime DEFAULT NULL,
  `DateLogin` datetime DEFAULT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=MyISAM AUTO_INCREMENT=7 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci$$


INSERT INTO `user`
(`ID`,
`Login`,
`HashPass`,
`DateReg`,
`DateLogin`)
VALUES
(
1,
"bosik-007@narod.ru",
"4761fe12f59bb38d3faca218bcffdaa3",
"2012-01-05 14:41:01",
"2014-02-25 20:10:34"
);