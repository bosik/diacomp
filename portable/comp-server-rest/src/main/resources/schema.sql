delimiter $$

CREATE DATABASE `compensation` /*!40100 DEFAULT CHARACTER SET utf8 */$$

CREATE TABLE `user` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `login` varchar(50) NOT NULL,
  `hash_pass` char(70) NOT NULL,
  `activation_key` char(64) DEFAULT NULL,
  `restore_key` char(64) DEFAULT NULL,
  `date_sign_up` datetime DEFAULT NULL,
  `date_sign_in` datetime DEFAULT NULL,
  `date_deleted` datetime DEFAULT NULL,
  `login_deleted` varchar(50) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `Login_UNIQUE` (`login`),
  UNIQUE KEY `ActivationKey_UNIQUE` (`activation_key`),
  UNIQUE KEY `RestoreKey_UNIQUE` (`restore_key`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8$$

CREATE TABLE `diary` (
  `id` char(32) NOT NULL,
  `user_id` int(11) unsigned NOT NULL,
  `time_stamp` datetime NOT NULL,
  `hash` char(32) NOT NULL,
  `version` int(11) NOT NULL,
  `deleted` bit(1) NOT NULL,
  `content` text COLLATE utf8_unicode_ci,
  `time_cache` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_DIARY_USER_idx` (`user_id`),
  CONSTRAINT `FK_DIARY_USER` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8$$

CREATE TABLE `dish` (
  `id` char(32) NOT NULL,
  `user_id` int(11) unsigned NOT NULL,
  `time_stamp` datetime NOT NULL,
  `hash` char(32) NOT NULL,
  `version` int(11) NOT NULL,
  `deleted` bit(1) NOT NULL,
  `content` text NOT NULL,
  `name_cache` varchar(100) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_DISH_USER_idx` (`user_id`),
  CONSTRAINT `FK_DISH_USER` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8$$

CREATE TABLE `food_common` (
  `id` char(32) NOT NULL,
  `name` varchar(100) NOT NULL,
  `prots` decimal(6,2) unsigned NOT NULL,
  `fats` decimal(6,2) unsigned NOT NULL,
  `carbs` decimal(6,2) unsigned NOT NULL,
  `value` decimal(6,2) unsigned NOT NULL,
  `from_table` bit(1) NOT NULL,
  `deleted` bit(1) NOT NULL,
  `last_modified` datetime NOT NULL,
  `hash` char(32) NOT NULL,
  `version` int(11) unsigned NOT NULL,
  `tag` varchar(50) DEFAULT NULL COMMENT 'Origin food set name (optional)',
  PRIMARY KEY (`id`),
  KEY `LAST_MODIFIED` (`last_modified`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='Stores shared food base, read-only for users'$$

CREATE TABLE `food_user` (
  `id` char(32) NOT NULL,
  `user_id` int(11) unsigned NOT NULL,
  `name` varchar(100) NOT NULL,
  `prots` decimal(6,2) unsigned NOT NULL,
  `fats` decimal(6,2) unsigned NOT NULL,
  `carbs` decimal(6,2) unsigned NOT NULL,
  `value` decimal(6,2) unsigned NOT NULL,
  `from_table` bit(1) NOT NULL,
  `deleted` bit(1) NOT NULL,
  `last_modified` datetime NOT NULL,
  `hash` char(32) NOT NULL,
  `version` int(11) unsigned NOT NULL,
  PRIMARY KEY (`id`,`user_id`),
  KEY `FK_FOOD_USER_USER_idx` (`user_id`),
  CONSTRAINT `FK_FOOD_USER_USER` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='Stores personal food base'$$

CREATE TABLE `preferences` (
  `user_id` int(11) unsigned NOT NULL,
  `key` char(32) NOT NULL,
  `value` varchar(1024) COLLATE utf8_unicode_ci DEFAULT NULL,
  `version` int(11) unsigned NOT NULL,
  PRIMARY KEY (`user_id`,`key`),
  KEY `FK_PREFERENCES_USER_idx` (`user_id`),
  CONSTRAINT `FK_PREFERENCES_USER` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='Stores user preferences as versioned key-value pairs'$$

CREATE TABLE `log` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `time` datetime NOT NULL,
  `remote_user` varchar(255) DEFAULT NULL,
  `remote_address` varchar(255) DEFAULT NULL,
  `request_url` varchar(255) NOT NULL,
  `request_params` text,
  `error_message` varchar(255) DEFAULT NULL,
  `stacktrace` text,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='Stores errors info'$$
