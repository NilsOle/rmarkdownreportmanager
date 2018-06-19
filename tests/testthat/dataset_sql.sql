CREATE TABLE `datasets` (
  `dataset_id` integer PRIMARY KEY AUTOINCREMENT,
  `name` varchar(255)  DEFAULT NULL,
  `filename` varchar(255)  DEFAULT NULL,
  `rows` integer(12)  DEFAULT NULL,
  `cols` integer(12)  DEFAULT NULL,
  `created` integer(12)  DEFAULT NULL,
  `last_updated` integer(12)  DEFAULT NULL,
  `file_md5sum` varchar(255)  DEFAULT NULL
);

CREATE TABLE `reportpackages` (
  `reportpackage_id` integer PRIMARY KEY AUTOINCREMENT,
  `name` varchar(255)  DEFAULT NULL,
  `version` varchar(255)  DEFAULT NULL,
  `filename` varchar(255)  DEFAULT NULL,
  `created` integer(12)  DEFAULT NULL,
  `last_updated` integer(12)  DEFAULT NULL,
  `file_md5sum` varchar(255)  DEFAULT NULL
);

CREATE TABLE `reports` (
  `report_id` integer PRIMARY KEY AUTOINCREMENT,
  `created` integer(12)  DEFAULT NULL,
  `lastrun_started` integer(12)  DEFAULT NULL,
  `lastrun_finished` integer(12)  DEFAULT NULL,
  `lastrun_status` integer(12)  DEFAULT NULL,
  `lastrun_messagelog` text  DEFAULT NULL,
  `lastrun_filesgenerated` integer(12)  DEFAULT NULL,
  `config` text  DEFAULT NULL,
  `dataset_md5sum` varchar(255)  DEFAULT NULL,
  `reportpackage_md5sum` varchar(255)  DEFAULT NULL,
  `config_md5sum` varchar(255)  DEFAULT NULL
);
