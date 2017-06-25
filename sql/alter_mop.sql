ALTER TABLE mop.session
ADD INDEX (groupId);

ALTER TABLE mop.action
ADD INDEX(detailId);
